{-# LANGUAGE OverloadedStrings #-}

module ProcessSpec (processTests, runProcessPeer, withPeer, bounded) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (Async, AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM (atomically, retry)
import Control.Exception (Exception, IOException, SomeException, bracket, finally, fromException, throwIO, try)
import Control.Monad (forM_, forever, unless, void, when)
import Data.Aeson (FromJSON, Object, Result (..), Value (..), eitherDecodeStrict', encode, fromJSON, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Factory.Droid.Protocol qualified as Protocol
import Factory.Droid.Schema.Notifications (AgentTurnCompleted (..), AgentTurnCompletionReason (..), AssistantTextDelta (..))
import Factory.Droid.Schema.RPC (BaseNotification (..), BaseResponseSuccess (..), CommandAck (..), WithEnvelope (..))
import Factory.Droid.Schema.Usage (TokenUsage (..))
import Factory.Droid.Transport.Process
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (findExecutable)
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (IOMode (WriteMode), hClose, hFlush, hSetBinaryMode, openBinaryTempFile, stderr, stdin, stdout, withBinaryFile)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (deviceID, fileID, getFdStatus, getFileStatus, removeLink)
import System.Posix.IO qualified as Posix
import System.Posix.Process (exitImmediately, getProcessID)
import System.Posix.Signals (Handler (Catch, Ignore), installHandler, nullSignal, sigKILL, sigTERM, signalProcess)
import System.Posix.Types (Fd (..))
import System.Process (CmdSpec (RawCommand), CreateProcess (close_fds, cmdspec, env, std_err), StdStream (CreatePipe, UseHandle))
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

processTests :: TestTree
processTests =
  testGroup
    "Local JSONL process exchange"
    [ testCase "Droid modes select exact shell-free commands without starting a process" $
        forM_ [(Acp, ["exec", "--output-format", "acp"]), (StreamJsonRpc, ["exec", "--input-format", "stream-jsonrpc", "--output-format", "stream-jsonrpc"])] $ \(mode, expected) ->
          case cmdspec (droidProcess "/fixture/droid with spaces" mode) of
            RawCommand executable arguments -> do
              executable @?= "/fixture/droid with spaces"
              arguments @?= expected
            _ -> assertFailure "Droid command used a shell",
      testGroup
        "Droid descriptor defaults"
        [ testCase name $ bounded $ withMarker $ \path ->
            bracket (Posix.openFd path Posix.ReadOnly (Posix.defaultFileFlags {Posix.cloexec = False})) Posix.closeFd $ \fd@(Fd number) -> do
              expected <- getFileStatus path
              actual <- getFdStatus fd
              (deviceID actual, fileID actual) @?= (deviceID expected, fileID expected)
              executable <- findExecutable "droid-ipc-test-peer" >>= maybe (assertFailure "Missing native process test peer") pure
              let configured = (build executable) {env = Just [("DROID_FD_FIXTURE", show number), ("DROID_FD_FIXTURE_PATH", path)]}
              response <- withJsonLinesProcess 4096 50000 configured receiveObject
              pid <- maybe (assertFailure "Missing fixture PID") decode (KeyMap.lookup "pid" response)
              assertReaped pid
              KeyMap.lookup "inherited" response @?= Just (Bool inherited)
        | (name, build, inherited) <-
            [ ("default ACP closes the fixture", (`droidProcess` Acp), False),
              ("default stream-jsonrpc closes the fixture", (`droidProcess` StreamJsonRpc), False),
              ("prepared ACP closes the fixture", \executable -> prepareDroidProcess executable Acp defaultDroidLaunchOptions [] id, False),
              ("prepared replacement arguments still close the fixture", \executable -> prepareDroidProcess executable StreamJsonRpc (defaultDroidLaunchOptions {launchArguments = Just ["--fixture-fd"]}) [] id, False),
              ("explicit low-level inheritance is preserved", \executable -> (proc executable ["--fixture-fd"]) {close_fds = False}, True),
              ("explicit low-level isolation is preserved", \executable -> (proc executable ["--fixture-fd"]) {close_fds = True}, False),
              ("explicit override of a Droid command is preserved", \executable -> (droidProcess executable Acp) {close_fds = False}, True)
            ]
        ],
      testCase "launch environment merges ordinary values before sanitization and trusted values after" $ do
        let options = defaultDroidLaunchOptions {launchEnvironment = Map.fromList [("value", "override"), ("drop", "ordinary")], launchTrustedEnvironment = Map.fromList [("drop", "trusted"), ("empty", "")]}
            sanitize = Map.map (<> "-clean") . Map.delete "drop"
            configured = prepareDroidProcess "fixture" Acp options [("value", "inherited"), ("keep", "inherited"), ("Case", "upper"), ("case", "lower")] sanitize
        env configured @?= Just (Map.toList (Map.fromList [("value", "override-clean"), ("keep", "inherited-clean"), ("Case", "upper-clean"), ("case", "lower-clean"), ("drop", "trusted"), ("empty", "")])),
      testCase "launch prefix replacement and extra arguments preserve empty arrays and strings" $
        forM_ [(Nothing, ["exec", "--output-format", "acp"]), (Just [], []), (Just ["custom", ""], ["custom", ""])] $ \(replacement, arguments) -> do
          let options = defaultDroidLaunchOptions {launchPrefixArguments = ["prefix with spaces"], launchArguments = replacement, launchExtraArguments = ["extra", ""]}
              configured = prepareDroidProcess "fixture" Acp options [] id
          case cmdspec configured of
            RawCommand executable actual -> do
              executable @?= "fixture"
              actual @?= ["prefix with spaces"] <> arguments <> ["extra", ""]
            _ -> assertFailure "Launch configuration used a shell",
      testCase "launch option displays do not expose arguments or environment values" $ do
        let options = defaultDroidLaunchOptions {launchExtraArguments = ["private argument"], launchEnvironment = Map.singleton "TOKEN" "private value", launchTrustedEnvironment = Map.singleton "TOKEN" "trusted value"}
        show options @?= "DroidLaunchOptions <redacted>",
      testCase "ACP mode preserves raw requests replies notifications and errors without Factory envelopes" $ bounded $ withAcpPeer 4096 $ \channel ->
        forM_
          [ KeyMap.fromList ["jsonrpc" .= String "2.0", "id" .= Number 9007199254740993, "method" .= String "fixture/echo", "params" .= ([] :: [Value])],
            KeyMap.fromList ["jsonrpc" .= String "2.0", "id" .= Number 1, "result" .= KeyMap.fromList ["empty" .= String "", "flag" .= False, "future" .= Null]],
            KeyMap.fromList ["jsonrpc" .= String "2.0", "method" .= String "fixture/notice", "params" .= String "سلام\n😀"],
            KeyMap.fromList ["jsonrpc" .= String "2.0", "id" .= String "request", "error" .= KeyMap.fromList ["code" .= Number (-32000), "message" .= String "fixture", "data" .= Null]]
          ]
          (\value -> sendObject channel value >> receiveObject channel >>= (@?= value)),
      testCase "ACP framing failures retain shared bounds and payload-free errors" $
        bounded $
          forM_ [("malformed", InvalidJsonObject), ("eof", EndOfStream), ("partial", TruncatedFrame), ("oversized", FrameTooLarge)] $ \(mode, expected) -> do
            result <- try @JsonLinesError $ withAcpPeer 128 $ \channel -> sendObject channel (KeyMap.singleton "fixtureControl" (String mode)) >> receiveObject channel
            result @?= Left expected,
      testCase "ACP callback exceptions retain identity and reap the child" $ bounded $ do
        result <- try @TestAbort (withAcpPeer 4096 (\_ -> throwIO TestAbort) :: IO ())
        result @?= Left TestAbort,
      testCase "ACP cancellation kills and reaps a SIGTERM-resistant child" $ bounded $ do
        ready <- newEmptyMVar
        withAsync (withAcpPeer 4096 $ \channel -> stallAcp channel >> putMVar ready () >> receiveObject channel) $ \worker -> do
          takeMVar ready
          cancelAndCheck worker,
      testCase "ACP normal scope exit kills and reaps a SIGTERM-resistant child" $
        bounded $
          withAcpPeer 4096 stallAcp,
      testCase "current-version request, acknowledgement, deltas and completion" $ bounded $ withPeer "exchange" 4096 $ \channel -> do
        sendObject channel request
        ackMessage <- decode . Object =<< receiveObject channel
        envelopeProtocolVersion ackMessage @?= Just "1.205.0"
        let ackFrame = envelopeBody ackMessage
        successResponseId ackFrame @?= "request-1"
        successResponseError ackFrame @?= Nothing
        traverse decode (successResponseResult ackFrame) >>= (@?= Just (CommandAck mempty))
        first <- notification channel :: IO AssistantTextDelta
        second <- notification channel :: IO AssistantTextDelta
        assistantDeltaText first <> assistantDeltaText second @?= "Hello سلام\n😀"
        completed <- notification channel :: IO AgentTurnCompleted
        turnCompletionReason completed @?= TurnCompleted
        usageOutputTokens (turnTokenUsage completed) @?= 2,
      testCase "explicit stderr pipes drain alongside stdout and retain owned cleanup" $ bounded $ do
        executable <- getExecutablePath
        let configured = (prepareDroidProcess executable StreamJsonRpc (defaultDroidLaunchOptions {launchArguments = Just ["--jsonl-peer", "stderr"]}) [("JSONL_FIXTURE", "native")] id) {std_err = CreatePipe}
        (pid, diagnostic) <- withJsonLinesProcessStderr 4096 50000 configured $ \channel errors -> do
          pipe <- maybe (assertFailure "Missing owned stderr pipe") pure errors
          withAsync (BS.hGetContents pipe) $ \draining -> do
            ready <- receiveObject channel
            pid <- maybe (assertFailure "Missing peer PID") decode (KeyMap.lookup "pid" ready)
            sendObject channel request
            receiveObject channel >>= (@?= request)
            bytes <- wait draining
            pure (pid, bytes)
        assertReaped pid
        diagnostic @?= BS.replicate (256 * 1024) 120,
      testCase "explicit stderr handles remain borrowed after process scope exit" $ bounded $ withMarker $ \path -> do
        executable <- getExecutablePath
        withBinaryFile path WriteMode $ \sink -> do
          let configured = (prepareDroidProcess executable StreamJsonRpc (defaultDroidLaunchOptions {launchArguments = Just ["--jsonl-peer", "stderr"]}) [("JSONL_FIXTURE", "native")] id) {std_err = UseHandle sink}
          pid <- withJsonLinesProcessStderr 4096 50000 configured $ \channel errors -> do
            errors @?= Nothing
            ready <- receiveObject channel
            pid <- maybe (assertFailure "Missing peer PID") decode (KeyMap.lookup "pid" ready)
            sendObject channel request
            receiveObject channel >>= (@?= request)
            pure pid
          assertReaped pid
          BS.hPut sink "tail"
          hFlush sink
        BS.readFile path >>= (@?= BS.replicate (256 * 1024) 120 <> "tail"),
      testCase "stderr cannot block an exchange and CRLF is accepted" $ bounded $ withPeer "stderr" 4096 $ \channel -> do
        sendObject channel request
        received <- receiveObject channel
        received @?= request,
      testCase "malformed/non-object frames fail without revealing contents" $
        bounded $
          forM_ ["malformed", "array", "utf8"] $ \mode -> do
            result <- try @JsonLinesError (withPeer mode 4096 receiveObject)
            result @?= Left InvalidJsonObject
            show result @?= "Left InvalidJsonObject",
      testCase "incoming and outgoing frames enforce the explicit byte limit" $ bounded $ do
        incoming <- try @JsonLinesError (withPeer "oversized" 128 receiveObject)
        incoming @?= Left FrameTooLarge
        let value = KeyMap.singleton "text" (String (Text.replicate 40 "😀"))
            size = fromIntegral (BL.length (encode value))
        withPeer "echo" size $ \channel -> do
          sendObject channel value
          receiveObject channel >>= (@?= value)
        outgoing <- try @JsonLinesError (withPeer "echo" (size - 1) $ \channel -> sendObject channel value)
        outgoing @?= Left FrameTooLarge,
      testCase "EOF and incomplete final frames are distinct failures" $ bounded $ do
        eof <- try @JsonLinesError (withPeer "eof" 4096 receiveObject)
        eof @?= Left (ProcessExited ExitSuccess)
        partial <- try @JsonLinesError (withPeer "partial" 4096 receiveObject)
        partial @?= Left TruncatedFrame,
      testGroup
        "process exit diagnostics"
        [ testCase mode $ bounded $ do
            result <- try @JsonLinesError (withPeer mode 4096 receiveObject)
            result @?= Left (ProcessExited status)
        | (mode, status) <- [("eof", ExitSuccess), ("exit37", ExitFailure 37), ("signal", ExitFailure (negate (fromIntegral sigTERM)))]
        ],
      testCase "buffered frames precede exit diagnostics and later sends retain the exit" $ bounded $ withPeer "buffered-exit37" 4096 $ \channel -> do
        receiveObject channel >>= (@?= KeyMap.singleton "retained" (Bool True))
        try @JsonLinesError (receiveObject channel) >>= (@?= Left (ProcessExited (ExitFailure 37)))
        try @JsonLinesError (sendObject channel request) >>= (@?= Left (ProcessExited (ExitFailure 37))),
      testCase "stdout EOF can precede the actual exit status" $ bounded $ do
        result <- try @JsonLinesError (withPeer "delayed-exit37" 4096 receiveObject)
        result @?= Left (ProcessExited (ExitFailure 37)),
      testCase "a live child closing stdout keeps EOF rather than a cleanup-induced signal" $ bounded $ do
        result <- try @JsonLinesError (withPeer "close-stdout" 4096 receiveObject)
        result @?= Left EndOfStream,
      testGroup
        "framing errors precede process diagnostics"
        [ testCase mode $ bounded $ do
            result <- try @JsonLinesError (withPeer mode 4096 receiveObject)
            result @?= Left expected
        | (mode, expected) <- [("partial-exit37", TruncatedFrame), ("invalid-exit37", InvalidJsonObject)]
        ],
      testGroup
        "write failures distinguish a live peer from an exiting peer"
        [ testCase mode $ bounded $ do
            result <- try @JsonLinesError (withPeer mode 4096 (`sendObject` request))
            result @?= Left expected
        | (mode, expected) <- [("closed-stdin", ProcessWriteFailure), ("closed-stdin-exit37", ProcessExited (ExitFailure 37))]
        ],
      testCase "RPC failure cause retains the structured process exit" $ bounded $ withPeer "exit37" 4096 $ \channel ->
        Protocol.withRpcChannel (sendObject channel) (receiveObject channel) $ \rpc -> do
          cause <- atomically (Protocol.rpcChannelFailureCause rpc >>= maybe retry pure)
          fromException cause @?= Just (ProcessExited (ExitFailure 37)),
      testCase "SIGTERM-resistant children are killed and reaped on return" $
        bounded $
          withPeer "stubborn" 4096 (\_ -> pure ()),
      testCase "nonpositive grace escalates immediately, including natural-exit races" $
        bounded $
          forM_ [-1, 0] $ \grace ->
            forM_ ["eof", "stubborn"] $ \mode ->
              withPeerSettings mode 4096 grace Nothing (\_ -> pure ()),
      testCase "cooperative shutdown receives its grace period" $ bounded $ withMarker $ \marker -> do
        withPeerSettings "graceful" 4096 1000000 (Just marker) (\_ -> pure ())
        BS.readFile marker >>= (@?= "graceful"),
      testCase "callback exceptions retain their identity and reap the child" $
        bounded $
          forM_ ["idle", "stubborn", "exit37"] $ \mode -> do
            result <- try @TestAbort (withPeer mode 4096 (\_ -> throwIO TestAbort) :: IO ())
            result @?= Left TestAbort,
      testCase "cancellation while receiving reaps the child" $
        bounded $
          forM_ ["idle", "stubborn", "close-stdout"] $ \mode -> do
            ready <- newEmptyMVar
            withAsync (withPeer mode 4096 $ \channel -> putMVar ready () >> receiveObject channel) $ \worker -> do
              takeMVar ready
              when (mode == "close-stdout") (threadDelay 50000)
              cancelAndCheck worker,
      testCase "cancellation while writing to a full pipe reaps the child" $
        bounded $
          forM_ [128, 4 * 1024 * 1024] $ \size -> do
            ready <- newEmptyMVar
            let payload = KeyMap.singleton "text" (String (Text.replicate size "x"))
                exchange = withPeer "stubborn-write" (8 * 1024 * 1024) $ \channel ->
                  withAsync (forever (sendObject channel payload)) $ \sender -> do
                    receiveObject channel >>= (@?= mempty)
                    putMVar ready ()
                    wait sender
            withAsync exchange $ \worker -> do
              takeMVar ready
              -- The peer has stopped reading; allow both small and large writes to fill the pipe.
              threadDelay 50000
              cancelAndCheck worker,
      testCase "cancellation during the grace period still kills and reaps" $ bounded $ withMarker $ \marker ->
        withAsync (withPeerSettings "stop-notice" 4096 (4 * 1000000) (Just marker) (\_ -> pure ())) $ \worker -> do
          let awaitNotice = do
                notice <- BS.readFile marker
                unless (notice == "stopping") (threadDelay 10000 >> awaitNotice)
          awaitNotice
          promptCleanup (cancelAndCheck worker),
      testCase "the fixture watchdog cannot be credited as SDK cleanup" $ bounded $ do
        result <- try @PeerWatchdogFired $ withPeer "watchdog" 4096 $ \channel ->
          try @JsonLinesError (receiveObject channel) >>= (@?= Left (ProcessExited (ExitFailure (-9))))
        result @?= Left PeerWatchdogFired,
      testCase "cleanup timing excludes time spent in the caller callback" $
        bounded $
          withPeer "idle" 4096 (\_ -> threadDelay 2100000),
      testCase "invalid limits and startup failures remain payload-free" $ do
        result <- try @JsonLinesError (withJsonLinesProcess 0 50000 (proc "/not/a/real/executable" []) (\_ -> pure ()))
        result @?= Left InvalidFrameLimit
        failedStart <- try @JsonLinesError (withJsonLinesProcess 128 50000 (proc "" []) (\_ -> pure ()))
        failedStart @?= Left ProcessStartFailure
    ]

-- This is a deadlock backstop, not an SDK response-time assertion. A test can
-- launch several peers or use the validator's thirty-second job deadline.
-- Operation-specific deadlines and cleanup bounds are asserted separately.
bounded :: IO () -> IO ()
bounded action = timeout (60 * 1000000) action >>= maybe (assertFailure "Offline exchange timed out") pure

promptCleanup :: IO () -> IO ()
promptCleanup action = do
  start <- getMonotonicTimeNSec
  action
  end <- getMonotonicTimeNSec
  assertBool "Cleanup waited for the peer watchdog instead of escalating" (end - start < 2 * 1000000000)

cancelAndCheck :: Async a -> IO ()
cancelAndCheck worker = do
  cancel worker
  result <- waitCatch worker
  case result of
    Left err -> fromException err @?= Just AsyncCancelled
    Right _ -> assertFailure "Cancelled exchange unexpectedly succeeded"

withMarker :: (FilePath -> IO a) -> IO a
withMarker action =
  bracket (openBinaryTempFile "/tmp" "droid-jsonl-shutdown") (\(path, handle) -> hClose handle `finally` removeLink path) $ \(path, handle) -> hClose handle >> action path

withPeer :: String -> Int -> (JsonLinesProcess -> IO a) -> IO a
withPeer mode limit = withPeerSettings mode limit 50000 Nothing

withPeerSettings :: String -> Int -> Int -> Maybe FilePath -> (JsonLinesProcess -> IO a) -> IO a
withPeerSettings mode = withPeerCommand (\executable -> proc executable ["--jsonl-peer", mode])

withAcpPeer :: Int -> (JsonLinesProcess -> IO a) -> IO a
withAcpPeer limit = withPeerCommand (`droidProcess` Acp) limit 50000 Nothing

stallAcp :: JsonLinesProcess -> IO ()
stallAcp channel = do
  sendObject channel (KeyMap.singleton "fixtureControl" (String "stubborn"))
  receiveObject channel >>= (@?= KeyMap.singleton "ready" (Bool True))

withPeerCommand :: (FilePath -> CreateProcess) -> Int -> Int -> Maybe FilePath -> (JsonLinesProcess -> IO a) -> IO a
withPeerCommand command limit grace marker action = withMarker $ \watchdogMarker -> do
  executable <- getExecutablePath
  pidRef <- newIORef Nothing
  cleanupStart <- newIORef Nothing
  let environment = [("JSONL_FIXTURE", "native"), ("JSONL_WATCHDOG", watchdogMarker)] <> maybe [] (\path -> [("JSONL_MARKER", path)]) marker
      config = (command executable) {env = Just environment}
  result <- try @SomeException $ withJsonLinesProcess limit grace config $ \channel -> do
    ready <- receiveObject channel
    pid <- maybe (assertFailure "Missing peer PID") decode (KeyMap.lookup "pid" ready) :: IO Integer
    writeIORef pidRef (Just pid)
    -- The old outer timer charged loader/startup and caller work to cleanup.
    action channel `finally` (getMonotonicTimeNSec >>= writeIORef cleanupStart . Just)
  finished <- getMonotonicTimeNSec
  readIORef pidRef >>= mapM_ assertReaped
  fired <- BS.readFile watchdogMarker
  unless (BS.null fired) (throwIO PeerWatchdogFired)
  readIORef cleanupStart >>= mapM_ (\started -> assertBool "SDK cleanup exceeded two seconds after the callback ended" (finished - started < 2 * 1000000000))
  either throwIO pure result

assertReaped :: Integer -> IO ()
assertReaped pid = do
  result <- try @IOException (signalProcess nullSignal (fromIntegral pid))
  case result of
    Left err | isDoesNotExistError err -> pure ()
    Left _ -> assertFailure "Could not inspect peer cleanup"
    Right () -> assertFailure "Peer was not terminated and reaped"

notification :: (FromJSON a) => JsonLinesProcess -> IO a
notification channel = do
  event <- envelopeBody <$> (decode . Object =<< receiveObject channel)
  baseNotificationMethod event @?= "droid.session_notification"
  maybe (assertFailure "Missing notification payload") decode (baseNotificationParams event)

decode :: (FromJSON a) => Value -> IO a
decode value = case fromJSON value of
  Error _ -> assertFailure "Fixture JSON failed typed decoding"
  Success result -> pure result

data TestAbort = TestAbort deriving stock (Eq, Show)

instance Exception TestAbort

data PeerWatchdogFired = PeerWatchdogFired deriving stock (Eq, Show)

instance Exception PeerWatchdogFired

-- Test-only control handshake identifies the exact child for cleanup checks.
-- Later exchanges use Factory or raw JSONL fixture frames; no Factory process runs.
runProcessPeer :: String -> IO ()
runProcessPeer mode = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  expectedEnv <- lookupEnv "JSONL_FIXTURE"
  unless (expectedEnv == Just "native") (throwIO TestAbort)
  pid <- getProcessID
  let watchdog =
        (lookupEnv "JSONL_WATCHDOG" >>= mapM_ (`BS.writeFile` "fired"))
          `finally` signalProcess sigKILL pid
  when (mode `elem` ["stubborn", "stubborn-write"]) $ void (installHandler sigTERM Ignore Nothing)
  when (mode `elem` ["graceful", "stop-notice"]) $ do
    marker <- lookupEnv "JSONL_MARKER" >>= maybe (throwIO TestAbort) pure
    let onTerminate =
          if mode == "graceful"
            then threadDelay 50000 >> BS.writeFile marker "graceful" >> exitImmediately ExitSuccess
            else BS.writeFile marker "stopping"
    void (installHandler sigTERM (Catch onTerminate) Nothing)
  when (mode `elem` ["stubborn", "stubborn-write", "graceful", "stop-notice"]) $
    -- Backstop makes a broken parent cleanup fail without leaving a child behind.
    void (forkIO (threadDelay (5 * 1000000) >> watchdog))
  when (mode `elem` ["closed-stdin", "closed-stdin-exit37"]) (hClose stdin)
  writeLine (KeyMap.singleton "pid" (toJSON (fromIntegral pid :: Integer)))
  case mode of
    "watchdog" -> watchdog >> forever (threadDelay 1000000)
    "acp" -> forever $ do
      received <- readObject
      case KeyMap.lookup "fixtureControl" received of
        Just (String "malformed") -> BS.hPut stdout "[]\n" >> hFlush stdout
        Just (String "eof") -> hClose stdout >> forever (threadDelay 1000000)
        Just (String "partial") -> BS.hPut stdout "{}" >> hClose stdout >> forever (threadDelay 1000000)
        Just (String "oversized") -> BS.hPut stdout (BS.replicate 1024 120) >> hFlush stdout
        Just (String "stubborn") -> do
          void (installHandler sigTERM Ignore Nothing)
          void (forkIO (threadDelay (5 * 1000000) >> watchdog))
          writeLine (KeyMap.singleton "ready" (Bool True))
          forever (threadDelay 1000000)
        _ -> writeLine received
    "exchange" -> do
      received <- readObject
      unless (received == request) (throwIO TestAbort)
      writeLine acknowledgement
      let first = frame (toJSON (AssistantTextDelta "message" 0 "Hello " mempty))
          second = frame (toJSON (AssistantTextDelta "message" 0 "سلام\n😀" mempty))
      forM_ (BS.unpack (BL.toStrict (encode first) <> "\n")) $ \byte -> BS.hPut stdout (BS.singleton byte) >> hFlush stdout
      BL.hPutStr stdout (encode second <> "\n" <> encode completion <> "\n")
      hFlush stdout
    "stderr" -> do
      BS.hPut stderr (BS.replicate (256 * 1024) 120)
      hFlush stderr
      received <- readObject
      BL.hPutStr stdout (encode received <> "\r\n")
      hFlush stdout
    "rpc" -> do
      first <- readObject
      second <- readObject
      identifiers <- mapM (maybe (throwIO TestAbort) pure . KeyMap.lookup "id") [first, second]
      writeLine (envelope ["type" .= String "notification", "method" .= String "notice", "params" .= String "queued before replies"])
      writeLine (envelope ["type" .= String "request", "id" .= String "server-id", "method" .= String "server.question", "params" .= String "fixture question"])
      forM_ (reverse identifiers) $ \identifier ->
        writeLine (envelope ["type" .= String "response", "id" .= identifier, "result" .= identifier])
      answer <- readObject
      unless (answer == envelope ["type" .= String "response", "id" .= String "server-id", "result" .= String "fixture answer"]) (throwIO TestAbort)
      writeLine (envelope ["type" .= String "notification", "method" .= String "answered"])
    "client-bindings" -> do
      models <- readObject
      unless (models == envelope ["type" .= String "request", "id" .= String "models", "method" .= String "droid.list_models", "params" .= KeyMap.singleton "includeDisabled" (Bool False)]) (throwIO TestAbort)
      writeLine (envelope ["type" .= String "response", "id" .= String "models", "result" .= KeyMap.singleton "models" (toJSON ([] :: [Value]))])
      changed <- readObject
      unless (changed == envelope ["type" .= String "request", "id" .= String "directory", "method" .= String "droid.change_working_directory", "params" .= KeyMap.singleton "workingDirectory" (String "/fixture/current")]) (throwIO TestAbort)
      writeLine (envelope ["type" .= String "response", "id" .= String "directory", "result" .= KeyMap.singleton "resolvedPath" (String "/fixture/resolved")])
    "echo" -> readObject >>= writeLine
    "malformed" -> BS.hPut stdout "{\"fixture-secret\": broken}\n" >> hFlush stdout
    "array" -> BS.hPut stdout "[]\n" >> hFlush stdout
    "utf8" -> BS.hPut stdout ("{\"text\":\"" <> BS.singleton 255 <> "\"}\n") >> hFlush stdout
    "oversized" -> BS.hPut stdout (BS.replicate 1024 120) >> hFlush stdout
    "partial" -> BS.hPut stdout "{}" >> hFlush stdout
    "eof" -> pure ()
    "exit37" -> exitImmediately (ExitFailure 37)
    "signal" -> signalProcess sigTERM pid >> forever (threadDelay 1000000)
    "buffered-exit37" -> writeLine (KeyMap.singleton "retained" (Bool True)) >> exitImmediately (ExitFailure 37)
    "delayed-exit37" -> hClose stdout >> threadDelay 50000 >> exitImmediately (ExitFailure 37)
    "close-stdout" -> hClose stdout >> forever (threadDelay 1000000)
    "partial-exit37" -> BS.hPut stdout "{}" >> hFlush stdout >> exitImmediately (ExitFailure 37)
    "invalid-exit37" -> BS.hPut stdout "[]\n" >> hFlush stdout >> exitImmediately (ExitFailure 37)
    "closed-stdin" -> forever (threadDelay 1000000)
    "closed-stdin-exit37" -> threadDelay 50000 >> exitImmediately (ExitFailure 37)
    "idle" -> forever (threadDelay 1000000)
    "stubborn" -> forever (threadDelay 1000000)
    "stubborn-write" -> do
      void (BS.hGetSome stdin 1)
      writeLine (mempty :: Object)
      forever (threadDelay 1000000)
    "graceful" -> forever (threadDelay 1000000)
    "stop-notice" -> forever (threadDelay 1000000)
    _ -> throwIO TestAbort
  where
    readObject = BSC.hGetLine stdin >>= either (const (throwIO TestAbort)) pure . eitherDecodeStrict'
    writeLine value = BL.hPutStr stdout (encode value <> "\n") >> hFlush stdout

request, acknowledgement, completion :: Object
request = envelope ["type" .= String "request", "id" .= String "request-1", "method" .= String "droid.add_user_message", "params" .= KeyMap.singleton "text" (String "hello\nسلام")]
acknowledgement = envelope ["type" .= String "response", "id" .= String "request-1", "result" .= CommandAck mempty]
completion = frame (Object (KeyMap.fromList ["type" .= String "agent_turn_completed", "reason" .= String "completed", "tokenUsage" .= TokenUsage 1 2 0 0 0 Nothing mempty]))

frame :: Value -> Object
frame payload = envelope ["type" .= String "notification", "method" .= String "droid.session_notification", "params" .= payload]

envelope :: [(Key, Value)] -> Object
envelope fields = KeyMap.fromList (["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.205.0"] <> fields)
