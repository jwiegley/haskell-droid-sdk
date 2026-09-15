{-# LANGUAGE OverloadedStrings #-}

module OwnedIpcSpec (ownedIpcTests, runOwnedIpcPeer) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (AsyncCancelled (..), asyncThreadId, cancel, wait, waitCatch, withAsync)
import Control.Exception (Exception, IOException, SomeException, bracket, finally, fromException, onException, throwIO, try)
import Control.Monad (forM_, forever, replicateM_, unless, void, when, (>=>))
import Data.Aeson (Object, Value (..), eitherDecodeStrict', encode, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text qualified as Text
import DroidSpec (assertReaped, runDroidPeer)
import Factory.Droid qualified as Droid
import Factory.Droid.Transport (processTransport)
import Factory.Droid.Transport.Process
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (ThreadStatus (ThreadBlocked, ThreadRunning), threadStatus)
import Network.Socket qualified as Socket
import ProcessSpec (bounded)
import System.Directory (canonicalizePath, findExecutable, getCurrentDirectory, getTemporaryDirectory, removePathForcibly)
import System.Environment (getEnv, getExecutablePath, lookupEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO (BufferMode (BlockBuffering), Handle, IOMode (ReadWriteMode, WriteMode), hClose, hFlush, hIsClosed, hSetBinaryMode, hSetBuffering, stderr, stdin, stdout, withBinaryFile)
import System.IO.Error (isFullError)
import System.Posix.Directory qualified as PosixDirectory
import System.Posix.IO (fdToHandle)
import System.Posix.IO qualified as Posix
import System.Posix.Process (getProcessID)
import System.Posix.Temp (mkdtemp)
import System.Posix.Types (Fd (..))
import System.Process (CmdSpec (ShellCommand), CreateProcess (child_group, child_user, close_fds, cmdspec, create_group, cwd, delegate_ctlc, env, new_session, std_err), StdStream (..), readCreateProcessWithExitCode)
import System.Process.Internals (ignoreSigPipe)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase, (@?=))
import Text.Read (readMaybe)
import ValidatorSpec (workerIdentifier)

ownedIpcTests :: TestTree
ownedIpcTests =
  testGroup
    "Owned process IPC"
    [ testCase "stdio and IPC preserve exact objects with independent duplex I/O" $
        bounded $
          withScratch $ \root -> do
            executable <- getExecutablePath
            canonical <- canonicalizePath root
            identifier <- withJsonLinesProcessIpc 4096 0 defaultIpcProcessOptions ((proc executable ["--owned-ipc-peer", "echo", "", "Ω"]) {cwd = Just root, env = Just [("IPC_FIXTURE_EMPTY", "")], std_err = CreatePipe}) $ \stdio ipc errors -> do
              withAsync (receiveObject ipc) $ \pending -> do
                sendObject stdio payload
                sendObject ipc payload
                received <- receiveObject stdio
                KeyMap.lookup "echo" received @?= Just (Object payload)
                KeyMap.lookup "arguments" received @?= Just (toJSON (["", "Ω"] :: [String]))
                KeyMap.lookup "empty" received @?= Just (String "")
                KeyMap.lookup "cwd" received @?= Just (String (Text.pack canonical))
                wait pending >>= (@?= payload)
                sink <- maybe (assertFailure "Missing captured stderr") pure errors
                BS.hGetLine sink >>= (@?= "stderr-fixture")
                peerIdentifier received
            assertReaped identifier,
      testCase "normal sessions borrow stdio while IPC remains independently usable" $ bounded $ do
        executable <- getExecutablePath
        identifier <- withJsonLinesProcessIpc 65536 0 defaultIpcProcessOptions ((proc executable ["--owned-ipc-peer", "session"]) {std_err = NoStream}) $ \stdio ipc _ ->
          Droid.withDroidSessionOn (Droid.defaultDroidSessionOptions ".") (processTransport stdio) $ \session -> do
            sendObject ipc payload
            receiveObject ipc >>= (@?= payload)
            pure (Droid.droidSessionId session)
        assertReaped identifier,
      testCase "invalid configuration and missing executables never publish channels" $ bounded $ do
        entered <- newIORef False
        executable <- getExecutablePath
        let config = (proc executable ["--owned-ipc-peer", "wait"]) {std_err = NoStream}
            callback _ _ _ = writeIORef entered True
            run settings command = try @JsonLinesError (withJsonLinesProcessIpc 4096 0 settings command callback)
        run (defaultIpcProcessOptions {ipcProcessStartupTimeoutMicros = 0}) config >>= (@?= Left IpcStartupTimedOut)
        run (defaultIpcProcessOptions {ipcProcessStartupTimeoutMicros = -1}) config >>= (@?= Left InvalidIpcProcessOptions)
        run (defaultIpcProcessOptions {ipcProcessLauncher = "bad\0path"}) config >>= (@?= Left InvalidIpcProcessOptions)
        run (defaultIpcProcessOptions {ipcProcessLauncher = "/nonexistent/launcher"}) config >>= (@?= Left IpcLauncherUnavailable)
        run defaultIpcProcessOptions (proc "/nonexistent/engine" []) >>= (@?= Left ProcessStartFailure)
        forM_
          [ proc "" [],
            proc executable ["bad\0argument"],
            config {cmdspec = ShellCommand "must-not-run"},
            config {cwd = Just "bad\0directory"},
            config {env = Just [("", "value")]},
            config {env = Just [("BAD=KEY", "value")]},
            config {env = Just [("KEY", "bad\0value")]},
            config {delegate_ctlc = True},
            config {child_user = Just 0},
            config {child_group = Just 0}
          ]
          (run defaultIpcProcessOptions >=> (@?= Left InvalidIpcProcessOptions))
        try @JsonLinesError (withJsonLinesProcessIpc 0 0 defaultIpcProcessOptions config callback) >>= (@?= Left InvalidFrameLimit)
        show (defaultIpcProcessOptions {ipcProcessLauncher = "private-launcher"}) @?= "IpcProcessOptions <redacted>"
        readIORef entered >>= (@?= False),
      testCase "process-group and session flags reach the owned child" $ bounded $ do
        executable <- testPeer
        forM_ [(True, False), (False, True)] $ \(group, session) -> do
          identifier <- withJsonLinesProcessIpc 4096 0 defaultIpcProcessOptions ((proc executable ["--identity"]) {create_group = group, new_session = session, std_err = NoStream}) $ \stdio _ _ -> do
            received <- receiveObject stdio
            KeyMap.lookup "group" received @?= KeyMap.lookup "pid" received
            unless group (KeyMap.lookup "session" received @?= KeyMap.lookup "pid" received)
            peerIdentifier received
          assertReaped identifier,
      testCase "callback exceptions preserve identity and reap the owned PID" $ bounded $ do
        executable <- getExecutablePath
        ready <- newEmptyMVar
        result <- try @FixtureFailure $
          withJsonLinesProcessIpc 4096 0 defaultIpcProcessOptions ((proc executable ["--owned-ipc-peer", "wait"]) {std_err = NoStream}) $ \stdio _ _ -> do
            identifier <- receiveObject stdio >>= peerIdentifier
            putMVar ready identifier
            throwIO FixtureFailure :: IO ()
        result @?= Left FixtureFailure
        takeMVar ready >>= assertReaped,
      testCase "caller cancellation preserves AsyncCancelled and reaps the owned PID" $ bounded $ do
        executable <- getExecutablePath
        ready <- newEmptyMVar
        held <- newEmptyMVar
        withAsync (withJsonLinesProcessIpc 4096 0 defaultIpcProcessOptions ((proc executable ["--owned-ipc-peer", "wait"]) {std_err = NoStream}) (\stdio _ _ -> receiveObject stdio >>= peerIdentifier >>= putMVar ready >> takeMVar held)) $ \pending -> do
          identifier <- takeMVar ready
          cancel pending
          waitCatch pending >>= assertCancelled
          assertReaped identifier,
      testCase "startup timeout reaps an actually executed unresponsive launcher" $
        bounded $
          withScratch $ \root -> do
            launcher <- testPeer
            let pidFile = root </> "pid"
                settings = defaultIpcProcessOptions {ipcProcessLauncher = launcher, ipcProcessStartupTimeoutMicros = 500000}
                config = (proc "/unreached/engine" []) {env = Just [("DROID_IPC_PID_FILE", pidFile)], std_err = NoStream}
            started <- getMonotonicTimeNSec
            try @JsonLinesError (withJsonLinesProcessIpc 4096 0 settings config (\_ _ _ -> assertFailure "Startup unexpectedly succeeded" :: IO ())) >>= (@?= Left IpcStartupTimedOut)
            finished <- getMonotonicTimeNSec
            assertBool "Startup cleanup exceeded two seconds" (finished - started < 2000000000)
            workerIdentifier pidFile >>= assertReaped,
      testCase "startup cancellation keeps identity and releases the unpublished child" $
        bounded $
          withScratch $ \root -> do
            launcher <- testPeer
            let pidFile = root </> "pid"
                settings = defaultIpcProcessOptions {ipcProcessLauncher = launcher}
                config = (proc "/unreached/engine" []) {env = Just [("DROID_IPC_PID_FILE", pidFile)], std_err = NoStream}
            withAsync (withJsonLinesProcessIpc 4096 0 settings config (\_ _ _ -> assertFailure "Startup unexpectedly succeeded" :: IO ())) $ \pending -> do
              identifier <- workerIdentifier pidFile
              cancel pending
              waitCatch pending >>= assertCancelled
              assertReaped identifier,
      testCase "borrowed stderr stays usable after the process scope" $
        bounded $
          withScratch $ \root -> do
            executable <- getExecutablePath
            let path = root </> "stderr"
            withBinaryFile path WriteMode $ \sink -> do
              withJsonLinesProcessIpc 4096 0 defaultIpcProcessOptions ((proc executable ["--owned-ipc-peer", "echo"]) {std_err = UseHandle sink}) $ \stdio ipc captured -> do
                case captured of Nothing -> pure (); Just _ -> assertFailure "Borrowed stderr became owned"
                sendObject stdio payload
                sendObject ipc payload
                void (receiveObject stdio)
                receiveObject ipc >>= (@?= payload)
              BS.hPut sink "caller\n"
              hFlush sink
            BS.readFile path >>= (@?= "stderr-fixture\ncaller\n"),
      testCase "borrowed writable duplex stderr remains usable in both directions" $
        bounded $
          withStderrHandles duplexHandles $ \reader writer -> do
            executable <- testPeer
            withJsonLinesProcessIpc 4096 0 defaultIpcProcessOptions ((proc executable ["--stderr-state"]) {std_err = UseHandle writer}) $ \stdio _ _ -> checkStderrTransfer reader stdio
            hIsClosed writer >>= (@?= False)
            BS.hPut writer "caller" >> hFlush writer
            BS.hGet reader 6 >>= (@?= "caller")
            BS.hPut reader "reply" >> hFlush reader
            BS.hGet writer 5 >>= (@?= "reply"),
      testCase "borrowed nonblocking stderr matches ordinary process flags and drains completely" $
        bounded $
          forM_ [False, True] $ \ipc ->
            withStderrHandles pipeHandles $ \reader writer -> do
              executable <- testPeer
              let config = (proc executable ["--stderr-state"]) {std_err = UseHandle writer}
              if ipc
                then withJsonLinesProcessIpc 4096 0 defaultIpcProcessOptions config (\stdio _ _ -> checkStderrTransfer reader stdio)
                else withJsonLinesProcessStderr 4096 0 config (\stdio _ -> checkStderrTransfer reader stdio)
              hIsClosed writer >>= (@?= False)
              BS.hPut writer "tail" >> hFlush writer
              BS.hGet reader 4 >>= (@?= "tail"),
      testCase "borrowed stdout receives child stderr without being replaced" $ bounded $ do
        executable <- getExecutablePath
        readCreateProcessWithExitCode (proc executable ["--owned-ipc-peer", "stderr-alias"]) "" >>= (@?= (ExitSuccess, "stderr-fixture\ncaller\n", "")),
      testCase "startup timeout includes flushing a full borrowed stderr pipe" $
        bounded $
          withFullStderr $ \sink -> do
            executable <- getExecutablePath
            let settings = defaultIpcProcessOptions {ipcProcessStartupTimeoutMicros = 50000}
                config = (proc executable ["--owned-ipc-peer", "wait"]) {std_err = UseHandle sink}
            result <- timeout 1000000 (try @JsonLinesError (withJsonLinesProcessIpc 4096 0 settings config (\_ _ _ -> assertFailure "Blocked stderr allowed startup" :: IO ())))
            result @?= Just (Left IpcStartupTimedOut)
            hIsClosed sink >>= (@?= False),
      testCase "startup deadline does not limit the caller's process scope" $ bounded $ do
        executable <- getExecutablePath
        let settings = defaultIpcProcessOptions {ipcProcessStartupTimeoutMicros = 500000}
        identifier <- withJsonLinesProcessIpc 4096 0 settings ((proc executable ["--owned-ipc-peer", "wait"]) {std_err = NoStream}) $ \stdio _ _ -> do
          value <- receiveObject stdio >>= peerIdentifier
          threadDelay 600000
          pure value
        assertReaped identifier,
      testCase "blocked IPC reads and writes cancel without losing identity or reaping" $ bounded $ do
        executable <- getExecutablePath
        forM_ [False, True] $ \writing -> do
          ready <- newEmptyMVar
          let blockedIO ipc = if writing then sendObject ipc (KeyMap.singleton "blocked" (String (Text.replicate (4 * 1024 * 1024) "x"))) else void (receiveObject ipc)
          withAsync
            ( withJsonLinesProcessIpc (8 * 1024 * 1024) 0 defaultIpcProcessOptions ((proc executable ["--owned-ipc-peer", "wait"]) {std_err = NoStream}) $ \stdio ipc _ -> do
                receiveObject stdio >>= peerIdentifier >>= putMVar ready
                blockedIO ipc
                assertFailure "IPC I/O unexpectedly completed"
            )
            ( \pending -> do
                identifier <- takeMVar ready
                let awaitBlocked =
                      threadStatus (asyncThreadId pending) >>= \case
                        ThreadBlocked _ -> pure ()
                        ThreadRunning -> threadDelay 1000 >> awaitBlocked
                        _ -> wait pending >> assertFailure "IPC worker ended before blocking"
                awaitBlocked
                started <- getMonotonicTimeNSec
                cancel pending
                waitCatch pending >>= assertCancelled
                assertReaped identifier
                finished <- getMonotonicTimeNSec
                assertBool "Blocked IPC cleanup exceeded two seconds" (finished - started < 2000000000)
            ),
      testCase "concurrent unrelated execs cannot inherit owned IPC descriptors" $ bounded $ do
        executable <- getExecutablePath
        readCreateProcessWithExitCode ((proc executable ["--owned-ipc-peer", "descriptor-privacy"]) {close_fds = True}) "" >>= (@?= (ExitSuccess, "", ""))
    ]

payload :: Object
payload = KeyMap.fromList ["number" .= Number 900719925474099312345, "decimal" .= Number 0.100000000000000000001, "false" .= Bool False, "null" .= Null, "empty" .= String "", "nul" .= String "a\0b", "extension" .= object []]

data FixtureFailure = FixtureFailure deriving stock (Eq, Show)

instance Exception FixtureFailure

assertCancelled :: Either SomeException a -> IO ()
assertCancelled = \case
  Left cause -> fromException cause @?= Just AsyncCancelled
  Right _ -> assertFailure "Cancellation unexpectedly succeeded"

peerIdentifier :: Object -> IO Text.Text
peerIdentifier fields = case KeyMap.lookup "pid" fields of
  Just (String value) -> pure ("fixture-" <> value)
  _ -> assertFailure "Missing native child PID"

withScratch :: (FilePath -> IO a) -> IO a
withScratch = bracket (getTemporaryDirectory >>= \root -> mkdtemp (root </> "droid-owned-ipc-test-")) removePathForcibly

testPeer :: IO FilePath
testPeer = findExecutable "droid-ipc-test-peer" >>= maybe (assertFailure "Missing Cabal native IPC test peer") pure

checkStderrTransfer :: Handle -> JsonLinesProcess -> IO ()
checkStderrTransfer reader stdio = do
  received <- receiveObject stdio
  KeyMap.lookup "nonblocking" received @?= Just (Bool False)
  BS.hGet reader (64 * 4096) >>= (@?= BS.replicate (64 * 4096) 'x')

withStderrHandles :: IO (Handle, Handle) -> (Handle -> Handle -> IO a) -> IO a
withStderrHandles acquire action = bracket acquire (\(reader, writer) -> hClose writer `finally` hClose reader) (uncurry action)

duplexHandles :: IO (Handle, Handle)
duplexHandles = do
  (readerSocket, writerSocket) <- Socket.socketPair Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
  reader <- Socket.socketToHandle readerSocket ReadWriteMode `onException` (Socket.close readerSocket `finally` Socket.close writerSocket)
  writer <- Socket.socketToHandle writerSocket ReadWriteMode `onException` (hClose reader `finally` Socket.close writerSocket)
  pure (reader, writer)

pipeHandles :: IO (Handle, Handle)
pipeHandles = do
  (readerFd, writerFd) <- Posix.createPipe
  let closeBoth = Posix.closeFd readerFd `finally` Posix.closeFd writerFd
  forM_ [readerFd, writerFd] (\fd -> Posix.setFdOption fd Posix.CloseOnExec True) `onException` closeBoth
  reader <- fdToHandle readerFd `onException` closeBoth
  writer <- fdToHandle writerFd `onException` (hClose reader `finally` Posix.closeFd writerFd)
  Posix.setFdOption writerFd Posix.NonBlockingRead True `onException` (hClose writer `finally` hClose reader)
  pure (reader, writer)

withFullStderr :: (Handle -> IO ()) -> IO ()
withFullStderr action = bracket acquire release $ \(reader, handle) -> do
  hSetBuffering handle (BlockBuffering (Just 8192))
  BS.hPut handle "x"
  action handle
  allocaBytes 4096 (\buffer -> void (Posix.fdReadBuf reader buffer 4096))
  hFlush handle
  where
    acquire = do
      (reader, writer) <- Posix.createPipe
      let closeBoth = Posix.closeFd reader `finally` Posix.closeFd writer
      flip onException closeBoth $ do
        forM_ [reader, writer] $ \fd -> Posix.setFdOption fd Posix.CloseOnExec True
        Posix.setFdOption writer Posix.NonBlockingRead True
        let fill bytes = BS.useAsCStringLen bytes $ \(buffer, size) ->
              let loop =
                    try @IOException (Posix.fdWriteBuf writer (castPtr buffer) (fromIntegral size)) >>= \case
                      Right _ -> loop
                      Left cause -> unless (isFullError cause) (throwIO cause)
               in loop
        mapM_ fill [BS.replicate 4096 'x', "x"]
        handle <- fdToHandle writer
        pure (reader, handle)
    release (reader, handle) = Posix.closeFd reader `finally` ignoreSigPipe (hClose handle)

runOwnedIpcPeer :: [String] -> IO ()
runOwnedIpcPeer ["descriptor-privacy"] = bounded $ do
  executable <- getExecutablePath
  inspector <- testPeer
  -- GHC may create inheritable control pipes; establish the baseline before IPC.
  bracket (PosixDirectory.openDirStream "/dev/fd") PosixDirectory.closeDirStream $ \directory ->
    let protect = do
          name <- PosixDirectory.readDirStream directory
          unless (null name) $ do
            forM_ (readMaybe name) $ \fd -> when (fd > 2) (Posix.setFdOption (Fd fd) Posix.CloseOnExec True)
            protect
     in protect
  let inspect stage = readCreateProcessWithExitCode ((proc inspector ["--check-fds"]) {close_fds = False}) "" >>= assertEqual stage (ExitSuccess, "", "")
  inspect "Before IPC descriptor creation"
  withAsync (replicateM_ 100 (inspect "Concurrent with IPC creation")) $ \spawns -> do
    replicateM_ 20 $ do
      identifier <- withJsonLinesProcessIpc 4096 0 defaultIpcProcessOptions ((proc executable ["--owned-ipc-peer", "wait"]) {std_err = NoStream}) $ \stdio _ _ -> receiveObject stdio >>= peerIdentifier
      assertReaped identifier
    wait spawns
  inspect "After IPC descriptor cleanup"
runOwnedIpcPeer ["stderr-alias"] = do
  executable <- getExecutablePath
  identifier <- withJsonLinesProcessIpc 4096 0 defaultIpcProcessOptions ((proc executable ["--owned-ipc-peer", "echo"]) {std_err = UseHandle stdout}) $ \stdio ipc _ -> do
    sendObject stdio payload
    sendObject ipc payload
    received <- receiveObject stdio
    receiveObject ipc >>= (@?= payload)
    peerIdentifier received
  assertReaped identifier
  BS.hPut stdout "caller\n"
  hFlush stdout
runOwnedIpcPeer arguments = do
  forM_ [stdin, stdout] $ \handle -> hSetBinaryMode handle True
  getEnv "NODE_CHANNEL_FD" >>= (@?= "3")
  getEnv "NODE_CHANNEL_SERIALIZATION_MODE" >>= (@?= "json")
  case arguments of
    "echo" : supplied -> do
      ipc <- fdToHandle (Fd 3)
      stdioValue <- readValue stdin
      ipcValue <- readValue ipc
      pid <- getProcessID
      directory <- getCurrentDirectory
      empty <- lookupEnv "IPC_FIXTURE_EMPTY"
      BS.hPut stderr "stderr-fixture\n"
      hFlush stderr
      writeValue stdout (object ["echo" .= stdioValue, "pid" .= show pid, "cwd" .= directory, "arguments" .= supplied, "empty" .= empty])
      writeValue ipc ipcValue
    ["wait"] -> do
      pid <- getProcessID
      writeValue stdout (object ["pid" .= show pid])
      forever (threadDelay 1000000)
    ["session"] -> do
      ipc <- fdToHandle (Fd 3)
      withAsync (readValue ipc >>= writeValue ipc) (const runDroidPeer)
    _ -> assertFailure "Invalid IPC peer mode"
  where
    readValue :: Handle -> IO Value
    readValue handle = BS.hGetLine handle >>= either (const (assertFailure "Invalid peer JSON")) pure . eitherDecodeStrict'
    writeValue :: Handle -> Value -> IO ()
    writeValue handle value = LBS.hPut handle (encode value <> "\n") >> hFlush handle
