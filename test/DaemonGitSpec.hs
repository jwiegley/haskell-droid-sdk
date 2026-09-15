{-# LANGUAGE OverloadedStrings #-}

module DaemonGitSpec (gitTests) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (AsyncCancelled (..), cancel, wait, waitCatch, withAsync)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Control.Exception (catch, fromException, try)
import Control.Monad (forM_, forever)
import Data.Aeson (FromJSON, Object, Result (..), ToJSON, Value (..), eitherDecode, encode, fromJSON, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Factory.Droid.Client qualified as Client
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Protocol (RpcChannel, RpcChannelError (..), RpcResultError (..), withRpcChannel)
import Factory.Droid.Schema.Daemon.Git
import Factory.Droid.Schema.RPC (JsonRpcError (..), JsonRpcErrorCode (RpcInvalidParams), WithEnvelope (..))
import Factory.Droid.Transport.WebSocket qualified as WebSocket
import Network.WebSockets qualified as WS
import ProcessSpec (bounded)
import SchemaTest (rejects)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebSocketSpec (withPeer)

gitTests :: TestTree
gitTests =
  testGroup
    "Daemon Git inspection"
    [ testCase "branch/checkout/divergence variants preserve nullability and missing-only defaults" $ do
        branches <- decodeValue @ListGitBranchesResult (object ["isGitRepository" .= False, "branches" .= ([] :: [Value]), "currentBranch" .= Null])
        gitCurrentBranch branches @?= Nothing
        rejects (Proxy @ListGitBranchesResult) (object ["isGitRepository" .= False, "branches" .= ([] :: [Value])])
        roundTrip (Proxy @CheckoutBranchResult) (object ["status" .= String "checked_out", "currentBranch" .= String "", "pullFailure" .= String ""])
        checkout <- decodeValue @CheckoutBranchResult checkoutWire
        toJSON checkout @?= checkoutNormalized
        rejects (Proxy @CheckoutBranchResult) (insertField "untrackedFiles" Null checkoutWire)
        rejects (Proxy @CheckoutBranchResult) (object ["status" .= String "future"])
        forM_ [object ["status" .= String "tracked", "ahead" .= Number 0, "behind" .= Number 2], object ["status" .= String "no_remote"], object ["status" .= String "unavailable"]] $ roundTrip (Proxy @GitBranchDivergence)
        forM_ [Number (-1), Number 0.5, Null] $ \bad -> rejects (Proxy @GitBranchDivergence) (object ["status" .= String "tracked", "ahead" .= bad, "behind" .= Number 0]),
      testCase "PR lookup batches are bounded and fallback is limited to the reason field" $ bounded $ do
        let item = PullRequestLookup (PullRequestSubject "session" mempty) (Just False) Nothing mempty
        fmap (length . pullRequestLookupItems) (mkPullRequestLookupBatch []) @?= Just 0
        fmap (length . pullRequestLookupItems) (mkPullRequestLookupBatch (replicate 20 item)) @?= Just 20
        mkPullRequestLookupBatch (replicate 21 item) @?= Nothing
        mkPullRequestLookupBatch (repeat item) @?= Nothing
        rejects (Proxy @ResolvePullRequestStatusesParams) (object ["lookups" .= replicate 21 (toJSON item)])
        forM_ [Null, String "future", Number 1, object []] $ \bad -> do
          decoded <- decodeValue @PullRequestLookup (object ["subject" .= subjectWire, "invalidate" .= False, "reason" .= bad])
          pullRequestLookupReason decoded @?= Nothing
          toJSON decoded @?= object ["subject" .= subjectWire, "invalidate" .= False]
        rejects (Proxy @PullRequestLookup) (object ["subject" .= object ["kind" .= String "future", "sessionId" .= String "session"]])
        rejects (Proxy @PullRequestLookup) (object ["subject" .= subjectWire, "invalidate" .= Null]),
      testCase "PR status reasons fall back while nullable result fields retain all three states" $ do
        forM_ [object ["state" .= String "unavailable"], object ["state" .= String "unavailable", "reason" .= Null], object ["state" .= String "unavailable", "reason" .= String "future"]] $ \wire -> do
          status <- decodeValue @PullRequestStatus wire
          toJSON status @?= object ["state" .= String "unavailable", "reason" .= String "unknown"]
        roundTrip (Proxy @PullRequestStatus) (object ["state" .= String "open", "url" .= String "", "title" .= String ""])
        roundTrip (Proxy @PullRequestStatus) (object ["state" .= String "none"])
        rejects (Proxy @PullRequestStatus) (object ["state" .= String "open", "url" .= Null])
        result <- decodeValue @PullRequestStatusResult statusWire
        pullRequestRemoteUrl result @?= Just Nothing
        absent <- decodeValue @PullRequestStatusResult (deleteField "remoteUrl" statusWire)
        pullRequestRemoteUrl absent @?= Nothing
        roundTrip (Proxy @PullRequestStatusResult) (insertField "remoteUrl" (String "") statusWire)
        rejects (Proxy @PullRequestStatusResult) (deleteField "branch" statusWire)
        rejects (Proxy @PullRequestStatusResult) (deleteField "status" statusWire)
        rejects (Proxy @PullRequestStatusResult) (insertField "provider" (String "future") statusWire),
      testCase "bare diff replies normalize to success with defaults, but explicit null stays invalid" $ do
        legacy <- decodeValue @GitDiffResult legacyDiffWire
        toJSON legacy @?= normalizedDiffWire
        modern <- decodeValue @GitDiffResult (object ["success" .= True, "data" .= legacyDiffWire])
        modern @?= legacy
        forM_ defaultKeys $ \key -> rejects (Proxy @GitDiffResult) (insertField key Null legacyDiffWire)
        rejects (Proxy @GitDiffResult) (insertField "success" Null legacyDiffWire)
        rejects (Proxy @GitDiffResult) (deleteField "remoteUrl" legacyDiffWire)
        rejects (Proxy @GitDiffResult) (object ["success" .= True, "data" .= Null])
        forM_ [object ["success" .= False, "unavailableMessage" .= String ""], object ["success" .= False, "unavailableReason" .= Null, "unavailableMessage" .= String ""], object ["success" .= False, "unavailableReason" .= String "future", "unavailableMessage" .= String ""]] $ \wire -> do
          unavailable <- decodeValue @GitDiffResult wire
          toJSON unavailable @?= object ["success" .= False, "unavailableReason" .= String "unknown", "unavailableMessage" .= String ""]
        rejects (Proxy @GitDiffResult) (object ["success" .= False, "unavailableMessage" .= Null]),
      testCase "populated diff sections retain numbers, metadata and distinct field groups" $ do
        let populated = Object (KeyMap.union (KeyMap.fromList ["committedDiff" .= String "committed", "committedFiles" .= [fileWire], "committedTotalAdditions" .= Number 1.5, "committedTotalDeletions" .= Number 2.5, "localDiff" .= String "local", "localFiles" .= ([] :: [Value]), "localTotalAdditions" .= Number 3.5, "localTotalDeletions" .= Number 4.5, "unstagedDiff" .= String "unstaged", "unstagedFiles" .= [fileWire], "unstagedTotalAdditions" .= Number 5.5, "unstagedTotalDeletions" .= Number 6.5, "pushableCommitCount" .= Number 0, "isDetachedHead" .= False, "defaultBranch" .= String "", "baseBranchExistsOnRemote" .= False, "pullRequestStatus" .= object ["state" .= String "none"], "pullRequestProvider" .= String "gitlab"]) (objectFields legacyDiffWire))
        roundTrip (Proxy @GitDiffData) populated
        value <- decodeValue @GitDiffData populated
        sectionDiff (committedGitDiff value) @?= "committed"
        sectionDiff (localGitDiff value) @?= "local"
        sectionDiff (unstagedGitDiff value) @?= "unstaged"
        show value @?= "GitDiffData <redacted>",
      testCase "all five native Git operations preserve requests and normalized peer results" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        query <- queryParams
        withGitPeer Normal trace ready $ \target -> Daemon.withConnection (options target) $ \connection ->
          forM_ (zip wireCases (highCalls connection query)) $ \((_, _, _, expected), request) -> request >>= (@?= expected)
        readIORef trace >>= (@?= map String ("daemon.authenticate" : map (\(method, _, _, _) -> method) wireCases)) . map (field "method"),
      testCase "Git RPC errors retain structured data and malformed replies do not retire the connection" $
        bounded $
          forM_ [Rejected, Malformed] $ \mode -> do
            trace <- newIORef []
            ready <- newEmptyMVar
            query <- queryParams
            withGitPeer mode trace ready $ \target -> Daemon.withConnection (options target) $ \connection ->
              forM_ (highCalls connection query) $ \request -> do
                result <- try @RpcResultError request
                case result of
                  Left (RpcRemoteFailure err) | mode == Rejected -> do
                    rpcErrorCode err @?= RpcInvalidParams
                    rpcErrorData err @?= Just conflictWire
                  Left RpcInvalidResult | mode == Malformed -> pure ()
                  _ -> assertFailure "Unexpected Git result",
      testCase "cancelled checkout waits preserve async identity without an invented rollback" $ bounded $ do
        trace <- newIORef []
        ready <- newEmptyMVar
        withGitPeer HeldCheckout trace ready $ \target -> Daemon.withConnection (options target) $ \connection -> do
          withAsync (Daemon.checkoutGitBranch connection checkoutParams) $ \pending -> do
            takeMVar ready
            cancel pending
            waitCatch pending >>= \case
              Left cause -> fromException cause @?= Just AsyncCancelled
              Right _ -> assertFailure "Cancelled checkout returned"
          Daemon.listGitBranches connection "/remote" >>= (@?= branchWire) . toJSON
        readIORef trace >>= (@?= map String ["daemon.authenticate", "daemon.checkout_git_branch", "daemon.list_git_branches"]) . map (field "method"),
      testCase "all low-level Git bindings preserve exact bodies and caller deadlines" $ bounded $ do
        incoming <- newTQueueIO
        outgoing <- newTQueueIO
        query <- queryParams
        withRpcChannel (atomically . writeTQueue outgoing) (atomically (readTQueue incoming)) $ \channel -> do
          let configured = Client.CallOptions "git-rpc" (WithEnvelope Nothing Nothing mempty) (Just 1000000)
          forM_ (zip wireCases (lowCalls channel configured query)) $ \((method, params, result, expected), request) -> withAsync request $ \pending -> do
            frame <- atomically (readTQueue outgoing)
            field "method" frame @?= String method
            field "params" frame @?= params
            atomically (writeTQueue incoming (response frame result))
            wait pending >>= (@?= expected)
          forM_ (lowCalls channel (configured {Client.callTimeoutMicros = Just 0}) query) $ \request -> try @RpcChannelError request >>= (@?= Left RpcRequestTimedOut)
          atomically (tryReadTQueue outgoing) >>= (@?= Nothing)
    ]

checkoutParams :: CheckoutBranchParams
checkoutParams = CheckoutBranchParams "/remote" "feature" (Just False) (Just CheckoutStash) (KeyMap.singleton "future" (Bool False))

diffParams :: GitDiffParams
diffParams = GitDiffParams "session" (Just "") (Just False) (KeyMap.singleton "future" (Bool False))

queryParams :: IO ResolvePullRequestStatusesParams
queryParams = decodeValue queryWire

highCalls :: Daemon.DaemonConnection -> ResolvePullRequestStatusesParams -> [IO Value]
highCalls connection query = [toJSON <$> Daemon.listGitBranches connection "/remote", toJSON <$> Daemon.checkoutGitBranch connection checkoutParams, toJSON <$> Daemon.getGitBranchDivergence connection (GitBranchParams "/remote" "feature" mempty), toJSON <$> Daemon.getGitDiff connection diffParams, toJSON <$> Daemon.resolvePullRequestStatuses connection query]

lowCalls :: RpcChannel -> Client.CallOptions -> ResolvePullRequestStatusesParams -> [IO Value]
lowCalls channel configured query = [toJSON <$> Client.listDaemonGitBranches channel configured (GitDirectoryParams "/remote" mempty), toJSON <$> Client.checkoutDaemonGitBranch channel configured checkoutParams, toJSON <$> Client.getDaemonGitBranchDivergence channel configured (GitBranchParams "/remote" "feature" mempty), toJSON <$> Client.getDaemonGitDiff channel configured diffParams, toJSON <$> Client.resolveDaemonPullRequestStatuses channel configured query]

wireCases :: [(Text, Value, Value, Value)]
wireCases = [("daemon.list_git_branches", object ["cwd" .= String "/remote"], branchWire, branchWire), ("daemon.checkout_git_branch", object ["cwd" .= String "/remote", "branch" .= String "feature", "create" .= False, "resolution" .= String "stash", "future" .= False], checkoutWire, checkoutNormalized), ("daemon.get_git_branch_divergence", object ["cwd" .= String "/remote", "branch" .= String "feature"], divergenceWire, divergenceWire), ("daemon.get_git_diff", object ["sessionId" .= String "session", "baseBranch" .= String "", "statsOnly" .= False, "future" .= False], legacyDiffWire, normalizedDiffWire), ("daemon.resolve_pull_request_statuses", queryWire, statusesWire, statusesWire)]

data Mode = Normal | Rejected | Malformed | HeldCheckout deriving stock (Eq, Show)

options :: WebSocket.WebSocketTarget -> Daemon.DaemonOptions
options target = (Daemon.defaultDaemonOptions target (Daemon.DaemonApiKey "OFFLINE_ONLY") "UNUSED_CWD") {Daemon.daemonTransport = WebSocket.defaultWebSocketOptions {WebSocket.webSocketTLS = Nothing}}

withGitPeer :: Mode -> IORef [Object] -> MVar () -> (WebSocket.WebSocketTarget -> IO a) -> IO a
withGitPeer mode trace ready = withPeer (\_ connection -> serve connection `catch` \(_ :: WS.ConnectionException) -> pure ())
  where
    readFrame connection = do
      frame <- WS.receiveData connection >>= either (const (assertFailure "Invalid RPC")) pure . eitherDecode
      field "factoryProtocolVersion" frame @?= String "1.201.1"
      modifyIORef' trace (<> [frame])
      pure frame
    serve connection = do
      auth <- readFrame connection
      field "method" auth @?= String "daemon.authenticate"
      field "params" auth @?= object ["apiKey" .= String "OFFLINE_ONLY", "caller" .= String "haskell-sdk"]
      reply connection auth (object ["userId" .= String "user", "orgId" .= String "org"])
      forever $ do
        request <- readFrame connection
        case [(method, params, result) | (method, params, result, _) <- wireCases, field "method" request == String method] of
          [(method, params, result)] -> do
            field "params" request @?= params
            if mode == HeldCheckout && method == "daemon.checkout_git_branch"
              then putMVar ready ()
              else case mode of
                Rejected -> sendFrame connection ["type" .= String "response", "id" .= field "id" request, "error" .= object ["code" .= RpcInvalidParams, "message" .= String "Git rejected", "data" .= conflictWire]]
                Malformed -> reply connection request Null
                _ -> reply connection request result
          _ -> assertFailure "Unexpected Git or implicit session request"
    reply connection request value = WS.sendTextData connection (encode (Object (response request value)))

response :: Object -> Value -> Object
response request result = KeyMap.fromList (envelope ["type" .= String "response", "id" .= field "id" request, "result" .= result])

sendFrame :: WS.Connection -> [Pair] -> IO ()
sendFrame connection = WS.sendTextData connection . encode . object . envelope

envelope :: [Pair] -> [Pair]
envelope fields = ["jsonrpc" .= String "2.0", "factoryApiVersion" .= String "1.0.0", "factoryProtocolVersion" .= String "1.201.1"] <> fields

field :: Key -> Object -> Value
field key = fromMaybe Null . KeyMap.lookup key

objectFields :: Value -> Object
objectFields (Object fields) = fields
objectFields _ = error "Expected object fixture"

insertField :: Key -> Value -> Value -> Value
insertField key value = Object . KeyMap.insert key value . objectFields

deleteField :: Key -> Value -> Value
deleteField key = Object . KeyMap.delete key . objectFields

decodeValue :: (FromJSON a) => Value -> IO a
decodeValue value = case fromJSON value of Success result -> pure result; Error err -> assertFailure err

roundTrip :: forall a. (FromJSON a, ToJSON a) => Proxy a -> Value -> IO ()
roundTrip _ value = decodeValue @a value >>= (@?= value) . toJSON

branchWire, checkoutWire, checkoutNormalized, divergenceWire, subjectWire, queryWire, statusWire, statusesWire, fileWire, legacyDiffWire, normalizedDiffWire, conflictWire :: Value
branchWire = object ["isGitRepository" .= True, "branches" .= [String "main", String "feature"], "currentBranch" .= String "feature"]
checkoutWire = object ["status" .= String "needs_resolution", "message" .= String "dirty", "changedFiles" .= Number 1, "additions" .= Number 2, "deletions" .= Number 3]
checkoutNormalized = insertField "untrackedFiles" (Number 0) checkoutWire
divergenceWire = object ["status" .= String "tracked", "ahead" .= Number 2, "behind" .= Number 1]
subjectWire = object ["kind" .= String "branch", "sessionId" .= String "session"]
queryWire = object ["lookups" .= [object ["subject" .= subjectWire, "invalidate" .= False, "reason" .= String "manual_refresh"]]]
statusWire = object ["subject" .= subjectWire, "branch" .= Null, "status" .= Null, "resolvedAt" .= Number 1.5, "staleAfterMs" .= Number 0, "provider" .= String "github", "remoteUrl" .= Null]
statusesWire = object ["statuses" .= [statusWire]]
fileWire = object ["path" .= String "file", "additions" .= Number 1.25, "deletions" .= Number (-0.5), "status" .= String "modified"]
legacyDiffWire = object ["diff" .= String "DIFF DATA", "branch" .= String "feature", "baseBranch" .= String "main", "files" .= [fileWire], "totalAdditions" .= Number 1.25, "totalDeletions" .= Number (-0.5), "remoteUrl" .= Null, "commits" .= [object ["hash" .= String "opaque-hash", "message" .= String "message"]]]
normalizedDiffWire = object ["success" .= True, "data" .= Object (KeyMap.union (KeyMap.fromList defaultFields) (objectFields legacyDiffWire))]
conflictWire = object ["reason" .= String "branch_occupied_by_managed_worktree", "worktree" .= object ["path" .= String "/remote/worktree", "repoRoot" .= String "/remote", "lifecycle" .= String "persistent"]]

defaultFields :: [Pair]
defaultFields = ["committedDiff" .= String "", "committedFiles" .= ([] :: [Value]), "committedTotalAdditions" .= Number 0, "committedTotalDeletions" .= Number 0, "localDiff" .= String "", "localFiles" .= ([] :: [Value]), "localTotalAdditions" .= Number 0, "localTotalDeletions" .= Number 0, "unstagedDiff" .= String "", "unstagedFiles" .= ([] :: [Value]), "unstagedTotalAdditions" .= Number 0, "unstagedTotalDeletions" .= Number 0]

defaultKeys :: [Key]
defaultKeys = map fst defaultFields
