# Droid from Haskell

A native SDK for the local Factory Droid CLI and existing daemons: prompts, typed streaming events, final results, follow-up turns and scoped cleanup.

**Live baseline:** the earlier local prompt path was verified against CLI 0.212.1: streamed `HELLO`, matching final text and child cleanup. Later API additions are covered by offline tests, not a new live run. See [verification evidence](docs/development.md#current-local-sdk-delivery).

## Requirements

- GHC 9.10.3 and Cabal; an optional Nix development shell is provided.
- For local sessions, a working, authenticated `droid` installation. The initial runtime target is CLI 0.212.1 / protocol 1.201.1.
- For daemon sessions, an existing endpoint and caller-supplied credentials; a local CLI installation is not required.
- Normal SDK use requires neither Python nor Node.js. Local launch reuses the CLI login; daemon authentication is explicit. Credentials do not belong in command arguments or logs.

## One prompt

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text.IO qualified as Text
import Factory.Droid

main :: IO ()
main = do
  result <- runDroid (defaultDroidOptions ".") "Say hello."
  Text.putStrLn (resultText result)
```

## Streaming and follow-ups

Use `streamDroid options prompt onText` for a single streamed turn. Keep a session open for follow-ups:

```haskell
withDroidSession (defaultDroidOptions ".") $ \session -> do
  _ <- sendPrompt session "Remember the word HELLO." Text.putStr
  Text.putStrLn ""
  result <- sendPrompt session "What word did I give you?" Text.putStr
  Text.putStrLn ""
  pure result
```

`DroidOptions` controls the executable, working directory, optional model, optional turn timeout in microseconds and positive `droidFrameLimitBytes`. The frame limit defaults to 10 MiB, applies to complete encoded JSON in both directions and excludes the newline. RPC envelopes, request identifiers and transport threads are managed internally. Prompts within a session are serialized; do not start a second prompt from that session's text or event callback.

To load a saved session, use its earlier `droidSessionId` or `resultSessionId`:

```haskell
withResumedDroidSession (defaultDroidOptions ".") "saved-session-id" $ \session ->
  sendPrompt session "Continue where we left off." Text.putStr
```

Loading retains the stored conversation, working directory and model. The options directory controls subprocess launch; setting `droidModel = Just ...` overrides the saved model after loading. A missing session raises a structured RPC error with `RpcEntityNotFound`; it never silently creates a replacement. Resume behavior is verified with native offline peers, not an additional live model run.

Completion is an explicit CLI turn-completed event, not an acknowledgement or idle notification. Callback failure (including a terminal-event callback), deadline expiry, asynchronous consumer cancellation or invalid protocol events invalidate the handle and request interruption. A matching terminal failure or interruption is settled data and does not itself invalidate the session. Leaving the scope releases subscriptions and reaps the child.

## Images and documents

Attachment constructors in `Factory.Droid.Input` produce validated, immutable input values before submission:

```haskell
import Factory.Droid.Input (documentFromFile, imageFromFile)

attachments :: IO DroidResult
attachments = do
  image <- imageFromFile "diagram.png"
  notes <- documentFromFile "notes.txt"
  let input = (droidInput "Describe the diagram using these notes.")
        { inputImages = [image], inputDocuments = [notes] }
      options = (defaultDroidOptions ".") { droidFrameLimitBytes = 20 * 1024 * 1024 }
  withDroidSession options $ \session -> sendDroidInput session input Text.putStr
```

Use `sendDroidInputEvents` for rich callbacks, or `sendDroidInputOutput` / `sendDroidInputOutputEvents` to combine attachments with structured output. Empty text is permitted; empty attachment arrays are omitted. The ordinary text helpers use the same path.

Images accept JPEG, PNG, GIF and WebP signatures with matching MIME types, up to 5 MiB decoded. Text documents are limited to 5 MiB of UTF-8; PDFs to 3 MiB decoded. `imageFromBytes` / `documentFromBytes`, `documentFromText` and the corresponding source constructors avoid file IO. Binary documents must be PDF. Validation checks signatures, not full image/PDF structure, and runs no extractor.

Source constructors preserve metadata and extensions without reading metadata paths. They use strict RFC 4648 Base64 decoding, including canonical padding bits; this deliberately rejects aliases accepted by Python's `b64decode(validate=True)`. File constructors follow symlinks to regular files, reject NUL paths and FIFOs, read within a byte budget and detect observed identity/size/timestamp changes. Reads are not transactional snapshots or filesystem-latency guarantees. Relative paths use the Haskell process's working directory; PDF file paths are retained as metadata.

Base64 and JSON increase payload size. Raise `droidFrameLimitBytes` explicitly when multiple valid attachments exceed the default ceiling. Oversized frames fail with existing channel-write semantics and make the session unusable; nothing is silently truncated. Attachment/input wrappers redact `Show`, but explicit source getters and metadata can contain private data.

## Complete and partial events

`sendDroidEvents` returns every terminal outcome, like `sendDroidTurn`, and supplies typed events instead of text chunks:

```haskell
import Factory.Droid.Schema.Notifications (AgentTurnCompleted(..), AssistantTextDelta(..))

withDroidSession (defaultDroidOptions ".") $ \session ->
  sendDroidEvents session AllEvents "Say hello." $ \event ->
    case event of
      TextDeltaEvent delta -> Text.putStr (assistantDeltaText delta)
      TurnCompletedEvent completion -> print (turnCompletionReason completion)
      _ -> pure ()
```

`CompleteMessages` delivers full messages, derived complete tool calls, tool results, hook events, retractions, errors and terminal completion. `AllEvents` additionally delivers partial text/thinking/tool input, status, usage and metadata. Unadapted inner notification objects remain available as `OtherNotificationEvent`; malformed adapted payloads fail instead of becoming raw events.

Both modes use the same reducer. `resultEvents` retains the complete-event arrival log, excluding terminal completion; it is not a deduplicated conversation or a history of every partial notification. `resultStructuredOutput` captures the latest reported JSON object, cleared by an explicit null or its message's retraction. Capture does not request structured output or validate a schema.

`resultText` uses the most recently observed assistant message's current text buffer; when empty, it concatenates surviving buffers in first-seen block order. Snapshots replace buffers, deltas extend them and retractions remove them. Text callbacks remain append-only: a block emits only extensions of its previously delivered prefix. If a correction diverges from that prefix, delivery for that block resumes only when the prefix is restored and extended. Corrections and retractions cannot undo earlier chunks; use message/retraction events and raw deltas for a replaceable display.

## Observing events outside turns

`onDroidSessionEvent session callback` subscribes to typed session notifications even when no prompt is running and returns an idempotent unsubscribe action. It does not consume or replace a turn stream:

```haskell
import Control.Exception (bracket)

observedTurn :: DroidSession -> IO DroidResult
observedTurn session =
  bracket (onDroidSessionEvent session print) id $ \_ ->
    sendPrompt session "Say hello." Text.putStr
```

Callbacks receive `Right event`, `Left DroidInvalidEvent` for malformed adapted payloads, or `Left DroidSessionUnusable` for channel failure. Session observers may receive wire-valid completion without a turn ID; correlated turn APIs still require their matching ID. Settings observations also feed [`getDroidSettings`](#settings-tools-and-skills); acknowledgements never synthesize settings.

These callbacks run on dispatcher intake, unlike turn text/event callbacks. Keep them brief. Ordinary queries are supported, but do not start a turn, replace/load a session, synchronize events or wait for later events from an observer callback. Ordinary callback exceptions are isolated; asynchronous exceptions retain dispatcher cleanup behavior.

Subscriptions stop admitting notifications while the handle is replacing, retired or unavailable, and do not follow successors. Unsubscribe is not a join: a callback already admitted may finish. Keep its resources alive until that work is finished; every subscription remains bounded by the original connection scope.

## Structured output

`rawDroidOutput schema` requests an object without local JSON Schema keyword validation. `jsonDroidOutput schema` additionally uses the selected Haskell `FromJSON` instance. Both require an explicit top-level `"type": "object"`; the schema is not derived from the Haskell type.

```haskell
import Control.Exception (throwIO)
import Data.Aeson (FromJSON(..), Value(..), object, withObject, (.:), (.=))
import Data.Aeson.KeyMap qualified as KeyMap

newtype Answer = Answer Int deriving Show
instance FromJSON Answer where
  parseJSON = withObject "Answer" $ \fields -> Answer <$> fields .: "answer"

structured :: DroidSession -> IO (DroidOutputResult Answer)
structured session = do
  let schema = KeyMap.fromList
        [ "type" .= String "object"
        , "properties" .= object ["answer" .= object ["type" .= String "integer"]]
        , "required" .= [String "answer"]
        ]
  output <- either throwIO pure (jsonDroidOutput schema)
  sendDroidOutput session output "Return an object with answer equal to 42." (\_ -> pure ())
```

`sendDroidOutputEvents session output mode prompt callback` provides the same adaptation with rich callbacks. `outputTurnResult` preserves the CLI's terminal outcome; `outputValue` is `Right value`, `Left DroidOutputMissing` or `Left (DroidOutputInvalid diagnostic)`. Check both: successful decoding does not imply successful turn completion, and a completed turn may still lack valid output.

Notification data takes precedence. When absent/null, adaptation tries the entire final text as a JSON object; it does not strip Markdown fences or extract JSON from prose. The returned turn result retains that raw object. Local adaptation follows the settled turn, outside its deadline, and validation failure leaves the session usable. The caller is responsible for keeping the wire schema and `FromJSON` contract consistent. Explicit diagnostics and decoded values may contain private data.

## Terminal outcomes and interruption

`sendDroidTurn` returns every explicit terminal outcome instead of requiring success:

```haskell
import Factory.Droid.Schema.Notifications (AgentTurnCompleted(..))

withDroidSession (defaultDroidOptions ".") $ \session -> do
  result <- sendDroidTurn session "Say hello." Text.putStr
  pure (turnCompletionReason (resultCompletion result), turnTokenUsage (resultCompletion result))
```

`DroidResult` contains `resultCompletion` (typed reason, usage and optional duration/cumulative data) and ordered `resultErrors`, alongside the session ID, final text and retained events/output. `sendPrompt`, `runDroid` and `streamDroid` remain success-checking helpers: completed/spec-handoff outcomes return normally; other settled outcomes raise `DroidTurnFailed` carrying an `AgentTurnCompletionReason`. The settled session remains available for another turn.

Call `interruptDroidSession session` from an application task or a text/event callback to request interruption. Idle sessions are a no-op. Its acknowledgement does not prove the turn stopped; await the turn result and inspect its completion reason. Interruption waits for the current prompt's submission acknowledgement, and a pending interrupt fences the next prompt so a delayed request cannot cancel a later turn. An uncertain sent interrupt invalidates the handle; cancelling before it can be sent does not.

Each prompt has a fresh UUID message ID. Completion must echo that turn ID; foreign completions are ignored, and a missing ID is a protocol failure for this correlated-turn API. These checks are validated offline against the selected runtime contract; no new live model call is claimed.

The default session APIs reject permission requests and cancel user questions. Explicit handlers can supply decisions as described below. This is not a sandbox: the CLI's own tool policy still applies. Optional unsupported SDK-language attribution remains unset rather than identifying the Haskell caller as Python or TypeScript.

`DroidError` covers stream/session failures; `DroidReplacementError` retains successor-attachment and rollback causes. Existing `RpcChannelError`, `RpcResultError` and `JsonLinesError` identify protocol and process failures. `DroidOptions`, `DroidEvent`, `DroidResult`, `DroidOutput`, `DroidOutputResult`, `DroidOutputError`, `DroidError`, `DroidReplacementError`, `DroidRewindOptions`, `DroidSessionStatus` and `RpcResultError` redact payloads in `Show`. Schema records, including replacement-result metadata, and explicit fields/JSON may contain sensitive data; do not log them indiscriminately.

## Permission and question handlers

Use `withDroidSessionHandlers` or `withResumedDroidSessionHandlers` to supply connection-scoped callbacks. This rejecting example enables application handling without granting permission:

```haskell
import Factory.Droid.Interaction (cancelDroidQuestions)
import Factory.Droid.Schema.Interaction (cancelPermissionResult)

handledRun :: IO DroidResult
handledRun = do
  let handlers = defaultDroidHandlers
        { onDroidPermission = Just (\_ -> pure cancelPermissionResult)
        , onDroidQuestion = Just (\_ -> pure cancelDroidQuestions)
        , onDroidInteractionFailure = Just print
        }
  withDroidSessionHandlers (defaultDroidOptions ".") handlers $ \session ->
    sendDroidTurn session "Say hello." (\_ -> pure ())
```

Handlers receive the existing typed `RequestPermissionParams` and `AskUserParams` payloads. `respondPermission` validates offered choices; `proceed_edit` requires edited content, including an explicitly empty string. `answerDroidQuestion` and `answerDroidQuestionMultiple` copy question identities; `submitDroidAnswers` rejects mismatched/duplicate answers. Multi-select uses comma-space text, and free-form answers remain permitted. The RPC adapters repeat request-bound validation even if these helpers are bypassed.

Unconfigured handlers cancel quietly. Malformed requests, invalid replies and ordinary callback failures cancel safely and can report a payload-free `DroidInteractionFailure`; ordinary failure-observer exceptions are isolated. JSON responses are fully evaluated inside that recovery boundary. Explicit request fields, comments and answers may contain sensitive data.

Only an explicitly configured permission handler disables the CLI's automatic permission rejection. The policy is retained on resume and successor loads. Workers may run concurrently and during initialization; they belong to the original connection scope, not an individual turn or handle. A pending handler can survive turn completion/cancellation or handle retirement. Scope exit cancels and joins workers before reaping the child; handlers must cooperate with asynchronous cancellation. Async cancellation is not converted into approval or a normal decision—return a cancellation result for ordinary rejection.

## Model and context queries

```haskell
import Factory.Droid.Schema.Models (ListModelsOptions(..))

withDroidSession (defaultDroidOptions ".") $ \session -> do
  models <- listDroidModels session (ListModelsOptions Nothing mempty)
  usage <- getDroidContextStats session
  breakdown <- getDroidContextBreakdown session
  pure (models, usage, breakdown)
```

`Nothing` leaves the include-disabled option unset; `Just True` includes disabled models. These typed queries manage their own request IDs and thirty-second exchange deadlines. They can run inside a text/event callback without taking the prompt lock. A query error does not itself invalidate the session; if it escapes a callback, the normal callback-failure rule applies. Transport failures remain terminal. Advanced sessionless model discovery remains available through `Factory.Droid.Client.listModels`.

## Settings, tools and skills

Use a partial update for model, reasoning, mode/autonomy, spec defaults, tags, mission models, compaction and tool policy:

```haskell
import Factory.Droid.Schema.Enums (DroidInteractionMode(DroidSpec), AutonomyLevel(AutonomyOff))
import Factory.Droid.Schema.Settings

withDroidSession (defaultDroidOptions ".") $ \session -> do
  _ <- updateDroidSettings session emptySettingsUpdate
    { updateSettingsMode = Just DroidSpec
    , updateSettingsAutonomy = Just AutonomyOff
    , updateSettingsSpecModel = Just Nothing
    , updateSettingsToolPolicy = emptyToolPolicy { policyRestrictedTools = Just ["Read", "Grep"] }
    }
  listDroidTools session defaultListToolsOptions
```

`Nothing` omits a patch field. Spec-model and spec-reasoning fields use `Just Nothing` to clear a stored override and `Just (Just value)` to set one. Tool lists preserve spelling, order and duplicates; `Just []` sends an explicit empty list. Use `DroidAuto` to leave spec mode. The acknowledgement is retained as peer data, not an optimistic local settings cache.

`listDroidTools` evaluates hypothetical options without applying them. Its `skipPermissionsUnsafe` flag affects only the query's reported allow states. `listDroidCommands` and `listDroidSkills` return metadata without executing commands, invoking skills or opening resource paths. `setDroidSkillDisabled session params` takes `SetSkillDisabledParams` from `Schema.Discovery`, including an optional user/project settings level, and retains the actual success flag.

These controls share the ordinary callback-safe request path and cancellation rules below. The CLI remains responsible for applying model and organization policy. `SettingsUpdatedEvent` is available in `AllEvents` streams and outside-turn subscriptions; its `SettingsChange` payload is not a complete snapshot.

`getDroidSettings session` returns typed `SessionSettings` from the last dispatcher-observed prefix. Initialize/load replies establish full baselines; subsequent settings notifications update them in receive order. The getter sends no request and waits for no future event. Inside a session observer, it includes that notification; immediately after a settings acknowledgement it may still lag queued notifications.

```haskell
settingsView :: IO SessionSettings
settingsView = withDroidSession (defaultDroidOptions ".") getDroidSettings
```

In the selected CLI 0.212.1 contract, notifications mix patches with current override observations: omitted model/reasoning/mode/autonomy and tool-policy fields retain prior values, while omitted spec model, spec reasoning and mission settings clear those observations. Reported mission objects replace their predecessors rather than recursively merging. Valid empty lists, empty spec-model text and `False` remain explicit values. Invalid enum-field fallbacks supply no new observation. Unknown extensions remain data; known snapshot-only extensions such as `sandbox` are validated before appearing in the view.

Malformed settings data makes the getter raise `DroidInvalidEvent` until a valid full load restores the baseline. This alone does not invalidate an idle session. Replacing, retired and closed handles reject reads; successor and rollback loads replace the entire baseline. `SessionSettings` redacts `Show`, not fields or JSON.

This is an ordered observation, not proof of the latest peer state or a snapshot's capture time. In particular, tags are not emitted by the selected CLI settings notifier and can remain at their last reported value. Explicit foreign session IDs are filtered; delayed untagged notifications, or same-ID notifications across rollback, cannot be assigned to a peer epoch without additional protocol information. See [contract evidence](docs/development.md#current-local-sdk-delivery).

## System prompts

`customSystemPrompt text` replaces the built-in prompt; `appendedSystemPrompt text` retains it and adds instructions. Both return `Nothing` for empty or ECMAScript-whitespace-only content without trimming valid text.

```haskell
systemPromptExample :: IO DroidResult
systemPromptExample = do
  prompt <- maybe (fail "Expected nonblank instructions") pure
    (appendedSystemPrompt "Keep explanations concise.")
  runDroid ((defaultDroidOptions ".") { droidSystemPrompt = Just prompt }) "Say hello."
```

System prompts are initialization-only in the selected CLI contract. Passing an explicit prompt to `withResumedDroidSession` raises `DroidSystemPromptRequiresNewSession` before launch rather than silently ignoring it. The SDK does not send an unsupported prompt field during successor or rollback loads. Leave `droidSystemPrompt` unset when resuming.

The wire decoder follows the CLI preset normalizer: object `type`/`preset` fields become `preset`/`droid`; extra keys, missing append text and whitespace-only content fail. This differs from the older Python high-level mapping check. Reported prompts are available through `getDroidSettings`; no unsupported prompt mutation is inferred.

## Session controls

Within a new or resumed session scope:

- `renameDroidSession session title` returns `SuccessResult`, retaining a reported `False` rather than pretending the rename succeeded.
- `changeDroidWorkingDirectory session directory` returns the CLI's resolved path. It does not change the calling Haskell process's directory.
- `getDroidRewindInfo session messageId` supplies the owned session ID and returns available/created/evicted file metadata. It does not open, restore or delete those files; an existing message ID is required.

These ordinary requests use the same managed IDs and exchange deadlines as queries. They can be called from text/event callbacks; the CLI still enforces operation-specific restrictions. If a mutating request is cancelled or times out without a definite reply, the handle becomes unavailable: the peer might still be executing it. Leave that session scope rather than replacing its active session.

## Fork, compact and rewind

```haskell
import Factory.Droid.Schema.Control (ForkSessionParams(..))

withDroidSession (defaultDroidOptions ".") $ \original -> do
  branch <- forkDroidSession original (ForkSessionParams (Just "Branch") Nothing mempty)
  sendPrompt branch "Continue on this branch." Text.putStr
```

Local replacements load the successor on the **same CLI connection**, then retire the old handle. Every successor remains valid only inside the original `withDroidSession` or `withResumedDroidSession` scope. `droidSessionStatus` reports ready/running/replacing/replaced/unavailable state; calls on a retired handle raise `DroidSessionReplaced` with its target ID.

- `forkDroidSession session options` returns the successor.
- `compactDroidSession session options` returns `(successor, CompactSessionResult)` and uses a four-minute compaction exchange deadline. Compaction can invoke the CLI's model and incur charges.
- `rewindDroidSession session choices` takes `DroidRewindOptions` and returns `(successor, ExecuteRewindResult)`. The CLI may restore/delete the selected files; inspect the returned failure counts rather than assuming every change succeeded.

Replacement is rejected during an active turn. It drains existing ordinary requests and prevents new requests from using the source while replacement is underway. A failed successor load attempts to reload the original; `DroidReplacementError` distinguishes successful rollback from failed rollback. Unknown operation outcomes or failed rollback make the handle unavailable.

**Cancellation boundary:** atomic retirement is the commit point. Cancellation delivered before commit can trigger rollback when a successor is known. If retirement wins the race, the source stays replaced even if cancellation prevents receipt of the returned handle; status retains the committed target ID and the outer scope still owns the process. If the handle was not received, leave that scope before explicitly resuming the target. Cancellation requested while masked need not be delivered before commit.

Replacement, rollback and cancellation behavior is verified with native offline peers. No additional live replacement or model run is claimed.

## Existing daemon sessions

`Factory.Droid.Daemon` connects to a caller-selected daemon without launching or owning that daemon. It shares the local runtime's turn, stream, input, output and interaction machinery, but uses a distinct session handle. The following example makes one model request when called; compilation alone does not contact a daemon.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module DaemonExample (daemonExample) where

import Data.Text.IO qualified as Text
import Factory.Droid (DroidResult)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Transport.WebSocket (WebSocketTarget)

daemonExample :: WebSocketTarget -> Daemon.DaemonCredential -> IO DroidResult
daemonExample target credential =
  let options =
        (Daemon.defaultDaemonOptions target credential "/daemon/workspace")
          {Daemon.daemonTurnTimeoutMicros = Just 120000000}
   in Daemon.withSession options $ \session ->
        Daemon.sendPrompt session "Say HELLO and nothing else." Text.putStr
```

Construct a target with `WebSocketTarget "daemon.example.com" 443 "/"`; the path is used as supplied, without an appended suffix. TLS is enabled by default and verifies both the certificate chain and hostname. Private trust roots require explicit `Network.Connection.TLSSettings`. Set `webSocketTLS = Nothing` only when intentionally using a plaintext endpoint, such as a trusted loopback daemon; plaintext provides no credential confidentiality.

Credentials are `DaemonApiKey key` or `DaemonToken token optionalActAsGrant`. They are sent in `daemon.authenticate` after the WebSocket handshake, not an inferred HTTP authorization header. The SDK does not read login files or environment variables. Configuration `Show` instances are redacted; explicit credential fields are not.

Use `Daemon.withResumedSession options savedId` to load a saved session. Cwd and machine options refer to the daemon host and apply only to new sessions; a model override on resume is rejected rather than ignored. The default protocol version is `1.201.1`, selected from the CLI baseline. `daemonProtocolVersion` permits explicit selection, not automatic negotiation or a live compatibility guarantee.

`Daemon.sendTurn` retains terminal outcomes; `sendEvents`, `sendInput*` and `sendOutput*` use the shared event/input/output types. `Daemon.interruptSession` waits for submission and fences the following turn. `Daemon.onSessionEvent` observes notifications outside turns on the dispatcher intake thread: callbacks must not start a turn, await later events or interrupt a turn whose submission is still pending.

The handler scopes `withSessionHandlers` and `withResumedSessionHandlers` accept the same `DroidHandlers`. Defaults cancel; only a configured permission handler disables daemon auto-rejection. Pending interactions from a loaded session run through the existing owned workers. Permission callbacks admit the owned session or an explicitly associated execution session; question callbacks admit only the owned session. Unrelated requests cancel safely, and replies preserve their original execution session ID.

Ordinary scope exit disconnects and joins owned workers; it does not terminate the external daemon, log out, close or delete the saved session. Incomplete turns retain the common invalidation and best-effort interruption policy. There is no automatic reconnect. Local replacement, settings and native-tool/skill control APIs are not exposed on the daemon handle; their daemon counterparts remain separate work. MCP management is described below.

This path is verified against offline plain/TLS peers, not a live Factory daemon. See [daemon contracts and evidence](docs/development.md#existing-daemon-session-delivery).

## External MCP management

Existing local and daemon sessions expose MCP reports and management requests. These operations act on the peer's MCP configuration; they do not run an MCP server in the Haskell process.

| Operation | Local session | Daemon session |
| --- | --- | --- |
| Reports | `listDroidMcpServers`, `listDroidMcpTools`, `listDroidMcpRegistry` | `Daemon.listMcpServers`, `Daemon.listMcpTools`, `Daemon.listMcpRegistry` |
| Add server | `addDroidMcpServer` | `Daemon.addMcpServer` |
| Remove or toggle | `removeDroidMcpServer`, `toggleDroidMcpServer`, `toggleDroidMcpTool` | `Daemon.removeMcpServer`, `Daemon.toggleMcpServer`, `Daemon.toggleMcpTool` |
| Begin authentication | `authenticateDroidMcpServer` | `Daemon.authenticateMcpServer` |
| Cancel authentication or clear stored auth | `cancelDroidMcpAuth`, `clearDroidMcpAuth` | `Daemon.cancelMcpAuth`, `Daemon.clearMcpAuth` |
| Submit OAuth code or error | `submitDroidMcpAuthCode`, `submitDroidMcpAuthError` | `Daemon.submitMcpAuthCode`, `Daemon.submitMcpAuthError` |

Report and callback parameter types are in `Factory.Droid.Schema.MCP`. Mutations return `SuccessResult` from `Schema.RPC`; preserve its `resultSuccess = False` rather than assuming the operation succeeded. Server removal/toggle uses user settings; tool toggle has no settings-level field. Daemon requests always use the handle's session ID, including when parameter extensions attempt to supply another.

Authentication has a five-minute **RPC** budget; other management requests use thirty seconds. An acknowledgement does not establish authentication completion or connected status. `McpAuthRequiredEvent`, `McpAuthCompletedEvent` and `McpStatusEvent` are separate typed metadata events, available through `AllEvents` and session observers, not complete-message callbacks or the retained complete-event history. Completion reports identify a server but carry no request/state ID; do not assign one to a concurrent attempt merely because it arrived last.

Explicit submit/cancel calls can proceed while authentication awaits its reply. The SDK opens no browser, follows no authentication URL and exchanges/stores no token itself. Timeout or caller cancellation does not automatically cancel server-side OAuth or clear credentials. As with other mutations, an uncertain RPC outcome invalidates the session handle before releasing its lease; leave the scope and reattach explicitly if needed. `Show` is redacted for auth payloads and mutation results; fields and JSON remain sensitive.

### Configuration and early observation

Configuration types live in `Factory.Droid.Schema.MCP.Config`. `McpStdioConfig` wraps the existing `StdioMcp`; `McpHttpConfig` and `McpSseConfig` wrap `McpRemoteConfig`. Startup HTTP/SSE headers are ordered arrays. `mcpServerParams` converts a startup value for add-server calls, whose headers are maps: the last exact-name value wins. Records are validated and OAuth scope/client-ID whitespace normalized before launch, connection or configuration mutation. Invalid configuration raises payload-free `InvalidMcpConfiguration`; secret whitespace is retained.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module McpExample (inspectExternalMcp, readGlobalMcp) where

import Factory.Droid
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.MCP (ListMcpServersResult)
import Factory.Droid.Schema.MCP.Config

inspectExternalMcp :: FilePath -> IO ListMcpServersResult
inspectExternalMcp directory =
  let server = McpHttpConfig (McpRemoteConfig "docs" "https://mcp.example.com/mcp" Nothing Nothing mempty)
      policy = defaultMcpSessionOptions {sessionMcpServers = Just [server]}
      options = (defaultDroidOptions directory) {droidMcpOptions = policy}
      handlers = defaultDroidHandlers {onDroidMcpEvent = Just observe}
   in withDroidSessionHandlers options handlers listDroidMcpServers
  where
    observe _ (Right (McpAuthRequiredEvent _)) = putStrLn "MCP authorization requested"
    observe _ (Left _) = putStrLn "MCP observation failed"
    observe _ _ = pure ()

readGlobalMcp :: Daemon.DaemonOptions -> IO GetMcpConfigResult
readGlobalMcp options = Daemon.withConnection options Daemon.getMcpConfig
```

Calling this example connects to the configured services; it is compiled, not executed, during offline verification. `daemonMcpOptions` supplies the same startup policy for daemon sessions. `Nothing` omits the server list; `Just []` transmits an empty list. Neither means “disable every MCP server.” Local replacement and rollback reuse the connection's normalized server/callback policy.

`sessionBlockOnMcpLoad` is accepted only at initialization; explicitly supplying it for resume raises `McpInitOnlyOptionOnResume` before I/O. In the selected CLI it controls a **bounded pre-turn readiness gate**, not a guarantee that every server connected. The CLI can proceed after readiness failure/timeout and resets this override on load. `sessionMcpOAuthCallbackUri` is forwarded as peer configuration; the SDK does not create an OAuth callback listener for it.

`onDroidMcpEvent` is an opt-in **connection-scoped** observer installed before init/load and retained through replacement/rollback. It receives an optional peer session ID and either a typed MCP event or `DroidMcpFailure`. Local observation follows the owned CLI connection across changing sessions; daemon observation filters the attached session. This differs from per-handle subscriptions, which still exclude replacement windows. The callback runs serially on dispatcher intake, must remain brief and must not start turns/replacements or await later events. A session handle is not yet available during startup. Ordinary callback exceptions are isolated; scope cleanup cancels and joins admitted callbacks.

OAuth configuration distinguishes omission, `McpOAuthDisabled` and `McpOAuthEnabled options`. Client credentials require an issuer; client metadata URIs exclude configured credentials and require public token-endpoint authentication. URI-valued fields use RFC 3986 parsing through `network-uri`, with the selected metadata restrictions. Browser/WHATWG aliases, including unescaped spaces or Unicode spelling, are not normalized automatically; use properly encoded URIs. This is not complete JavaScript URL-validator equivalence. Names/commands/header strings follow the selected CLI wire contract rather than Python-only restrictions; passing header data to the peer is not local HTTP-header validation.

Global daemon configuration uses `Daemon.withConnection`, which authenticates without creating/loading a session and uses only endpoint, transport, credential and protocol options. `Daemon.getMcpConfig` and `Daemon.updateMcpConfig` take that connection, not a session handle, and send no implicit session ID. `StoredMcpConfig McpOAuthConfig` represents read results; update parameters instead use `StoredMcpConfig McpOAuthOptions`, because the global update contract does not accept OAuth `false`. Session add-server does accept it. Global update results preserve their success flag, server reports and optional error independently.

The selected local CLI merges a nonempty supplied list with its user MCP configurations; filesystem entries win same-name conflicts. An empty list skips that merge. The Haskell layer writes no MCP configuration files and makes no general persistence promise about the external CLI/daemon. Inspect returned reports after mutations.

External MCP configuration/management and early-event delivery are verified against offline peers. SDK-hosted Haskell tools and live authentication verification remain separate work; see [current evidence](docs/development.md#external-mcp-configuration-delivery).

## Build and example

```sh
nix develop path:.
cabal build all --ghc-options=-Werror
cabal test all --test-show-details=direct
```

The compiled example makes **two model requests** using the existing CLI login and may incur charges. Each turn has a two-minute timeout:

```sh
cabal run droid-example -- /path/to/working-directory
```

The offline high-level tests require no Factory credentials:

```sh
cabal test all --test-show-details=direct \
  --test-option=-p --test-option='High-level local Droid path'
```

## Scope

The current delivery implements local run/session capabilities, the existing-daemon connection/session/turn path and external MCP configuration/management, not complete Python/TypeScript functional parity. Remaining work includes SDK-hosted tools, REST/advanced resources, broader transports, daemon controls, saved-session discovery and broader configuration, attribution/observability, and platform/live verification. The [parity inventory](docs/parity.md) tracks that functional work. Exhaustive schema/codec coverage is a separate [opt-in backlog](docs/exhaustive-codec-backlog.md), not a completion gate.

See [development notes](docs/development.md) for retained build evidence. Licenses and upstream notices are in [LICENSE](LICENSE) and [NOTICE](NOTICE).
