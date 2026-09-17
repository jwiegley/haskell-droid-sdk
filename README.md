# Droid from Haskell

A native SDK for the local Factory Droid CLI and existing daemons: prompts, typed streaming events, final results, follow-up turns and scoped cleanup.

**Verification:** GHC 9.12.4 full suites pass on the observed macOS/GNU/Linux ARM64 targets. Bounded local and daemon runs verify text streaming, final results, multi-turn sessions, interruption and scoped cleanup. See [live verification](docs/development.md#september-14-new-key-live-verification) and the [platform evidence](docs/development.md#ghc9124-platform-verification).

**Functional scope:** native Haskell interfaces cover the functional capabilities of the baselined Python and TypeScript SDKs. The [parity matrix](docs/parity.md) records contracts, verification evidence and the limits of observed platform/live coverage.

## Requirements

- GHC **9.12.4** (`base` 4.21), Cabal (verified with 3.16.1.0), and a C11 toolchain. An optional Nix development shell provides these tools.
- For local sessions, a working, authenticated `droid` installation. Native live verification includes CLI 0.217.0; the frozen reference remains CLI 0.212.1 / protocol 1.201.1.
- For daemon sessions, an existing endpoint and caller-supplied credentials; a local CLI installation is not required.
- Normal SDK use requires neither Python nor Node.js. Local launch reuses the CLI login; daemon authentication is explicit. Credentials do not belong in command arguments or logs.

Examples assume `GHC2024` and `OverloadedStrings`. Short fragments reuse the imports from the first example and their surrounding section; expression fragments belong inside an `IO` action. Standalone modules declare their own imports.

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

## Creation configuration

`Schema.Configuration.SessionConfiguration` supplies supplemental creation-time settings through `droidConfiguration` / `droidSessionConfiguration` and `daemonConfiguration` / `daemonClientConfiguration`. Existing model, cwd, structured prompt, MCP and daemon worktree options keep their original owners; they are not duplicated inside the new record.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module InitializationExample (localOptions, daemonOptions) where

import Data.Text (Text)
import Factory.Droid qualified as Droid
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Configuration
import Factory.Droid.Schema.Enums (ReasoningEffort)
import Factory.Droid.Schema.Settings (ToolPolicy (..), emptyToolPolicy)

configuration :: ReasoningEffort -> [Text] -> SessionConfiguration
configuration effort allowedTools =
  defaultSessionConfiguration
    { configurationReasoning = Just effort,
      configurationToolPolicy = emptyToolPolicy {policyRestrictedTools = Just allowedTools},
      configurationAutoRejectPermissions = Just True,
      configurationDisableBuiltinSkills = Just False
    }

localOptions :: FilePath -> ReasoningEffort -> [Text] -> Droid.DroidOptions
localOptions directory effort allowedTools =
  (Droid.defaultDroidOptions directory)
    { Droid.droidMachineId = Just "configured-host",
      Droid.droidConfiguration = configuration effort allowedTools
    }

daemonOptions :: Daemon.DaemonOptions -> ReasoningEffort -> [Text] -> Daemon.DaemonOptions
daemonOptions options effort allowedTools =
  options
    { Daemon.daemonConfiguration = configuration effort allowedTools,
      Daemon.daemonInitializationTimeoutMicros = Just 60000000
    }
```

The example is compiled only. Pass the resulting options to the existing session constructors; calling those constructors starts/authenticates/initializes the selected engine. Supply actual tool IDs from discovery. An explicit empty restricted list remains an empty allowlist; it is not omission or a grant of permission.

Creation configuration includes caller session/workspace identity, legacy/interaction/autonomy/reasoning/spec settings, mission settings/decomposition metadata, tool policy, source/origin/location, tags, title/privacy, compaction checking, explicit permission-rejection and builtin-skill flags, raw `systemPromptOverride`, and nullable `structuredOutputFormat`. `Nothing` omits optional configuration; explicit false/empty values remain exact. Initial spec strings cannot use update-patch null clearing. Structured-output capability omission, explicit null and an object schema remain distinct; this does not evaluate JSON Schema keywords locally.

Local `droidMachineId` / `droidSessionMachineId` are optional, defaulting to `default`; an explicit empty string is not replaced. A requested session ID must match the initialization reply before a handle is exposed. Configuration extensions cannot override reserved identity, token, model, MCP or other declared fields. The approved Haskell attribution policy omits unsupported optional SDK-language metadata; caller metadata is preserved without forging another SDK identity.

`daemonSystemPrompt` / `daemonClientSystemPrompt` accept the same structured prompt representation as local sessions. A peer that omits `settings.systemPrompt` when that field was requested produces `DroidInvalidEvent` before settings/handle publication. This acknowledgment rule does not apply to the distinct older `systemPromptOverride` field. Existing MCP validation errors are preserved and still precede process/socket acquisition.

Daemon `daemonSpawnOptions` / `daemonClientSpawnOptions` contain a positive finite integer `inactivityTimeoutMs`, explicit inactivity disablement and a runtime settings path. Integers retain exact precision. Creation configuration/prompt/deadline fields remain rejected on resume; the dedicated load configuration below supplies the supported subset. Existing local post-load model override and daemon create-only model/worktree rules remain unchanged.

Daemon initialization permits two wire-timeout attempts with the same session ID, token and parameters but fresh RPC request IDs, following the source daemon's session-ID deduplication contract. Only `RpcRequestTimedOut` retries; other errors, restoration and caller actions do not. `daemonInitializationTimeoutMicros` / `daemonClientInitializationTimeoutMicros` default to 60,000,000 per attempt when absent; zero sends no session initialization, negative values or an overflowing aggregate budget are invalid. The aggregate initialization/restoration budget is twice that value after connection/authentication; it is not a bound on prior transport or hosted-server acquisition. One raw readiness gate covers the complete attempt sequence. Cancellation does not undo remote initialization.

Low-level `Client.initializeSession` / `initializeDaemonSession` validate `InitializeSessionParams` / `DaemonInitializeSessionParams` before dispatch and retain the raw result object. Normal constructors validate their required receipt and publish through the existing settings/load owners. Permission flags are explicit engine inputs, not local approval; changing a flag never fabricates a permission response or credential. See [initialization delivery and fess](docs/development.md#initialization-configuration-delivery).

## Resume and retained load policy

`SessionLoadConfiguration` supplies resume and future-load intent through local `droidLoadConfiguration` / `droidSessionLoadConfiguration` and daemon `daemonLoadConfiguration` / `daemonClientLoadConfiguration`. Daemon-only load controls use `daemonLoadSpawnConfiguration` / `daemonClientLoadSpawnConfiguration`. Existing MCP options remain with their current connection/server owner.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module LoadPolicyExample (withSavedLocal, withSavedDaemon) where

import Data.Text (Text)
import Factory.Droid qualified as Droid
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Configuration
import Factory.Droid.Schema.Settings (ToolPolicy (..), emptyToolPolicy)

loadPolicy :: [Text] -> SessionLoadConfiguration
loadPolicy allowedTools =
  defaultSessionLoadConfiguration
    { loadToolPolicy = emptyToolPolicy {policyRestrictedTools = Just allowedTools},
      loadDisableBuiltinSkills = Just False
    }

withSavedLocal :: FilePath -> Text -> [Text] -> (Droid.DroidSession -> IO a) -> IO a
withSavedLocal launchDirectory identifier allowedTools =
  Droid.withResumedDroidSession
    ((Droid.defaultDroidOptions launchDirectory) {Droid.droidLoadConfiguration = loadPolicy allowedTools})
    identifier

withSavedDaemon :: Daemon.DaemonConnection -> Text -> [Text] -> (Daemon.DaemonSession -> IO a) -> IO a
withSavedDaemon connection identifier allowedTools =
  Daemon.withResumedSessionOnConfigured
    connection
    Droid.defaultDroidHandlers
    identifier
    (loadPolicy allowedTools)
    defaultDaemonLoadConfiguration
```

The example is compiled only; invocation loads the selected session and applies the explicit restriction patch. The daemon function borrows its enclosing connection and does not authenticate, reconnect or take physical ownership. Supply actual tool IDs; `Just []` remains an explicit empty restriction, not omission or permission elevation.

Load intent includes additional/enabled/disabled tools, explicit rejection/skill flags, source/origin/location, snapshot selection and nullable structured-output capability. `restrictToolIds` is not a load wire field: the existing owner performs its settings patch only after a successful load and before publishing the handle/readiness. `prepareLoadSessionParams` exposes this two-step projection for low-level composition; `Client.loadSession` / `loadDaemonSession` reject direct restriction and known init-only parameters. The Client methods retain raw result objects, while normal constructors validate and restore their required snapshots.

Creation policies seed subsequent local replacements and daemon reloads; explicit load configuration can override those future defaults without changing the initial request. An omitted override preserves retained intent, explicit false/empty lists replace it, and `loadStructuredOutput = Just Nothing` clears the capability. Low-level omission stays omitted; normal daemon loads default `loadAllMessages` to true, while an explicit false is retained. A requested message limit is a positive finite integer with exact native precision; a selected snapshot must not be mistaken for complete history.

`loadSessionInfoWithConfiguration` performs an explicit configured reload on an existing daemon connection. Per-session policy selection and generation advancement are atomic. Older loads cannot replace a newer policy or publish newer state; queued load/restriction writes recheck admission at the existing writer. Admission rejection retains its original exception and sends nothing without poisoning a healthy channel. Real transport failures still use the documented payload-free request errors, with original raw-send/cause access. Tokens are fetched afresh for each actual load; a superseded provider wait cannot send its stale request. No successful restriction patch is inferred from ACK-free transport delivery.

The source controller does not automatically retain initialization's unsafe-permission bypass: later loads require explicit `daemonLoadSkipPermissionsUnsafe`. Once explicitly supplied as load intent it follows that session's retained configuration, until replaced or definitively closed. Init-only inactivity duration is never sent on load. Definitive session closure clears per-session policy; inactivity invalidation retains it for the same session. Neither metadata nor policy storage grants a pending permission response or transfers authentication to another principal.

### Observed local working directory

`getDroidWorkingDirectory` reads the last intake-observed local session cwd without changing the caller's process directory. `getDroidWorkingDirectoryState` distinguishes unknown, inherited, reported (including explicit null) and invalid observations using the existing `WorkingDirectoryState` model. Initialization prefers a validated worktree path over requested cwd. Load prefers direct reported cwd, then legacy worktree path only when direct cwd is omitted; malformed direct cwd rejects publication. A successful replacement may inherit a known nonempty parent cwd only when its own state is unknown. Explicit null and invalid state are not concealed by inheritance.

Working-directory response observation and notifications retain wire order; malformed recognized notifications remain invalid until repaired by valid data. A receipt's return alone does not synchronize later intake. The normal create/load/replacement boundaries drain their required observations before exposing handles, while ordinary getters remain last-observed views. Retired handles cannot be adopted to read a successor. Python and TS differ on missing/null cwd fallback; native state preserves those distinctions and does not invent launch cwd as authoritative saved metadata. See [load-policy delivery and fess](docs/development.md#resumeload-policy-delivery).

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

### Mission lifecycle events

Local and daemon `AllEvents` streams expose `MissionStateEvent`, `MissionFeaturesEvent`, `MissionProgressEvent`, `MissionHeartbeatEvent`, `MissionWorkerStartedEvent` and `MissionWorkerCompletedEvent`. The existing session observers deliver them outside turns as well. These metadata events are omitted by `CompleteMessages` and are not retained in `resultEvents`.

```haskell
module MissionEventExample (progressSnapshot) where

import Factory.Droid (DroidEvent (..))
import Factory.Droid.Schema.Mission (MissionProgressEntry (..), ProgressLogEntry)

progressSnapshot :: DroidEvent -> Maybe [ProgressLogEntry]
progressSnapshot (MissionProgressEvent report) = Just (missionProgressLog report)
progressSnapshot _ = Nothing
```

The progress notification carries a complete ordered log snapshot, not entries to append blindly. `ProgressLogEntry` has an opaque timestamp, one of eleven typed payload variants and an extension map. Worker reports retain partial success, false flags, exact numeric values and nested handoffs. Only all-ECMAScript-whitespace `commitId`/`repoPath` strings become absent on decode; nonblank spelling is unchanged, and null/nonstrings remain invalid.

These are observations, not verified work, ownership transfers or instructions to run handoff commands. Owned sessions maintain the read-only mission view described below; they do not start workers or synthesize worker idle transitions. The baseline mission resource's readiness calls remain separate. Cross-session association and pre-baseline observation handling follow the scopes in [owned mission views](#owned-mission-views). See [mission event verification](docs/development.md#mission-lifecycle-event-delivery).

### Mission snapshots and state transformations

`Schema.Mission.MissionSnapshot` retains the complete reported mission state, including opaque timestamps/directory, worker reports, aggregate usage and per-session usage. A resumed daemon's `LoadedSessionState.loadedMissionSnapshot` validates this optional report before handle publication; it is not a live cache.

```haskell
module MissionStoreExample (fromLoadedMission) where

import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Mission qualified as Mission
import Factory.Droid.Schema.Daemon.Session (loadedMissionSnapshot)

fromLoadedMission :: Text -> Daemon.DaemonSession -> Maybe Mission.MissionStore
fromLoadedMission observedAt session = do
  loaded <- Daemon.daemonLoadedState (Daemon.sessionInfo session)
  snapshot <- loadedMissionSnapshot loaded
  pure (Mission.restoreSnapshotAt observedAt snapshot Mission.emptyMissionStore)
```

The example is compiled, not executed. `Factory.Droid.Mission` is an immutable state model: setters return new values, `missionSnapshot` renders the view, and `applyEventAt` consumes already-scoped events from existing observers. Callers own storage, synchronization and observation timestamps. No listener registry, background task, clock read or automatic mission cache is created.

`mergeFrom source destination` preserves the distinction between pristine fields and explicit clears/defaults. Progress snapshots replace the log without removing historical workers; missing failure exit codes preserve prior reports. Worker identities remain unique with reference-compatible insertion/property-key ordering. Per-session usage replaces earlier values and totals are recomputed exactly, with missing credits contributing zero. An empty usage map removes the derived aggregate; a supplied aggregate-only report remains in the raw receipt rather than being mistaken for per-session data.

Explicit worker completion replaces its state record, while progress completion patches it. Unknown extensions are retained as data, not interpreted as worker identities; native maps have no JavaScript prototype-key behavior. The pure model remains caller-owned. Shared association, lookup and subscription APIs are provided through the connection-owned and immutable registry interfaces described in [owned mission views](#owned-mission-views), without creating a process-global manager. See [state verification](docs/development.md#mission-snapshots-and-immutable-state-delivery).

### Owned mission views

`getDroidMissionSnapshot` and `Daemon.getMissionSnapshot` return `IO (Maybe MissionSnapshot)` from the existing connection-owned observation path. They send no RPC and do not wait for future events. `Nothing` means no mission baseline or mission mutation has been observed; ordinary usage or heartbeats alone do not create a mission.

```haskell
module MissionObservationExample (currentPhase, cachedPhase, watchPhase) where

import Data.Text (Text)
import Factory.Droid (DroidError)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Mission (MissionPhase, missionSnapshotState)

currentPhase :: Daemon.DaemonSession -> IO (Maybe MissionPhase)
currentPhase session =
  fmap missionSnapshotState <$> Daemon.getMissionSnapshot session

cachedPhase :: Daemon.DaemonConnection -> Text -> IO (Maybe MissionPhase)
cachedPhase connection identifier =
  fmap missionSnapshotState <$> Daemon.getMissionSnapshotForSession connection identifier

watchPhase :: Daemon.DaemonSession
  -> (Either DroidError (Maybe MissionPhase) -> IO ()) -> IO (IO ())
watchPhase session receive =
  Daemon.onMissionSnapshot session (receive . fmap (fmap missionSnapshotState))
```

The example is compiled, not executed. Named mission observations are retained before load; sessionless local initialization observations bind when the session ID is learned. Full mission load snapshots establish the owner's baseline, then reported workers' provisional stores merge into it. Explicit source fields win without pristine defaults erasing destination data. A nonempty `callingSessionId` associates a loaded worker with its parent's mission; empty strings remain in the immutable receipt without creating an association.

A malformed mission mutation invalidates this view until a successful full baseline replaces it; the getter throws `DroidInvalidEvent` rather than presenting stale state as valid. Settings remain independent. Busy, replaced and closed handles retain the ordinary lifecycle errors, checked atomically with the view.

Usage contributes only for the owner or a registered worker, preferring `inclusiveTokenUsage` when present—including zero—and otherwise using `tokenUsage`. This corrects the prior base-only event-helper behavior. Totals are sums of the selected reports, not independently verified accounting. Missing worker start times use local UTC observation samples, not claims about actual worker startup.

`Daemon.getMissionIdForSession` resolves observed associations, and `getMissionSnapshotForSession` looks up their shared state without loading or creating a mission. Both borrow the connection's lifetime and fail after its scope closes. `MissionRegistry` in `Factory.Droid.Mission` provides the same association, lookup, merge and event operations as immutable values for caller-owned storage; it creates no threads, callbacks or processes.

`onDroidMissionSnapshot` / `Daemon.onMissionSnapshot` observe owned and explicitly associated mission notifications. Ordinary session/turn callbacks remain session-scoped; association grants no permission or question access. Mission callbacks run on the existing serial dispatcher after state updates: getters and ordinary RPC queries are safe, but do not start turns, replace/load sessions or wait for later events there. Unsubscribe is idempotent; replacing or retired handles admit no new notifications. Replacement and rollback finish the ordered load boundary before publishing usable state.

See [mission association verification](docs/development.md#mission-association-and-provisional-observation).

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

## Standalone event feeds and pure accumulation

`Factory.Droid.Stream` exposes the same decoder, accumulator and collector used by normal local/daemon turns. It creates no transport, reader, subscription or remote interruption. Feed already-scoped `DroidEvent` values, or use `feedDroidNotification` for local/daemon envelopes. Daemon envelopes require the matching session ID; local envelopes may omit it. The default `withDroidStream` requires a matching terminal turn ID; working-state changes alone do not complete it. Legacy idle completion is separately selected below.

```haskell
module StreamFeedExample (observeTurn) where

import Control.Exception (bracket, toException)
import Control.Monad (void)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Factory.Droid.Protocol.Dispatch (RpcDispatcher, onRpcError, onRpcNotification)
import Factory.Droid.Stream

observeTurn :: RpcDispatcher -> Text -> Text -> IO () -> IO DroidStreamResult
observeTurn dispatcher sessionId turnId submit =
  withDroidStream options $ \stream ->
    bracket (onRpcNotification dispatcher (void . feedDroidNotification stream)) id $ \_ ->
      bracket (onRpcError dispatcher (void . feedDroidError stream . toException)) id $ \_ -> do
        submit
        consumeDroidStream stream (mapM_ Text.putStr . streamFrameText)
  where
    options =
      (defaultDroidStreamOptions sessionId turnId)
        { streamMode = AllEvents,
          streamTimeoutMicros = Just 60000000
        }
```

The caller owns the dispatcher and its physical scope. `submit` must use the supplied turn ID and perform only the intended submission; listeners are installed first. Do not consume the stream or wait for later events from a dispatcher callback. Cancellation stops local waiting, not remote effects, and never replays submission.

`consumeDroidStream` admits one consumer and runs its callback on that calling thread. Default `CompleteMessages` filters delivery, not accumulation; choose `AllEvents` for append-only text additions. Frames preserve raw events and supply the tool name known at admission without rewriting payloads. Explicit-turn streams retain unknown events as raw data; malformed known events fail after the accepted prefix. Ordinary fed exceptions retain their identity; asynchronous exceptions passed to `feedDroidError` are raised immediately on the feeding thread.

`getDroidStreamResult`, `getDroidStreamFailure` and `droidStreamCompleted` are STM observations. Results are available once a terminal event is admitted, before callback consumption; failure inspection is explicit sensitive data. `streamTurnResult` retains the wire outcome and `streamDurationMs` uses reported duration, including zero, or monotonic elapsed time. `adaptDroidStreamOutput` reuses the existing local output decoder independently of turn success.

`closeDroidStream` is idempotent: it retires feeding, wakes a waiter and discards undelivered frames, but retains an observed terminal result or failure. A callback failure or later scope timeout does not erase a previously received terminal receipt. This separates observation from successful consumption; it differs from Python's timeout-cleared result property. Closing does not unsubscribe a caller-owned source, join an external producer, or establish remote completion. Bracket those resources explicitly. Queues and retained content have no aggregate memory bound.

The optional timeout covers the enclosing action, including submission and callbacks; zero enters no caller setup, negative values are rejected, and the default has no deadline. Cancellation remains cooperative. A scope timeout without an earlier terminal outcome remains available through failure inspection even if the consumer has already closed the stream.

For caller-supplied completion rather than a peer receipt, use the pure accumulator and summary:

```haskell
module StreamSummaryExample (summarize) where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Factory.Droid.Schema.Notifications (AgentTurnCompletionReason)
import Factory.Droid.Stream

summarize :: Text -> AgentTurnCompletionReason -> Scientific -> [DroidEvent] -> DroidStreamSummary
summarize sessionId reason duration events =
  summarizeStream sessionId reason Nothing duration $
    foldl' (\state event -> fst (stepStream state event)) initialStreamState events
```

`summarizeStream` takes explicit duration and completion reason. Omitted usage falls back to the latest usage event, including all-zero usage, or remains absent; an explicit usage value wins. It neither invents counters nor constructs a peer completion event. `decodeNotification`, `decodeDaemonNotification`, `stepStream` and `eventToolName` are also available for explicit pure composition. `adaptDroidSummaryOutput` decodes structured output or final JSON text without changing the summary. Caller-supplied completion grants no session authority.

### Legacy idle completion

`withDroidLegacyStream sessionId timeoutMicros` selects Python low-level `receive_response` semantics in the same feed. Initial Idle and explicit `agent_turn_completed` events are ignored. Every non-idle working state arms completion; the next Idle is delivered after the accepted prefix, followed by the returned `DroidIdleCompletion`. Its `idleTokenUsage` is the latest observed usage, including zero, or `Nothing`. This is an idle observation, not a successful turn, peer completion reason, duration, acknowledgement or fabricated usage receipt.

```haskell
module LegacyStreamExample (observeLegacyResponse) where

import Control.Exception (bracket, toException)
import Control.Monad (void)
import Data.Text (Text)
import Factory.Droid.Protocol.Dispatch (RpcDispatcher, onRpcClose, onRpcNotification)
import Factory.Droid.Stream

observeLegacyResponse :: RpcDispatcher -> Text -> IO () -> (DroidStreamFrame -> IO ()) -> IO DroidIdleCompletion
observeLegacyResponse dispatcher sessionId submit consume =
  withDroidLegacyStream sessionId (Just 60000000) $ \stream ->
    bracket (onRpcNotification dispatcher (void . feedDroidNotification stream)) id $ \_ ->
      bracket (onRpcClose dispatcher (void . feedDroidError stream . maybe (toException DroidStreamClosed) toException)) id $ \_ -> do
        submit
        consumeDroidStream stream consume
```

The caller owns the enclosing dispatcher/physical scope; these brackets release only the two subscriptions on success, error or cancellation. Drain unrelated prior notifications and serialize submissions for the observed session: legacy working states carry no turn identity. Local sessionless notifications require an exclusively scoped source; naming a session does not multiplex an arbitrary shared stream.

Legacy delivery selects assistant/thinking deltas, created tool uses, tool results/progress, usage, non-idle working states, the terminating Idle and error notifications. Unmapped events are filtered without changing correlation state. Error notifications remain data; fed source failures remain exceptions. Tool-name enrichment, single-consumer ownership, inspection, deadlines, cancellation and close use the common implementation. Native malformed known payloads fail rather than being silently skipped as in Python, and full precise usage/extensions are retained rather than reduced to four counters. Ordinary native turns remain explicitly correlated.

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

## Saved-session file observations

`listDroidSessions`, available from both `Factory.Droid` and `Factory.Droid.Discovery`, reads local saved sessions without launching Droid or invoking credential discovery. It combines project and legacy layouts, selects the newest eligible observation for each ID, and applies a final limit.

```haskell
module SavedSessionListExample (recentIds) where

import Data.Text (Text)
import Factory.Droid
  ( DroidSavedSession (savedSessionFile),
    DroidSessionFile (sessionFileId),
    ListDroidSessionsOptions (..),
    defaultListDroidSessionsOptions,
    listDroidSessions,
  )

recentIds :: FilePath -> IO [Text]
recentIds cwd = do
  sessions <- listDroidSessions (defaultListDroidSessionsOptions {listSessionsCwd = Just cwd, listSessionsLimit = Just 20})
  pure (map (sessionFileId . savedSessionFile) sessions)
```

The example is compiled only. `DroidSavedSession` wraps the complete `savedSessionFile` observation, adding a resolved `savedSessionCwd` view and root-level `savedSessionIsFavorite` membership. Raw cwd, header/settings extensions, counts and timestamp provenance remain accessible through the nested observation rather than a rebuilt summary.

| Option | Default and meaning |
| --- | --- |
| `listSessionsDirectory` | `Nothing` selects the current home directory's `.factory/sessions`; `Just path` selects an explicit root. |
| `listSessionsCwd` | `Nothing` selects the process cwd captured at entry; `Just path` selects a workspace. |
| `listSessionsOutsideCwd` | `False` combines matching project directories with cwd-filtered legacy root files. `True` includes root files and one level of `-`-prefixed project directories without a legacy cwd filter. |
| `listSessionsLimit` | `Nothing` returns all selected IDs. Nonnegative limits select the final sorted prefix; zero performs no filesystem work after option validation, and a negative value raises a payload-free user I/O error. |

Explicit empty paths refer to the captured process cwd, not the default home root. A leading `~` in these library options is literal, as in the reference list entry points; it is not shell-expanded. NUL caller paths are rejected, including with a zero limit.

Workspace keys and legacy matching retain both baselined path conventions: canonical filesystem resolution and lexical parent-segment resolution, including the different backslash escaping and missing-suffix rules. Project-directory membership remains independent of a file's descriptive stored cwd. Legacy files require a matching canonical or lexical cwd when outside-cwd selection is disabled; missing or NUL-containing stored cwd cannot match. Stored raw cwd is never rewritten, and an invalid path cannot trigger filesystem normalization.

Eligibility is established before deduplication. Exact modification time sorts newest first, with code-point ID and path ordering for deterministic ties; creation time and filesystem enumeration order do not decide ties. Filtering or an archived/malformed newer copy cannot hide an eligible older observation. Limits apply after all duplicates, rather than the TypeScript implementation's fixed oversampling cutoff, which can omit valid results. This means nonzero queries may read every candidate in the selected directories. Favorites are read once from the root, not from each project. No file is restored, modified or opened as an engine session.

The lower-level `defaultDroidSessionsDirectory`, `scanDroidSessionDirectory`, `readDroidSessionFile` and `readDroidSessionFavorites` remain available for direct file observations. The scanner reads direct `.jsonl` entries in one selected directory, in filename order. Native birth-time handling has compilation and execution evidence on both macOS and GNU/Linux ARM64. See [selection contracts](docs/parity.md#saved-session-selection), [selection verification](docs/development.md#saved-session-selection-delivery) and [Linux execution](docs/development.md#linux-execution-checkpoint).

```haskell
module SavedSessionFilesExample (inspectStoredFiles) where

import Data.Set qualified as Set
import Data.Text (Text)
import Factory.Droid.Discovery
  ( DroidSessionFile (..),
    readDroidSessionFavorites,
    scanDroidSessionDirectory,
  )

inspectStoredFiles :: FilePath -> FilePath -> IO [(Text, Text, Integer, Bool)]
inspectStoredFiles sessionsRoot selectedDirectory = do
  favorites <- readDroidSessionFavorites sessionsRoot
  files <- scanDroidSessionDirectory selectedDirectory
  pure [(sessionFileId file, sessionFileTitle file, sessionFileMessageCount file, Set.member (sessionFileId file) favorites) | file <- files]
```

Use the storage root for `.favorites`, even when the selected directory is a project subdirectory. The example is compiled only. A `DroidSessionFile` retains the filename-derived ID, title, owner, raw cwd, physical message-line count, mission metadata, original header/settings objects, observed modification/status-change times and optional reported birth time. Status-change time is not birth time. Explicit field access and standard I/O exceptions can expose sensitive paths or content despite the record's redacted `Show`.

Readers accept only regular files, acquire nonblocking/CLOEXEC descriptors, verify identity after opening, and read no more than the size observed then. Later appends are not chased; premature EOF rejects the observation. The header is decoded, but later lines are counted rather than decoded or retained. Only CR, space and tab are ignored within each LF-delimited line; vertical-tab/form-feed lines count. Memory follows the first header plus a fixed chunk; settings/favorites decoding retains their observed contents. This is not an immutable snapshot, global memory cap or hard kernel-I/O deadline. Asynchronous cancellation propagates and closes owned descriptors.

Missing, permission-denied, malformed, nonregular and archived session files are omitted. Missing/malformed settings or favorites remain optional; unrelated I/O errors propagate. Nonblank legacy `sessionTitle` overrides `title` without trimming. Invalid optional owner/cwd fields follow Python's tolerant defaults, with raw JSON retained; valid TypeScript-shaped settings provide first-tag mission metadata. Python's truthy archive marker is honored even when unrelated tags are invalid, rather than making an archived entry visible. These reference differences are explicit in the [discovery mapping](docs/parity.md#saved-session-file-scanning).

Stable symlinks are followed, including links outside the supplied directory, as in both references. A selected root is not a physical-containment or trust boundary. No stored header ID or cwd chooses another file to read; settings paths derive from the actual filename. See [native evidence and fess](docs/development.md#saved-session-file-scanning-delivery).

### Creation-time provenance

`sessionFileBirthTime` is `Just` the reported filesystem birth time, or `Nothing` when the platform/filesystem cannot supply it. Zero is a valid reported timestamp, not an absence marker. `sessionFileCreatedAt` supplies the SDK-compatible projection: birth time when present, otherwise the separately retained status-change time. Inspect the optional field when actual birth provenance matters; the fallback is not a claim of true creation.

```haskell
module SavedSessionTimesExample (observedTimes) where

import Data.Time (UTCTime)
import Factory.Droid.Discovery (DroidSessionFile (..), sessionFileCreatedAt)

observedTimes :: DroidSessionFile -> (UTCTime, Maybe UTCTime, UTCTime)
observedTimes file =
  ( sessionFileModifiedAt file,
    sessionFileBirthTime file,
    sessionFileCreatedAt file
  )
```

A small native bridge queries the already-owned descriptor before streaming: macOS uses `fstat` and `st_birthtimespec`; Linux uses descriptor-only `statx` and tests the returned `STATX_BTIME` bit before reading the value. Unsupported Linux OS/filesystem reporting yields absence, not a fabricated zero. Other errors retain errno, and the Haskell safe FFI preserves exact seconds/nanoseconds. Settings and favorites do not request unused birth metadata. These observations do not make concurrently changing files an immutable snapshot.

The installed `unix` birth-time API was reproduced as unsupported on macOS; the native bridge supplies the supported path instead. The macOS bridge and Haskell conversion match Node's bigint nanoseconds exactly. The Linux descriptor-only `statx` path was compiled and executed, with birth-time availability and Haskell conversion checked against independent descriptor observations. Both platform branches are covered by the GHC 9.12.4 macOS/GNU/Linux ARM64 suites. The example above is compiled only, not executed. See the [original timestamp checkpoint](docs/development.md#saved-session-timestamp-checkpoint), [Linux execution](docs/development.md#linux-execution-checkpoint) and [platform verification](docs/development.md#ghc9124-platform-verification).

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

Successful `updateDroidSettings` tool-policy fields are retained for subsequent local successor and rollback loads, in reply-intake order. Omitted fields preserve earlier load intent; explicit empty lists remain overrides. Replacement drains accepted policy observations before constructing its load request, without making ordinary settings updates wait for dispatcher callbacks. Rejected or malformed replies do not update this retained intent. This does not change the separate, notification-driven `getDroidSettings` view described below.

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

## ACP and raw process modes

`Factory.Droid.Transport.Process.droidProcess executable mode` constructs a shell-free `CreateProcess`: `Acp` selects `exec --output-format acp`; `StreamJsonRpc` selects the ordinary input/output stream-jsonrpc flags. Construction starts nothing. `withJsonLinesProcess` supplies the same bounded JSONL pipes and owned shutdown/reaping for either mode.

Droid command builders set `close_fds = True`: unrelated inheritable parent descriptors are closed in the child, without removing the owned stdio pipes or explicitly routed stderr. `prepareDroidProcess` retains this default even when replacing arguments, so ordinary high-level local sessions use it too. The generic JSONL scope still honors a caller-supplied descriptor policy, including `close_fds = False`; use a Droid builder or explicitly enable isolation when constructing a raw command. The separately owned IPC launcher already isolates unrelated descriptors and is unchanged.

`JsonLinesError` includes `ProcessExited ExitCode`, retaining clean exit (`ExitSuccess`), nonzero exit, or a POSIX signal (the platform-specific negative `ExitFailure` convention from `System.Process`). Buffered objects are delivered before clean-EOF diagnostics. Clean EOF and failed writes use a two-second exit-settling budget, separate from shutdown grace; if the child remains live, they retain `EndOfStream` or `ProcessWriteFailure` instead of attributing a later cleanup signal. Known exits reject later sends. Framing errors and caller cancellation remain primary, and these diagnostics contain no stderr, argv or environment payload.

When this transport is wrapped in `RpcChannel`, its existing request-error classification is unchanged; `rpcChannelFailureCause` exposes the original typed process diagnostic through the public channel API. The settling budget is not a hard OS/scheduler deadline and does not replace caller deadlines.

```haskell
module AcpExample (withAcp) where

import Factory.Droid.Transport.Process
import System.Process (CreateProcess (env))

withAcp :: FilePath -> [(String, String)] -> (JsonLinesProcess -> IO a) -> IO a
withAcp executable environment =
  withJsonLinesProcess
    (10 * 1024 * 1024)
    5000000
    ((droidProcess executable Acp) {env = Just environment})
```

The example is compiled, not executed. The supplied environment is explicit and unchanged; `droidProcess` performs no discovery, environment sanitization, authentication or protocol negotiation. Keep credentials out of arguments and configuration logs. Use the launch preparation API below for explicit environment ordering.

Inside the callback, `sendObject` and `receiveObject` exchange raw JSON objects without Factory envelopes or SDK attribution. The caller supplies the ACP protocol layer and finishes concurrent I/O before leaving the scope. This is not an ACP session client: the ordinary `Factory.Droid` session API deliberately remains StreamJsonRpc, as does the baselined SDK transport. Framing errors and cancellation end the exchange; cleanup owns the direct child, not its descendants. See [ACP verification and full fess](docs/development.md#acp-process-mode-delivery).

### Launch arguments, environments and stderr

`DroidLaunchOptions` adds explicit command preparation through `prepareDroidProcess`. `launchPrefixArguments` precedes the selected mode's arguments; `launchExtraArguments` follows them. `launchArguments = Nothing` uses the mode defaults, while `Just []` replaces them with no arguments. Arguments are vectors, not shell text. Prefix arguments belong to the executable explicitly selected by the caller; there is no hidden executable resolver or prefix fallback.

Environment preparation merges inherited values and `launchEnvironment`, applies the caller's sanitizer, then merges `launchTrustedEnvironment`. Later layers win; case-distinct keys and empty values remain distinct. The normal local session API accepts these values through `droidLaunchOptions` and retains its existing removal of inherited/ordinary `FACTORY_UPSTREAM_CLIENT_TYPE` and `FACTORY_UPSTREAM_SDK` before explicit trusted overrides. This is not a general secret scrubber. Environment values never become arguments; explicit configuration fields and `CreateProcess` displays can still contain secrets.

```haskell
module LaunchExample (configuredSession, withCapturedStderr) where

import Data.Map.Strict (Map)
import Factory.Droid (DroidOptions (..), defaultDroidOptions)
import Factory.Droid.Transport.Process
import System.IO (Handle)
import System.Process (CreateProcess (std_err), StdStream (CreatePipe))

configuredSession :: FilePath -> Map String String -> Map String String -> DroidOptions
configuredSession directory overrides trusted =
  (defaultDroidOptions directory)
    { droidLaunchOptions =
        defaultDroidLaunchOptions
          { launchEnvironment = overrides,
            launchTrustedEnvironment = trusted
          }
    }

withCapturedStderr :: FilePath -> DroidLaunchOptions -> [(String, String)] -> (Map String String -> Map String String) -> (JsonLinesProcess -> Maybe Handle -> IO a) -> IO a
withCapturedStderr executable options inherited sanitize =
  withJsonLinesProcessStderr
    (10 * 1024 * 1024)
    5000000
    ((prepareDroidProcess executable StreamJsonRpc options inherited sanitize) {std_err = CreatePipe})
```

This example is compiled, not executed. `withJsonLinesProcessStderr` honors the configured stderr stream: `CreatePipe` supplies an owned binary handle; other `StdStream` choices supply `Nothing`, and `UseHandle` remains borrowed. Drain and join any stderr reader within the callback. Raw stderr is neither bounded nor redacted by this API. Standard `withJsonLinesProcess` and normal session scopes still discard stderr. Stdin/stdout remain owned JSONL pipes.

### Owned child IPC

`withJsonLinesProcessIpc` owns JSONL stdin/stdout and an additional Node-compatible JSON IPC channel. It uses the existing process cleanup and session runtime. The C-only `factory-droid-launcher` performs descriptor handoff and execs the selected program under the same PID; it is not a persistent broker and requires neither Node.js nor Python.

Install the launcher with `cabal install exe:factory-droid-launcher`, or set `ipcProcessLauncher` to its built executable path. This dependency is needed only when selecting owned IPC. `defaultIpcProcessOptions` searches `PATH` and supplies a five-second startup budget.

```haskell
module OwnedIpcExample (withOwnedIpcSession) where

import Factory.Droid qualified as Droid
import Factory.Droid.Transport (processTransport)
import Factory.Droid.Transport.Process qualified as Process
import System.Process (CreateProcess (std_err), StdStream (NoStream))

withOwnedIpcSession :: FilePath -> Droid.DroidSessionOptions -> (Droid.DroidSession -> Process.JsonLinesProcess -> IO a) -> IO a
withOwnedIpcSession executable options action =
  Process.withJsonLinesProcessIpc
    (10 * 1024 * 1024)
    5000000
    Process.defaultIpcProcessOptions
    ((Process.droidProcess executable Process.StreamJsonRpc) {std_err = NoStream})
    (\stdio ipc _ -> Droid.withDroidSessionOn options (processTransport stdio) (\session -> action session ipc))
```

The stdio stream belongs to the ordinary session reader; the callback may use `Process.sendObject` and `Process.receiveObject` on the separate IPC stream according to the peer's protocol. No Factory method or IPC acknowledgement is invented. Finish all channel and captured-stderr threads before returning; scope exit terminates/reaps only the owned PID. A framing, I/O or cancellation failure must end that exchange.

Raw executable/argv, cwd/environment, process-group/session flags and stderr routing are preserved. Use `prepareDroidProcess` instead of `droidProcess` for the explicit sanitizer/trusted-environment policy above. Unrelated parent descriptors are not inherited, even when `CreateProcess.close_fds` is false. The launcher reserves `NODE_CHANNEL_FD=3` and `NODE_CHANNEL_SERIALIZATION_MODE=json` for this channel. Shell commands, child uid/gid changes and delegated terminal control are rejected rather than silently dropped.

`InvalidIpcProcessOptions`, `IpcLauncherUnavailable` and `IpcStartupTimedOut` distinguish invalid settings, missing deployment and startup deadlines; existing payload-free process/framing errors cover setup and exchange failures. The startup budget includes preparation, borrowed-stderr flush and handoff, not the callback. Delivery can still wait for an OS spawn/filesystem call, and cleanup must finish reaping. Zero budget starts no child; negative budget is invalid. `UseHandle` remains caller-owned, including stdout aliases and writable duplex handles. As with ordinary `System.Process` export, borrowed stderr is put in blocking mode; POSIX descriptor aliases share that status change.

Native macOS and GNU/Linux ARM64 checks cover exact objects, normal sessions, flags, failures, blocked I/O cancellation, reaping, stderr ownership and concurrent unrelated execs. The Linux implementation uses the close-from spawn extension introduced in glibc 2.34; older-libc compatibility and the remaining compiler/architecture/live matrix are not claimed. See the [implementation and fess checkpoint](docs/development.md#owned-child-ipc-implementation-and-fess-checkpoint) and [Linux execution record](docs/development.md#linux-execution-checkpoint).

## Host-supplied IPC channels

`Factory.Droid.Transport.IPC` borrows an existing host channel. Supply `IpcMessageChannel` with `ipcIsAvailable`, `ipcSendMessage`, `ipcOnMessage` and optional `ipcOnDisconnect` actions. Messages are JSON text; any filtering of unrelated non-string host messages belongs to the host adapter. Availability is checked during setup and before sending, not treated as authentication.

```haskell
module HostIpcExample (withHostRpc) where

import Factory.Droid.Protocol (RpcChannel, withRpcChannel)
import Factory.Droid.Transport.IPC qualified as IPC

withHostRpc :: IPC.IpcMessageChannel -> (RpcChannel -> IO a) -> IO a
withHostRpc supplied action =
  IPC.withIpcChannel (10 * 1024 * 1024) supplied $ \transport ->
    withRpcChannel (IPC.sendObject transport) (IPC.receiveObject transport) action
```

This example is compiled, not executed. The adapter creates no reader: the existing `RpcChannel` consumes object messages and provides correlation, envelope validation, deadlines and server-request events. `sendObject` serializes JSON without a newline or additional envelope; its return means host send completion, not a peer ACK. Receive/send failures preserve the initiating exception and retire the adapter.

`withIpcChannel` owns only its subscriptions. `closeIpc` is local and idempotent; neither it nor scope exit closes the host connection, logs out or kills a process. `isIpcConnected` reports remembered state, not a heartbeat. Peer disconnect drains already-received messages before reporting `IpcDisconnected`; explicit local close revokes access immediately. Invalid JSON atomically retires and discards the invalid suffix. Empty disconnect reasons remain distinct from absence, while error displays redact them.

Host registration must be exception-safe, callbacks delivered in wire order, and unsubscribe actions nonblocking/cooperative. Finish caller-owned I/O before leaving scope. Early registration messages are retained, early disconnect prevents publication, and stale callbacks cannot revive a closed lease. Scope exit joins an unsubscribe already running on a host callback. Cleanup attempts every stop action; an earlier send/receive/user exception takes precedence over a secondary cleanup failure.

The byte limit applies per UTF-8 message. The callback queue is not capacity-limited, so no total-buffer-memory bound is claimed. Explicit text/JSON fields remain sensitive. This supplied-channel adapter does **not** allocate a child IPC descriptor; normal session integration is provided separately below. See [host IPC verification and full fess](docs/development.md#host-supplied-ipc-delivery).

## In-process runtimes

`Factory.Droid.Transport.InProcess` uses the same callback-channel owner as supplied IPC. `InProcessRuntime` supplies a message handler and optional connect, disconnect, message/close/error subscriptions and pending-readiness registration. Use `defaultInProcessRuntime` when only a send handler is needed. The caller supplies the runtime; the SDK does not implement a daemon.

```haskell
module InProcessExample (withRuntimeRpc) where

import Data.Text (Text)
import Factory.Droid.Protocol (RpcChannel, withRpcChannel)
import Factory.Droid.Transport.InProcess qualified as InProcess

withRuntimeRpc :: Text -> InProcess.InProcessRuntime -> (InProcess.InProcessEvent -> IO ()) -> (RpcChannel -> IO a) -> IO a
withRuntimeRpc url runtime observe action =
  InProcess.withInProcessChannel (10 * 1024 * 1024) url runtime observe $ \transport ->
    withRpcChannel (InProcess.sendObject transport) (InProcess.receiveObject transport) action
```

The example is compiled, not executed. Subscriptions precede connect; the URL is passed unchanged. `InProcessOpened`, `InProcessClosed` and `InProcessError` expose lifecycle observations, exact close code/reason and original exceptions, with redacted displays. Runtime error events and message-handler failures are nonterminal for the raw in-process channel. A failing send still raises its original exception; this does not change `RpcChannel`'s separate write-failure policy or turn a failed request into success.

A native send waits for the supplied message handler to return; no detached processing task is created by the adapter. Runtime implementations should dispatch long-running work rather than await further frames through the same serialized send. Observers run on the caller/runtime delivery thread: do not reenter writes from a send-error observer or wait for work depending on that delivery. Finish caller-owned operations, and make registration exception-safe and teardown cooperative. The supplied disconnect is scoped cleanup, attempted once after subscription/setup failure, failed connect, reported peer close or normal exit; it must tolerate partial or already-closed setup. `closeInProcess` is idempotent. Runtime teardown waits for subscription cleanup even under cancellation: only the cleanup-lock join is uninterruptible, with cancellation delivered at a safe point afterward. Arbitrary runtime callbacks remain interruptible; no shutdown bound is promised for a non-cooperative callback.

`setPendingSessionReady connection sessionId gate` passes a wait-only `STM ()` gate to the optional runtime hook without awaiting or executing it. The producer and its failure remain caller-owned. Absent hooks are inert, while expired transports reject registration. No automatic initialization, resume, approval replay or producer cancellation is implied.

The public IPC API remains intact, and both transports use the existing RPC parser/correlation rather than duplicating client methods. Normal session integration is described below. See [in-process contracts, regressions and full fess](docs/development.md#in-process-channel-delivery).

## Sessions over supplied transports

`Factory.Droid.Transport.ObjectTransport` borrows object send/receive actions plus optional locality and pending-readiness information. Use `processTransport`, `ipcTransport` or `inProcessTransport` with an already-scoped transport, or `objectTransport` for custom object I/O. The normal session runtime still owns its RPC reader, dispatcher, handlers, state and logical lifetime; the caller retains the physical transport scope.

| API | Configuration and scope |
| --- | --- |
| `withDroidSessionOn`, `withResumedDroidSessionOn` and handler variants | Local-protocol sessions using `DroidSessionOptions`, without executable or launch settings. `droidSessionOptions` projects existing `DroidOptions`; `defaultDroidSessionOptions` supplies defaults. |
| `Daemon.withConnectionOn` | Authenticated logical daemon connection using `DaemonClientOptions`, without a fabricated endpoint or root session. Machine ID supplies default local-directory association; no session-specific MCP or creation mutation is performed by connection-only scope. |
| `Daemon.withSessionUsing`, `withResumedSessionUsing` and handler variants | Normal daemon session construction and operations over the supplied transport. Existing WebSocket constructors project into the same implementation. |
| `Daemon.daemonClientOptions`, `defaultDaemonClientOptions` | Project existing WebSocket/session options or construct transport-independent options with explicit authentication and daemon cwd. |

`DaemonAuthenticate credential` performs the normal authentication RPC. `DaemonInheritAuthentication identity token` is an explicit caller attestation that the host has already authenticated this channel; it sends no authentication request. The caller supplies the real identity receipt and exact session token, including an explicitly empty string when applicable. `DaemonTokenProvider getToken actAsGrant` refreshes bearer tokens for authentication and each init/load; `DaemonInheritAuthenticationProvider identity getToken` skips authentication and fetches only for init/load. These provider forms also work without a relay. Availability or locality never selects inherited authentication, and the SDK reads no login files or environment credentials for daemon injection.

```haskell
module InjectedExample (withIpcSession, withRuntimeSession) where

import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Transport qualified as Transport
import Factory.Droid.Transport.IPC qualified as IPC
import Factory.Droid.Transport.InProcess qualified as InProcess

withIpcSession :: Daemon.DaemonClientOptions -> IPC.IpcMessageChannel -> (Daemon.DaemonSession -> IO a) -> IO a
withIpcSession options supplied action =
  IPC.withIpcChannel (10 * 1024 * 1024) supplied $ \channel ->
    Daemon.withSessionUsing options (Transport.ipcTransport channel) action

withRuntimeSession :: Text -> Daemon.DaemonClientOptions -> InProcess.InProcessRuntime -> (InProcess.InProcessEvent -> IO ()) -> (Daemon.DaemonSession -> IO a) -> IO a
withRuntimeSession url options runtime observe action =
  InProcess.withInProcessChannel (10 * 1024 * 1024) url runtime observe $ \channel ->
    Daemon.withSessionUsing options (Transport.inProcessTransport channel) action
```

These examples are compiled, not executed. Keep the supplied transport scope alive around SDK work, and give each logical client exclusive receive ownership. SDK cleanup invalidates its handles and cancels its reader/owned callbacks, but does not close the caller's physical connection or process. Cancellation can retire the borrowed adapter's exchange; it is not a reusable-session or reconnect guarantee. A per-connection UUID request namespace prevents deterministic ID reuse across scopes; it does not multiplex arbitrary shared streams or make stale untagged notifications safe.

Hosted MCP requires `transportLocality = LocalHost`, an assertion that the engine can reach this process's loopback servers—not authentication or folder trust. Generic and IPC adapters default to `UnspecifiedHost`; process/in-process adapters assert genuine co-location. If a supplied runtime proxies a remote engine, retain `UnspecifiedHost` rather than claiming local reachability. Unknown locality rejects session-owned hosted servers before they start.

In-process readiness registration receives a separate wait-only gate for each validated initialization/load RPC receipt. It settles before later controller/terminal restoration, avoiding a circular wait; use the existing session-readiness API when controller completion is required. Registration must not await its own gate. Registration failure and cancellation settle it with the original failure; superseded controller generations remain independently guarded. There is no extra readiness registry or automatic replay. See [injection verification and full fess](docs/development.md#injected-session-runtime-delivery).

## Authenticated relay connections

`Factory.Droid.Transport.Relay.withRelayConnection` authenticates a caller-selected WebSocket relay before publishing an `ObjectTransport` for the existing daemon constructors. Verified TLS and socket ownership remain in `Transport.WebSocket`; relay authentication is a separate control exchange, not a daemon RPC or evidence that the daemon itself is authenticated.

```haskell
module RelayExample (withRelaySession) where

import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Transport.Relay qualified as Relay
import Factory.Droid.Transport.WebSocket qualified as WebSocket

withRelaySession :: WebSocket.WebSocketTarget -> IO (Maybe Text) -> IO (Maybe Text) -> Text -> Text -> (Daemon.DaemonSession -> IO a) -> IO a
withRelaySession target getToken getOrganization machineId cwd action =
  Relay.withRelayConnection WebSocket.defaultWebSocketOptions target Relay.defaultRelayOptions (Relay.RelayTokenProvider getToken getOrganization Nothing) $ \relay ->
    Daemon.withSessionUsing
      ((Daemon.defaultDaemonClientOptions (Relay.relayDaemonAuthentication relay) cwd) {Daemon.daemonClientMachineId = machineId})
      (Relay.relayTransport relay)
      action
```

This example is compiled only. Supply the selected relay endpoint, credential providers, remote machine ID and working directory; the SDK does not discover an endpoint, inspect login files, perform OAuth, refresh credentials on a timer or reconnect implicitly. Invoking the helper establishes a connection and initializes a remote session. `RelayApiKey key` instead forwards the key in the relay token field, omits organization context and supplies the daemon API-key authentication variant. An optional act-as grant on `RelayTokenProvider` belongs only to daemon authentication.

The bearer token and active organization are fetched sequentially for each new relay scope. Absent or empty organization values omit `activeOrganizationId`; whitespace and other text remain exact. `relayOrganization` is that scope's immutable snapshot, never inferred from daemon identity, cwd, locality or a previous connection. Providers must supply coherent credentials for the same principal and selected organization; changing either requires a new scope. Subsequent token refresh does not rebind the relay organization or change the immutable daemon identity receipt. Providers can be called concurrently by independent session operations and must supply their own synchronization and I/O bounds.

The relay handshake has a separate ten-second default budget, starting after provider acquisition. Provider exceptions and cancellation retain their identity; provider work is outside that handshake budget and raw daemon RPC deadlines. Missing or empty relay/initial-auth tokens fail before sending authentication. At init/load, `Nothing` fails with `DaemonCredentialUnavailable` rather than falling back to the authentication token; `Just ""` remains an explicit string, as required by the session wire contract. Readiness gates still describe raw RPC receipts, not preceding credential acquisition.

`RelayAuthenticationRejected` retains the message, optional code, optional retryability, exact positive-integer retry delay and additional fields. Unknown codes, wrong types or null optional fields in a recognized rejection fail promptly with `RelayMalformedAuthentication`. Malformed JSON, binary frames and peer close retain the existing WebSocket failure types. Valid non-authentication objects arriving before acceptance are retained in order, bounded by both `relayPreludeMessageLimit` (64 by default) and `relayPreludeByteLimit` (10 MiB of re-encoded JSON); these limits are not heap-size guarantees. Failed I/O and scope exit revoke both socket use and buffered reads. Finish caller-owned I/O threads before scope exit.

Credentials, rejection data and connection values have redacted `Show` instances; explicit fields, encoded JSON and arbitrary provider exceptions remain sensitive. This object adapter is distinct from the binary tunnel below; neither implies a retry scheduler, tracing backend or endpoint readiness orchestration. Its locality remains `UnspecifiedHost`, even when the relay address is loopback. See [relay authentication delivery and full fess](docs/development.md#relay-authentication-delivery).

## Binary relay tunnels

`relayTunnelTarget relayOrigin computerId port` constructs `/v0/computer/{encoded-id}/tunnel?port=…`, replacing the origin's path/query. Computer identity is an opaque UTF-8 path component: separators are encoded rather than interpreted as routes, and empty, `.` or `..` identities or ports outside 1–65535 are rejected. The WebSocket scope validates the supplied host/relay port and applies the explicit TLS policy. Building a target neither wakes a computer nor establishes trust.

`withRelayTunnel` makes one connection/authentication attempt, then supplies a binary byte channel without daemon JSON-RPC. `defaultTunnelWebSocketOptions` retains verified TLS and the source tunnel's thirty-second setup budget; `defaultRelayOptions` supplies the separate ten-second authentication budget and pre-authentication buffer limits. API keys and bearer organization context follow the relay contract above, but an act-as grant is rejected before acquisition because this endpoint has no daemon authentication operation to apply it to.

```haskell
module TunnelExample (exchangeChunk) where

import Control.Exception (throwIO)
import Data.ByteString.Lazy qualified as Bytes
import Data.Text (Text)
import Factory.Droid.Transport.Relay qualified as Relay
import Factory.Droid.Transport.WebSocket qualified as WebSocket

exchangeChunk :: WebSocket.WebSocketTarget -> Text -> Int -> Relay.RelayCredential -> Bytes.ByteString -> IO Bytes.ByteString
exchangeChunk origin computerId port credential payload = do
  target <- either throwIO pure (Relay.relayTunnelTarget origin computerId port)
  Relay.withRelayTunnel Relay.defaultTunnelWebSocketOptions target Relay.defaultRelayOptions credential $ \tunnel -> do
    Relay.sendTunnelData tunnel payload
    Relay.receiveTunnelData tunnel
```

The example is compiled only. Invocation connects to the selected remote port and sends the caller's bytes. It returns one binary chunk, not an entire application response: tunnel chunks do not define the target protocol's message boundaries. `sendTunnelData` sends binary bytes without JSON/base64 wrapping; completion is not acknowledgment from the target application. `receiveTunnelData` ignores text, including JSON-RPC-looking text. Empty binary payload is data, not EOF. Early binary frames remain bounded and unavailable until relay authentication succeeds. Unlike the object adapter, unrecognized/non-JSON text before tunnel authentication is retained within the same prelude limits and subsequently ignored; a malformed recognized authentication rejection still fails closed. JSON-shaped binary data cannot satisfy authentication.

Peer close raises `WebSocket.WebSocketClose` with its exact code/reason; other transport errors retain `WebSocketError`, and relay authentication failures retain `RelayError`. `isRelayTunnelOpen` reports last-known local openness, not a ping or an assertion about an unread peer close. The adapter creates no receive worker: run the receive loop to observe data, errors and close, and give it exclusive receive ownership. Bound reads or cancel them according to the carried protocol; there is no implicit RPC deadline or replay.

`closeRelayTunnel` revokes both buffered data and socket use. Healthy closure sends code 1000 with reason `tunnel closed`; peer failure or the configured close deadline can instead require forced cleanup. Explicit and scoped close share the same once-only owner. A concurrent close joins existing cleanup; cancellation is delivered after that join without holding the cleanup lock. Only the join is uninterruptible, and protocol I/O remains interruptible. Finish caller-owned I/O threads before leaving the scope.

The underlying `Transport.WebSocket.withMessageWebSocket` exposes `WebSocketText`/`WebSocketBinary`, `sendMessage`, `receiveMessage`, last-known openness and bounded `closeMessageWebSocket`. Text payloads are validated as UTF-8; close reasons are limited to 123 UTF-8 bytes. Existing object scopes still reject binary/non-object input and retain their code-only `WebSocketPeerClosed` error and `Client disconnect` close reason. No second TCP/TLS implementation or competing reader is introduced. See [binary tunnel delivery and full fess](docs/development.md#binary-relay-tunnel-delivery).

## Explicit connection retries and recovery ownership

`Factory.Droid.Retry` supplies a shared bounded executor for explicitly replay-safe work and scoped connection acquisition. Existing connection/session constructors remain single-attempt unless this wrapper is selected. The connection factory must acquire a fresh physical transport and perform only setup/authentication before invoking its callback. `Daemon.withConnection`, owned WebSocket scopes and relay connection/tunnel scopes satisfy that boundary.

```haskell
module RetryExample (withRetriedSession) where

import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Retry qualified as Retry

withRetriedSession :: Daemon.DaemonOptions -> Text -> (Daemon.DaemonSession -> IO a) -> IO a
withRetriedSession options identifier action =
  Retry.withConnectionRetries Retry.connectionRetryOptions (Daemon.withConnection options) $ \connection ->
    Daemon.withResumedSessionOn connection identifier action
```

This example is compiled only. Connection setup/authentication can retry; loading the saved session and running its callback occur after publication and cannot be retried by this wrapper. Do not put `Daemon.withSession` or another session-initializing factory inside `withConnectionRetries`: its pre-callback work is already a remote mutation. Do not reuse a fixed borrowed stream through `withConnectionOn` across attempts. Custom factories must uphold the same replay-safe setup and scoped-callback contract; the SDK cannot infer it from arbitrary `IO`.

| Options | Total attempts and delay schedule |
| --- | --- |
| `defaultRetryOptions` | Three total attempts; 250 ms between failures. |
| `webSocketRetryOptions` | Six total attempts; 500 ms first delay, factor 2, capped at 5,000 ms before jitter. |
| `connectionRetryOptions` | Eleven total attempts; 2,000 ms fixed delay, matching the source connect/auth helper. |
| `reconnectionRetryOptions` | Three total attempts; first delay before any attempt is 1,000 ms, factor 1.5, capped at 10,000 ms, then 0–30% positive jitter. |

Schedules use **milliseconds**, unlike the transport's microsecond timeout fields. `ExponentialBackoff first factor cap` names the first actual delay. The source generic exponential option starts at `delay * factor`; for its 250 ms/factor-2 case, supply a first actual delay of 500 ms. `HalfToFullJitter` uses 50–100% of the computed base; `PositiveReconnectJitter` adds up to 30% after capping, so the final delay can exceed that cap. `retryDelayMillis` exposes the zero-based schedule calculation with an explicit sample in `[0,1)`. Delays are rounded down to whole milliseconds and validated before native timer conversion; negative/nonfinite values and overflow are not silently converted into short waits.

`RetryOptions` supplies an error predicate, failure/success hooks, optional custom delay, explicit exhaustion recovery and an optional wait-only abort action. The failure hook receives a one-based failed-attempt count, including the final eligible failure; a permanent predicate stops before that hook. The success hook receives the number of prior failures, zero on first success. Custom delay receives the failure, its one-based count and the computed base before jitter; it overrides jitter and applies after failures, not to the initial reconnect wait. Hooks can provide logging without a mandatory backend. `retryOnAllError` is explicit caller-selected recovery; otherwise the original last failure is raised.

A connection attempt commits before the success hook and caller body. Neither a success-hook exception, a caller-body exception nor later cleanup failure becomes a new connection attempt or exhaustion fallback. Failed setup scopes are released before another attempt starts, and the caller body stays on the caller thread. `retry` itself is for explicitly selected replay-safe actions, not arbitrary model/session requests. No queue, permission response, uncertain write or old session handle is automatically replayed or adopted.

`retryAbort` accepts a waiting `STM SomeException`, such as `readTMVar` on a caller-owned abort cell. It is checked before attempts and publication and interrupts backoff with that original reason; it does not interrupt in-flight actions or roll back their effects. Transport/request deadlines and ordinary Haskell asynchronous cancellation remain separate. Standard `SomeAsyncException` cancellation is never retried. Custom asynchronous exception types must use the `asyncExceptionToException`/`asyncExceptionFromException` instance pair, be excluded by the predicate, or be supplied through the explicit abort gate; an ordinary exception thrown asynchronously is otherwise indistinguishable from a synchronous retryable failure. Backoff waits use scoped concurrency and leave no abort listener behind. The attempt budget does not bound arbitrary user I/O or hooks.

`withReconnection options ReconnectLocally localFactory externalFactory action` uses the local schedule. Selecting `ReconnectDelegated` invokes only the external factory, once, without local delays or fallback to local restart. That owner supplies its own bounded polling/recovery and fresh scoped transport. Ownership is explicit, never inferred from locality or a machine-name string; each invocation starts a fresh budget rather than mutating a global retry registry. Endpoint readiness is described below; automatic lifecycle/request-hook integration remains separate work. See [retry delivery and full fess](docs/development.md#connection-retry-delivery).

## Connection polling, status and session readiness

`Factory.Droid.Connection.withConnectionController` owns one readiness worker that holds existing scoped daemon connections. It does not create another protocol reader or session runtime. `daemonConnectionPlan` uses the normal daemon constructor; `relayConnectionPlan` composes relay authentication and the existing daemon client options. Plans can supply an explicit `ensurePlannedRunning` action and failure classifier. No daemon start, cloud wakeup or trust change is inferred by default.

```haskell
module ReadinessExample (withReadySession) where

import Data.Text (Text)
import Factory.Droid.Connection qualified as Connection
import Factory.Droid.Daemon qualified as Daemon

withReadySession :: Daemon.DaemonOptions -> Maybe (IO ()) -> Text -> (Daemon.DaemonSession -> IO a) -> IO (Maybe a)
withReadySession options ensureRunning identifier action =
  let plan = (Connection.daemonConnectionPlan options) {Connection.ensurePlannedRunning = ensureRunning}
   in Connection.withConnectionController plan $ \controller ->
        Connection.withReadyConnection controller Connection.defaultConnectionPollOptions $ \connection ->
          Daemon.withResumedSessionOn connection identifier action
```

This example is compiled only. Invocation can run the caller-supplied ensure action, connect/authenticate and load the selected session. `Nothing` means readiness was not obtained; abort or other exceptional termination retains its exception. User work runs on the caller thread after readiness and is not replayed. Finish caller-owned work before leaving the controller scope; expired handles are not adopted by a replacement connection.

`pollUntilConnected` coalesces overlapping calls into one pending operation. The first caller supplies its progress and abort options; joiners do not replace them. Cancelling an individual waiting thread stops only that wait, while the controller still owns the acquisition. `connectionPollAbort` supplies the shared loop's wait-only STM reason; it is checked between stages and interrupts polling delay, not arbitrary in-flight I/O. Scope exit cancels/joins the worker, closes its existing connection scope and settles pending waiters. Active-attempt ownership is separate from unstarted pending work: an old connection's teardown failure cannot consume, settle or repeat a new poll.

The default budget is fifteen counted attempts, separated by one second. Explicitly classified pre-spawn failures remain uncounted for at most 120 seconds of monotonic elapsed time; they count normally afterward. The first observed transport connection can extend the effective limit to leave three further authentication attempts. Progress therefore uses a one-based attempt and an effective maximum that can grow; uncounted attempts can repeat the same number. A zero budget returns `False` without work when the controller is not already ready; an already ready controller still returns `True`. Negative settings are invalid. `attemptInitialConnection` performs at most one attempt without either budget exemption and throws its failure rather than returning `False`. These budgets do not bound arbitrary plan callbacks or in-flight I/O.

The ensure-running action runs before each new physical connection attempt, not for an already usable connection or in-place authentication repair. It must be explicitly supplied, authorized and replay-safe. Failure classification is also explicit: `ConnectionFailure` retains a reason, retryability, a pre-spawn flag and an optional original cause. Native defaults recognize authentication, identity, configuration, relay and known close-code failures; host-specific pre-spawn or transient-token policy belongs to the supplied classifier, not to hostname/locality guesses.

`getConnectionStatus` returns atomic remembered health, failure, polling/recovery state, progress and observer failure. `waitConnectionStatusChange` uses STM state changes rather than another callback registry. `connectionReady` requires both an open SDK exchange and authentication; neither this nor `connectionRetryAllowed` is a heartbeat or proof about a half-open socket. Progress callbacks run serially before attempts and must be cooperative; do not wait for that same poll from its progress callback. Ordinary progress-callback failures are retained in `connectionStatusObserverFailure` and do not terminate polling.

`Daemon.getConnectionHealth` and the STM `readConnectionHealth` distinguish transport lifetime from authentication. Accepted logout leaves an open transport unauthenticated. Explicit `ensureConnectionAuthenticated connection authentication` repairs that state without reconnecting; concurrent callers share the authentication lock. A validated receipt must match the connection's original user/org and current authentication epoch. Identity mismatch, failed or cancelled authentication retires the logical connection; a late receipt cannot undo a later logout. Success updates the current session-token provider but does not rewrite immutable `connectionUser`. Inherited authentication is a renewed explicit host attestation, never inferred from availability.

The original transport failure is available explicitly through `rpcChannelFailureCause` and `Daemon.readConnectionFailureCause`; first-failure recording is atomic with channel retirement, and normal scope cleanup does not overwrite it. Default RPC error displays remain payload-free. The explicit cause and callback exceptions can contain sensitive information and are not sanitized by status displays.

Session readiness remains in the existing `Daemon.getSessionReadiness`/`ensureSessionLoaded` coordinator and raw init/load readiness gates. Connection polling does not invent another session store, replay an uncertain load or automatically approve pending requests. See [readiness delivery and full fess](docs/development.md#connection-readiness-delivery).

## Connection and request hooks

Hooks use the existing RPC correlation and serial intake owners; they do not create another reader or replay requests. `Daemon.withConnectionObserved` reports physical acquisition before authentication, and exceptions during acquisition remain constructor exceptions. The live `onConnectionError`, `onConnectionClose` and `onRequestSettled` registrations return idempotent unsubscribe actions. A close cause retains its original exception, including full WebSocket code/reason when available; `Nothing` denotes logical scope closure, not physical closure of a borrowed transport.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module HooksExample (observeOnce) where

import Control.Concurrent.STM
import Control.Monad (void)
import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Daemon.Cron (defaultListCronsParams)

observeOnce :: Daemon.DaemonOptions -> IO [Text]
observeOnce options = do
  events <- newTQueueIO
  settled <- newEmptyTMVarIO
  let record = atomically . writeTQueue events
  Daemon.withConnectionObserved options (record "open") $ \connection -> do
    void (Daemon.onConnectionError connection (const (record "error")))
    void (Daemon.onConnectionClose connection (const (record "close")))
    void $ Daemon.onRequestSettled connection $ \_ -> atomically $ do
      writeTQueue events "request-settled"
      void (tryPutTMVar settled ())
    void (Daemon.listCrons connection defaultListCronsParams)
    atomically (readTMVar settled)
  atomically (flushTQueue events)
```

The example is compiled only; invoking it connects, authenticates and lists cron records. It records only event categories, not credentials, request IDs, payloads or error text. The caller waits for settlement delivery before leaving the scope; receipt return alone does not imply that an observer has run.

`Daemon.setBeforeRequest connection (Just hook)` replaces the default load guard. It receives the exact string `params.sessionId` (including an empty string) and method on the requesting thread, before writer/pending admission. Absent, null and non-string IDs do not invoke it. The default guard ensures known sessions are loaded, excluding source-defined lifecycle and control-plane operations; unknown sessions are not speculatively loaded. Clearing the hook or invoking its stop action does not restore the prior hook. A stale stop action cannot remove a newer replacement.

The low-level `Protocol.requestReplyWithHookPolicy`, `Client.callWithHookPolicy` and observed variant accept `SkipBeforeRequest`; it skips only the optional guard, never registered mandatory barriers. Daemon authentication waiting remains mandatory and follows the guard. It waits for in-flight authentication, not a new local authorization policy; existing permission/deferred-action checks and server authorization remain unchanged. Explicit logout may revoke an in-flight grant without waiting behind it. The authentication owner retires the logical scope on failure and retains its original outcome for queued callers; cancelling one waiting request does not cancel repair. Guards/providers require caller-supplied bounds: the RPC send/reply deadline starts afterward, except zero/negative budgets reject before guard work.

`Daemon.getPendingCount` (or STM `Protocol.getRpcPendingCount`) counts unresolved outgoing requests, excluding guard work and already accepted replies. `getConnectionId` reports the SDK logical generation while open, not a peer identity or authentication proof. `getTransportKind` reports descriptive `TransportKind` metadata; use `objectTransport` and the native adapters when constructing `ObjectTransport`, whose record now includes `transportKind`. Locality, kind and identity never select credentials or confer permission.

`DroidHandlers.onDroidRequestSettled` observes local startup/turn requests; on a daemon attachment it observes only outgoing requests with that attachment's exact session ID and retires on detach. Construct handlers through `defaultDroidHandlers`; the record has gained this optional field. Connection-wide subscription is separate. Server permission/question requests are not outgoing settlements.

Settlement is admitted once for accepted success/error, malformed response, timeout, cancellation or terminal I/O failure. Ordered result observations precede settlement; bulk failure follows request admission order. Ordinary observer failures are isolated, while guard failures propagate and async exceptions retain their identity. Unsubscribe affects future snapshots, not already admitted callbacks. Intake observers must be cooperative: do not await later intake, load/restore/detach/replace work or an authentication/poll operation whose completion needs that intake. Correlated RPC replies have their own reader, but this does not make lifecycle calls safe inside observers. Scope exit can cancel unfinished intake; applications needing observed delivery must await it before exit. No remote completion or rollback is inferred from settlement. See [delivery evidence and fess](docs/development.md#connection-hook-delivery).

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

Use `Daemon.withResumedSession options savedId` to load a saved session. Cwd and machine options refer to the daemon host and apply only to new sessions; supplied model/worktree options on resume are rejected rather than ignored. The default protocol version is `1.201.1`, selected from the CLI baseline. `daemonProtocolVersion` permits explicit selection, not automatic negotiation or a live compatibility guarantee.

`Daemon.sendTurn` retains terminal outcomes; `sendEvents`, `sendInput*` and `sendOutput*` use the shared event/input/output types. `Daemon.interruptSession` waits for submission and fences the following turn. `Daemon.onSessionEvent` observes notifications outside turns on the dispatcher intake thread: callbacks must not start a turn, await later events or interrupt a turn whose submission is still pending.

The handler scopes `withSessionHandlers` and `withResumedSessionHandlers` accept the same `DroidHandlers`. Defaults cancel; a configured permission handler or explicit manual-response scope disables daemon auto-rejection for loads. Pending interactions from a loaded session run through the existing owned workers. Permission callbacks admit the owned session or an explicitly associated execution session; question callbacks admit only the owned session. Unrelated requests cancel safely, and replies preserve their original execution session ID.

Owned connection scopes disconnect and join their workers; they do not terminate the external daemon, log out, close or delete saved sessions. Explicit turn cancellation/timeouts retain the common invalidation and best-effort interruption policy; detachment alone does not request interruption. There is no automatic reconnect. Local replacement semantics remain distinct from daemon controls. Daemon settings, discovery, skill and MCP operations use the owned handle variants described below.

This path is verified against offline plain/TLS peers, not a live Factory daemon. See [daemon contracts and evidence](docs/development.md#existing-daemon-session-delivery).

### Pending and deferred interactions

`Daemon.pendingInteractions` exposes the connection-owned `Interaction.PendingInteractions` controller without changing response policy. `Daemon.withPendingInteractions` explicitly defers otherwise-unconfigured requests for its scope; configured callbacks remain active. Nested manual scopes fail. Scope exit cancels its undecided requests, preserves prior choices and leaves older automatic requests alone.

| Operation | Contract |
| --- | --- |
| `getPendingSnapshot`, `getPendingPermissions`, `getPendingPermissionsForSession`, `getPendingQuestions` | Enumerate retained metadata: opaque token, wire request ID, execution session, associated surfaces, typed parameters, millisecond observation time and inactivity. Inactive or already-chosen entries can remain until retirement. |
| `waitPendingSnapshotChange` | Wait for a different immutable view; intermediate views may coalesce. Controller expiry fails the wait. |
| `onPendingPermissions`, `onPendingQuestions` | Subscribe to newly admitted requests. `Nothing` selects all surfaces; `Just []` selects none. One registration fires once despite overlapping surfaces; its stop action is idempotent. Ordinary subscriber failures are isolated. |
| `dispatchExternalPermissionRequest` | Forward observation to another controller's subscribers without installing response authority. Respond through the original controller. |
| `respondPendingPermission`, `respondPendingQuestion` | Validate the current opaque token, surface and request, then choose one local response. Permissions allow reported associated surfaces; questions allow execution only. Cancellation is a safe permission fallback. |
| `Daemon.setSessionHandlers` | Replace permission/question/failure/MCP handlers without replacing the attachment. Already-admitted interaction callbacks retain their handler snapshot. |

A successful response call means **local choice accepted**, not remote acknowledgment or completion. The first selected response or suppression wins. Stale, foreign, inactive and already-answered requests raise distinct `PendingInteractionError` values. Association-only live/load replays update the snapshot without rerunning an admitted callback; observe these changes through `waitPendingSnapshotChange`. A conflicting undecided replay cancels instead of using the old callback's approval. An already-chosen response cannot be withdrawn by later metadata.

Inactivity or process exit retains undecided prompts as inactive and sends no response. `Daemon.deferPermissionResponse` and `deferQuestionResponse` validate an inactive prompt and atomically store its decision under the **execution session and tool identity**. They require a usable tool identity and do not send, resume or replay anything. Low-level `storeDeferredPermission`, `storeDeferredQuestion`, `takeDeferredPermission`, `takeDeferredQuestion` and `clearDeferredUserActions` manipulate the same `SessionState` data. Storage alone is not approval: inspect a fresh request and explicitly submit the retained response through its new token. Permission-resolution notices retire matching associated permission data, never questions or unrelated sessions.

```haskell
module PendingExample (withManualSession, answerOrDefer) where

import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Interaction qualified as Interaction
import Factory.Droid.Schema.Interaction (RequestPermissionResult)

withManualSession :: Daemon.DaemonConnection -> Text -> (Daemon.DaemonSession -> Interaction.PendingInteractions -> IO a) -> IO a
withManualSession connection identifier action =
  Daemon.withPendingInteractions connection $ \controller ->
    Daemon.withResumedSessionOn connection identifier $ \session -> action session controller

answerOrDefer :: Daemon.DaemonConnection -> Text -> Interaction.PendingPermission -> RequestPermissionResult -> IO ()
answerOrDefer connection surface pending response
  | Interaction.pendingInactive pending =
      Daemon.deferPermissionResponse connection surface (Interaction.pendingInteractionId pending) response
  | otherwise =
      Interaction.respondPendingPermission (Daemon.pendingInteractions connection) surface (Interaction.pendingInteractionId pending) response
```

This example is compiled, not executed. Its snapshot can become stale: the called operation revalidates the token and state rather than silently retrying. Pending subscribers run in owned request workers, not a second reader; keep callbacks cooperative and avoid waiting for their own retirement. Existing serial notification callback restrictions still apply. Redacted displays do not sanitize explicit parameters, answers or JSON.

`Daemon.isAuthenticated` combines the remembered authentication flag with connection lifetime; it is not a heartbeat or token-validity check. Accepted `Daemon.logout` clears that flag, pending authority and deferred decisions before returning; rejection preserves them. It neither closes the connection nor rewrites the immutable `connectionUser` receipt. See [pending-interaction contracts, regressions and full fess](docs/development.md#pending-interaction-delivery).

### Independent attachments on one connection

`withSessionOn connection options` creates a new session on that same authenticated owner. `defaultDaemonSessionOptions directory` supplies the ordinary defaults; `daemonSessionParameters` reuses `Schema.Configuration.InitializeSessionParams` for machine/model, system prompt, worktree, MCP and other creation fields. Spawn options, initialization/turn budgets and future load configuration are separate fields. `withSessionOnHandlers connection handlers options` installs per-attachment handlers before initialization. Connection authentication, protocol and transport settings are not accepted or silently replaced by these per-session options. Initialization MCP configuration is forwarded; subsequent reloads retain the connection's existing MCP policy. Caller-owned hosted-server endpoints must remain alive for the operations that use them.

`withResumedSessionOn connection savedId` attaches a saved session using an existing authenticated connection, without opening another transport or authenticating again. `withResumedSessionOnHandlers` additionally accepts per-session `DroidHandlers`. Settings and turn admission are session-local; different attached sessions can have concurrent turns. The connection remains the owner of the common reader, dispatcher and authentication context.

```haskell
module SharedAttachmentExample (createNew, inspectIndependent, closeSaved) where

import Data.Aeson (Object)
import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Settings (SessionSettings)

createNew :: Daemon.DaemonConnection -> Text -> IO Text
createNew connection directory =
  Daemon.withSessionOn connection (Daemon.defaultDaemonSessionOptions directory)
    (pure . Daemon.sessionId)

inspectIndependent :: Daemon.DaemonConnection -> Text -> Text -> IO SessionSettings
inspectIndependent connection firstId secondId =
  Daemon.withResumedSessionOn connection firstId $ \first ->
    Daemon.withResumedSessionOn connection secondId $ \second -> do
      _ <- Daemon.getSettings first
      Daemon.detachSession first
      Daemon.getSettings second

closeSaved :: Daemon.DaemonConnection -> Text -> IO Object
closeSaved connection identifier =
  Daemon.withResumedSessionOn connection identifier Daemon.closeAttachedSession
```

The example is compiled, not executed. Attachment scope exit calls `detachSession`: it retires that handle, releases subscriptions and joins its admitted local operations without closing the connection or sending remote close/logout/interruption requests. Detach is idempotent, and a stale handle cannot remove a newer attachment of the same ID. Duplicate active attachments fail with `DaemonSessionAlreadyAttached` before initialization or loading. Failed or cancelled creation/load releases the attachment lease without publishing a handle; cancellation does not undo a remote operation that already ran. The creation path uses the owner's current token provider and existing stable-ID initialization retry, while caller work remains outside that retry.

`closeAttachedSession` explicitly requests remote close and then detaches the particular handle on a successful reply, even without a lifecycle notification. It also invalidates that current attachment's load generation and child linkage; a late reload cannot restore the closed child, and an older handle cannot retire a newer lease. Remote errors remain errors; no rollback or retry is inferred. A lifecycle notification for one session also retires only that session. Other attachments remain usable while the physical connection remains healthy. As with existing RPC operations, cancellation during an uncertain transport write can fail the shared channel rather than conceal a partial frame.

Permission routing prefers the execution session, then the first active associated session in reported order; questions use only their exact session. Detaching cancels that attachment's blocked permission/question callback rather than another session's worker. Stream callbacks remain on the caller's thread. Notification callbacks remain serial, and an already admitted ordinary callback may finish; keep callbacks cooperative and do not call lifecycle operations from them.

`sessionConnection` borrows the original physical owner's scope: `withConnection`, or the owning `withSession`/`withResumedSession`. Detaching a borrower does not shorten that owner's lifetime, and borrowing does not extend it. Borrowed resume uses the connection's existing authentication/MCP policy and the default unbounded turn timeout. Creation and explicit load configuration use `withSessionOn` and `withResumedSessionOnConfigured`; retained state and queues are described below. See [attachment verification](docs/development.md#independent-daemon-session-attachments).

### Session cache controls

Each connection defaults to a twenty-entry **registered-session** cache. `getSessionCacheCapacity` returns `Just 20`; `setSessionCacheCapacity` accepts an exact `Natural` limit, with `Nothing` for unlimited retention and `Just 0` for protected entries only. It returns the evicted IDs in least-recently-used order. `getCachedSessionIds` returns the remaining registered IDs in insertion order after eligible maintenance.

Initialization, loading and child registration update recency. `touchSession` explicitly touches an existing cached entry and returns `False` for an unknown ID, without registering or loading it. Reading `getSessionState` alone does not touch it. `setActiveSessionId` pins a viewed entry, including a future ID; it does not select a daemon-side session. `getActiveSessionId` reads that selection, and passing `Nothing` releases it.

```haskell
module SessionCacheExample (keepSelected, releaseSelected) where

import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon

keepSelected :: Daemon.DaemonConnection -> Text -> IO [Text]
keepSelected connection identifier = do
  _ <- Daemon.setSessionCacheCapacity connection (Just 20)
  Daemon.setActiveSessionId connection (Just identifier)

releaseSelected :: Daemon.DaemonConnection -> IO [Text]
releaseSelected connection = Daemon.setActiveSessionId connection Nothing
```

This example is compiled, not executed. `pruneSessionCache` reapplies the limit explicitly; ordinary load, detach and notification boundaries also maintain it. Attached, selected, loading and non-idle entries are protected, as are retiring local operations, unresolved restored requests and deferred decisions. Protected entries can exceed the configured capacity. `removeCachedSession` returns `False` for absent or protected entries. These operations neither close a remote session nor log out, and blank cache identities fail with `InvalidSessionCacheIdentity`.

Retirement discards cached conversation, cwd, child-link and terminal views. It retains independently owned load intent, child summaries and identity counters; an older load receipt or terminal acknowledgement cannot restore or consume a newer cached view. Failed creation removes a fresh provisional entry only while its generation and attachment still own it, preserving previously cached or superseding state. None of this is a hard memory bound: unregistered observations, durable metadata, other owners and caller-held immutable snapshots remain outside this cache.

### Local directory and machine groups

`getSessionDirectory` returns the registered local entries in insertion order, with session ID, machine association, readiness and observed/inherited cwd. This is not `listOpenedSessions`, which queries the daemon. `readSessionDirectory` reads the same owners atomically without cache maintenance and can be composed with STM `check` for observation.

`registerSessionState connection identifier machine` registers an empty, not-yet-loaded local entry without an RPC. It returns `False` for an existing entry without changing its association or recency. `getSessionMachineId` returns `Nothing` for an absent entry. `setSessionMachineId` explicitly reassigns an existing entry and fails with `DaemonSessionNotRegistered` if it is absent. Empty machine IDs remain literal. Neither registration nor reassociation selects a transport, creates a daemon session or moves remote work.

Fresh loads and child registration use `daemonClientMachineId` (or `daemonMachineId` for endpoint options) as their default association. A fresh creation uses `initializeMachineId` from its creation parameters. Existing registrations retain their association through loads; use the setter to change it. Cache retirement removes the association, so later fresh registration applies its own defaults.

```haskell
module SessionDirectoryExample (associate, watchDirectory) where

import Control.Concurrent.STM (atomically, check)
import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon

associate :: Daemon.DaemonConnection -> Text -> Text -> IO [Daemon.SessionDirectoryEntry]
associate connection identifier machine = do
  _ <- Daemon.registerSessionState connection identifier machine
  Daemon.setSessionMachineId connection identifier machine
  Daemon.getSessionDirectory connection

watchDirectory :: Daemon.DaemonConnection -> [Daemon.SessionDirectoryEntry] -> IO [Daemon.SessionDirectoryEntry]
watchDirectory connection previous = atomically $ do
  current <- Daemon.readSessionDirectory connection
  check (current /= previous)
  pure current
```

This example is compiled, not executed. Registration remains subject to cache eligibility: pin a future ID first if it must survive a zero-capacity policy. The directory reader reflects local observations, not a network health or machine-ownership authorization check; it cannot extend the physical connection's lifetime.

`hasActiveSessionsForMachine` reports remembered loaded/loading membership, excluding pre-init entries. `countActiveSessionsForCwd` instead counts non-idle entries for an exact machine/cwd pair, also excluding pre-init. Thus an optimistic, not-loaded child with an inherited cwd can count as work without counting as loaded/loading membership. An unreported working state counts as idle; relevant malformed observations fail explicitly instead of silently becoming zero.

`markSessionsNotLoadedForMachine` atomically invalidates only that group's loaded/loading entries and returns their IDs. Existing epochs reject older receipts, while other groups, association, cached history and retained load intent remain intact. Already-not-loaded entries are unchanged. It resets remembered working state for the invalidated entries but does not stop remote work, detach handles or close a transport. Reassignment changes which group a subsequent invalidation selects.

### Coordinated loading and readiness

`loadSessionInfo connection savedId` explicitly loads and validates a receipt without creating a public session handle. Initialization and scoped resume use the same coordinator. A newer explicit load supersedes an older generation; stale receipts cannot overwrite settings, mission state or readiness. The older caller receives `DaemonLoadSuperseded`, not an obsolete success.

`ensureSessionLoaded` is different: it does nothing for unknown or already-loaded sessions, joins an existing load, and reloads known not-loaded state. A check during an explicit reload waits for that flight. Load RPC and ordered-intake processing retain the existing sixty-second boundary; joining does not restart it. Cancelling a joined waiter leaves the owner running; cancelling the owner preserves its original asynchronous exception and releases other waiters with `DaemonLoadInterrupted`. Duplicate attached handles remain prohibited independently of load coordination.

```haskell
module ReadinessExample (reload, ensureKnown) where

import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon

reload :: Daemon.DaemonConnection -> Text -> IO Daemon.DaemonSessionInfo
reload = Daemon.loadSessionInfo

ensureKnown :: Daemon.DaemonConnection -> Text -> IO Daemon.SessionReadiness
ensureKnown connection identifier = do
  Daemon.ensureSessionLoaded connection identifier
  Daemon.getSessionReadiness connection identifier
```

The example is compiled, not executed. `SessionReadiness` distinguishes known state, not-loaded/loading/loaded phase, an in-flight operation, not-found and pre-init flags, and observed or event-derived working state. A refresh can retain the previous loaded phase while `readinessLoading` is true. `sessionReadinessBusy` reports an error for malformed working-state data; absence means no observed busy state, not proof of remote idleness. Busy is independent of local handle admission and of load completion.

Snapshot working state is applied at the existing ordered reply-observation boundary. Later working-state notifications remain authoritative, rather than being overwritten when a load waiter finishes. Malformed full receipts and restored-request containers do not publish new settings. The same generation governs restored-request admission; previously admitted callbacks retain their normal ownership and cancellation rules. Not-found preserves the original RPC error and marks readiness; `clearSessionNotFound` clears only the marker. `setSessionPreInit` changes metadata without speculatively loading or pinning a connection.

`markSessionNotLoaded` invalidates known readiness and the authority of an older pending result; it performs no remote mutation. Lifecycle notices also invalidate readiness, but loading does not resurrect a detached handle. Reattach explicitly when an owned handle is needed. `waitSessionReadinessChange` waits for a differing snapshot using STM; intermediate states may coalesce and connection scope expiry ends the wait. Getters are safe in ordinary notification callbacks; load/ensure and readiness waits are not, because they can depend on later intake. See [load-coordination verification](docs/development.md#daemon-load-coordination).

### Child session hydration

`ChildSessionAvailableEvent` records provisional parent/tool linkage before session callbacks run. `daemonHydrateChildSessions` defaults to `True`: discovery starts a connection-owned load through the existing coordinator. Set it to `False` to retain discovery without automatic loading. Discovery and hydration neither execute a child nor grant it a parent's permission handler.

`registerChildSession connection parentId notice` records metadata only. `findSubagentSessionId`, `getSubagentSessionIdsForParent` and `getSubagentSessionIdsByParent` resolve parent/tool pairs; first registration or load wins duplicate-pair lookup. A self-link or conflicting provisional parent is ignored. A validated load can replace provisional linkage. Blank identities are rejected without trimming valid identifiers.

```haskell
module ChildSessionExample (loadChildSummary) where

import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Mission (SubagentInvocationSummary)

loadChildSummary :: Daemon.DaemonConnection -> Text -> Text -> IO (Maybe SubagentInvocationSummary)
loadChildSummary connection parentId toolUseId = do
  child <- Daemon.findSubagentSessionId connection parentId toolUseId
  case child of
    Nothing -> pure Nothing
    Just childId -> do
      attached <- Daemon.ensureChildSessionAttached connection childId
      if attached
        then Daemon.getSubagentInvocationSummary connection childId
        else pure Nothing
```

The example is compiled, not executed. After discovery, `ensureChildSessionAttached` coalesces or reuses the physical connection's load; an unknown child returns `False` without an RPC. It does not create a handler-owning `DaemonSession`: use `withResumedSessionOn` or `withResumedSessionOnHandlers` for that scope. Load/ensure operations must not run in serial notification callbacks.

`setSubagentInvocationSummary` and `hydrateSubagentInvocationSummaries` update reported summaries without registering or loading children. Bulk hydration preserves unrelated entries and skips blank IDs. A new availability notice reopens a terminal summary as running; a turn-completion event refreshes observed tool count and positive duration only for an existing summary tagged `subagent`. Turn completion does not manufacture a terminal task status.

`SessionState.sessionChildLoadError` distinguishes not-found, interrupted and other failed loads. Explicit callers retain their original errors, and retry clears the observation. Scope exit cancels owned hydration jobs without remote close/logout; cancellation does not undo a remote effect. Explicit state fields and summaries remain sensitive despite redacted state `Show`. Provisional cwd inheritance is described under observed working directories below. See [contracts, regressions and fess](docs/development.md#child-session-hydration-delivery).

### Subagent tag metadata

`findSubagentSessionTag` returns the first exact typed `subagent` tag. Its `Maybe` result supplies presence; existing record selectors and `KeyMap.lookup` supply string-valued calling metadata:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module SubagentTagsExample (isSubagent, callingIds) where

import Data.Aeson.KeyMap qualified as KeyMap
import Data.Maybe (isJust)
import Data.Text (Text)
import Factory.Droid.Schema.Session (SessionTag (sessionTagMetadata), findSubagentSessionTag)

isSubagent :: [SessionTag] -> Bool
isSubagent = isJust . findSubagentSessionTag

callingIds :: [SessionTag] -> (Maybe Text, Maybe Text)
callingIds tags =
  let metadata = sessionTagMetadata =<< findSubagentSessionTag tags
   in (metadata >>= KeyMap.lookup "callingSessionId", metadata >>= KeyMap.lookup "callingToolUseId")
```

The first matching tag wins even when its metadata is absent, empty or lacks one calling field; later tags are not merged. Empty and whitespace string values remain literal, and the selected tag retains all metadata and extensions. For optional tag lists, use ordinary `Maybe` composition, as in `settingsTags settings >>= findSubagentSessionTag`. Existing codecs still reject null/non-string metadata; this is inspection of typed session tags, not an alternate permissive decoder.

Extraction creates no linkage, load or execution authority. The daemon reuses this selector only for its existing tag-presence check; reported/provisional linkage rules remain separate. Returned tags, their existing `Show` and explicit JSON can expose supplied metadata; inspection is not redaction.

For raw JSON metadata, use `inspectSubagentSessionTag`. It selects the first matching object without decoding it as `SessionTag`, retaining numbers, nulls, arrays and extensions:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module RawSubagentTagsExample (callingIds) where

import Data.Aeson (Object, Value (Object))
import Data.Aeson.KeyMap qualified as KeyMap
import Factory.Droid.Schema.Session (inspectSubagentSessionTag)

callingIds :: [Object] -> (Maybe Value, Maybe Value)
callingIds tags =
  case inspectSubagentSessionTag tags >>= KeyMap.lookup "metadata" of
    Just (Object metadata) -> (KeyMap.lookup "callingSessionId" metadata, KeyMap.lookup "callingToolUseId" metadata)
    _ -> (Nothing, Nothing)
```

A present null calling ID is `Just Null`, distinct from an absent field. Missing, null, scalar or empty metadata on the first tag does not permit fallback to a later tag. These raw values are not validated session/tool identities; normal tag codecs and linkage admission remain unchanged.

### Pure content and inspection helpers

`Schema.Content.buildUserMessageContent` assembles supplied images, documents and optional text in that order. Defaults preserve text verbatim and include explicitly empty text; omission emits no text block. Trimming uses ECMAScript whitespace. This low-level builder performs no file I/O, base64 validation or attachment-limit checks; normal `Factory.Droid.Input` handling retains those responsibilities.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module UtilityHelpersExample (content, nonPendingResults, rawResultIds) where

import Data.Aeson (Object, Value (String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text (Text)
import Factory.Droid.Schema.Content
  ( Base64ImageSource,
    ContentBlock (ContentToolResult),
    DocumentSource,
    ToolResultBlock,
    UserContentOptions (..),
    buildUserMessageContent,
    defaultUserContentOptions,
    inspectToolResultId,
    isPendingToolResult,
  )

content :: Maybe Text -> [Base64ImageSource] -> [DocumentSource] -> [ContentBlock]
content = buildUserMessageContent (defaultUserContentOptions {contentTrimText = True, contentIncludeEmptyText = False})

nonPendingResults :: [ContentBlock] -> [ToolResultBlock]
nonPendingResults blocks = [result | ContentToolResult result <- blocks, not (isPendingToolResult result)]

rawResultIds :: [Object] -> [(Object, Maybe Value)]
rawResultIds blocks = [(fields, inspectToolResultId fields) | fields <- blocks, KeyMap.lookup "type" fields == Just (String "tool_result")]
```

Only scalar text `__TOOL_RESULT_PENDING__` is the pending marker; an array containing that text is not. Removing markers does not establish successful or completed execution. Typed results expose `toolResultToolUseId`; raw inspection prefers `toolUseId` unless absent or null, then uses `tool_use_id`. Empty and non-string values survive inspection, but the canonical codec still requires its declared camel-case field.

Other pure projections remain in their owning modules:

| API | Contract |
| --- | --- |
| `Schema.Usage.sumTokenUsage` | Exact known counters and credits; missing credits contribute zero and empty input returns zeros. Unknown extensions are not summed. Empty mission aggregation remains absent. |
| `SessionState.summarizeSessionToolUsage` | Assistant tool-use count and optional positive last-minus-first duration in supplied order. Nonzero updated timestamps outrank created timestamps; zero is not an observation. No synthetic linked session is required. |
| `Schema.Interaction.permissionToolInputForDisplay` | Matching exit-spec/mission details fill absent, empty or non-string plan/proposal/title fields. Existing nonempty strings, including whitespace, win. The original input and permission decision are unchanged. |
| `Schema.Settings.hasDecoupledInteractionSettings` | Presence of either `interactionMode` or `autonomyLevel`, including null or invalid raw values; not settings validation. |
| `Schema.Notifications.equalToolStreamingUpdates` | Deep object equality ignoring only top-level `timestamp`; nested timestamps and other fields still matter. It does not apply an event or alter the state owner's deduplication/deadline policy. |
| `Schema.Host.machineConnectionType` | Exact computer→computer, local→tui, ephemeral→workspace mapping; unknown names return `Nothing`. Descriptive labels grant no authority. |
| `Schema.RPC.inspectJsonRpcEnvelope` | Explicit expected protocol version, then raw JSON. Returns `Nothing` for invalid present known fields, otherwise the original object and an optional `ProtocolVersionMismatch`. Missing fields, including all fields, are permitted only for this inspection. |

Envelope inspection validates present literals, protocol version, message type, method, nullable ID and metadata through the existing native codecs. It does not interpret body fields, admit a wire message, negotiate versions or choose attribution. Native inspection retains the original nested extensions; the pinned TypeScript helper returns a Zod-normalized copy that strips unknown nested metadata. Native known metadata fields follow the supplied 1.205.0 codec rather than that older schema. Missing, null and empty IDs remain distinct in mismatch observations.

Native `Exception`, `SomeException`, `toException`, `throwIO` and the existing SDK errors provide error conversion without a JavaScript-style hierarchy or arbitrary-value stringification. Explicit objects, metadata, JSON and exception causes can contain sensitive data; these helpers do not redact them. The examples are compiled only. See [utility verification and fess](docs/development.md#utility-helper-delivery).

### Optimistic submissions

`Daemon.submitUserMessage` sends a connection-level command with an explicit request ID, using the same backend submission path as ordinary daemon turns. `Factory.Droid.SessionState` retains real messages and separate optimistic entries: each entry carries its request ID, assistant-placeholder ID, exact input, status and in-flight flag. It does not emit synthetic stream messages or render a conversation.

```haskell
import Data.Aeson (Object)
import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Control (defaultUserMessageParams)
import Factory.Droid.SessionState qualified as State

submit :: Daemon.DaemonConnection -> Text -> Text -> Text -> Text -> IO Object
submit connection savedId requestId placeholder prompt = do
  let params = defaultUserMessageParams prompt
  Daemon.registerOptimisticSubmission connection savedId requestId placeholder params Nothing
  Daemon.submitUserMessage connection savedId requestId params

watchOnce :: Daemon.DaemonConnection -> Text -> IO State.SessionState
watchOnce connection savedId = do
  previous <- Daemon.getSessionState connection savedId
  Daemon.waitSessionStateChange connection savedId previous
```

This example is compiled, not executed. Supply a request ID unique across the connection, preferably a UUID, and an independently chosen placeholder ID. Preparation is optional: sending creates an overlay when none exists. Re-registering the same request/placeholder preserves its input and refreshes its display-error deadline. Replacing or resending an in-flight request is rejected; a confirmed request ID cannot be prepared again. `defaultUserMessageParams` leaves optional fields omitted. Sending retains an explicit `userMessageId` or generates a distinct persisted-message UUID, available in the observed entry. An omitted `userMessageSource` defaults to `api` on the wire; extensions cannot replace declared message fields or the target session.

A literal `accepted: true` reply waits for a matching session/request `create_message`; the confirmation may arrive before the reply. Legacy result objects return unchanged and do not themselves confirm an overlay. The command has one thirty-second RPC/confirmation budget and does not await the agent turn. Ordinary `sendPrompt`/`sendEvents` retain their existing turn semantics.

The independent display-error deadline defaults to twenty seconds; preparation accepts microseconds, including zero, and rejects negative values. Expiry retains a `SubmissionFailed SubmissionTimedOut` entry without interrupting its RPC. `getSessionState` evaluates expiry, and `waitSessionStateChange` wakes for state changes or expiry without retaining a background timer when no observer is waiting. Intermediate snapshots may coalesce. Run waits and submissions outside serial notification callbacks; the getter and local confirmation/cancellation operations do not wait for future intake.

`cancelOptimisticSubmission` removes one overlay; `cancelSessionOptimisticSubmissions` atomically removes a session's overlays and returns their count. Neither cancels or replays RPCs. `confirmOptimisticSubmission` records an externally established confirmation locally; it neither manufactures a message nor settles a pending RPC. Unknown explicit confirmations are no-ops. Detachment removes only the current lease's session overlays. Caller cancellation removes the overlay and preserves its exception; known protocol failures retain a typed error entry. A later valid echo remains authoritative after cancellation or timeout.

Validated loads reconcile by a fresh explicit persisted-message ID, not matching text or client/daemon clock proximity. This deliberately avoids the published SDK's ambiguous text heuristic. Unknown-ID preparations cannot be inferred from history until sending establishes an ID. Loaded, created and retained streaming messages are available through the conversation and progress views below; queue coordination is described in the queue section. State `Show` is redacted, but explicit inputs/messages remain sensitive. See [submission contracts and evidence](docs/development.md#optimistic-submission-delivery).

### Retained conversation views

`getSessionState` retains loaded messages and observed text, thinking, tool-call/result and retraction changes. The immutable `sessionInfo` receipt remains separate. Raw selectors expose last-known data; use `checkedSessionMessages` when consuming the current conversation, because a malformed recognized message event or a role-conflicting update marks it invalid until a validated load repairs the observation.

```haskell
import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Messages (FactoryDroidMessage)
import Factory.Droid.SessionState qualified as State

readConversation :: Daemon.DaemonConnection -> Text -> IO (Either State.MessageStateError [FactoryDroidMessage])
readConversation connection identifier =
  State.checkedSessionMessages <$> Daemon.getSessionState connection identifier

waitConversation :: Daemon.DaemonConnection -> Text -> State.SessionState -> IO (State.SessionState, Either State.MessageStateError [FactoryDroidMessage])
waitConversation connection identifier previous = do
  current <- Daemon.waitSessionStateChange connection identifier previous
  pure (current, State.checkedSessionMessages current)
```

This example is compiled, not executed. The model is updated before ordinary session callbacks observe the corresponding event. Waits remain caller-owned and must not run on serial notification intake. Native block-address tracking keeps text and thinking distinct, including repeated or non-integral wire indices; newly encountered provisional blocks append without allocating gaps. A full message/load replaces provisional content and resets those addresses. It does not retract already delivered stream-callback text.

Provisional message timestamps are client observations, not daemon persistence times. Thinking duration uses monotonic elapsed time when no explicit duration is supplied; explicit zero and false capability metadata remain meaningful. Completion changes streaming metadata without inventing text. Tool results target tool-role messages; unmatched results remain in `orphanToolMessages` until a matching assistant arrives. `pendingToolCalls` represents calls awaiting an assistant identity without manufacturing a message ID. These views do not execute tools or establish successful execution.

`sessionMessages`, `sessionRecentMessages` and `sessionMessagesByRole` provide retained ordering and selection. Parent repair removes self-links/cycles; equal-time leaf ties use portable UTF-16 order rather than host-locale collation. Pure removal/truncation changes only the supplied model, not remote history; truncation uses a nonnegative count and preserves pending submissions. Hook and progressive display views are described below; working-state observation remains in `SessionReadiness`. See [conversation-state evidence](docs/development.md#conversation-state-work-in-progress).

### Pure message helpers

Message-chain and display helpers also operate directly on typed lists, without a connection or mutable session model:

```haskell
module MessageHelpersExample (displayHistory, hookRows) where

import Factory.Droid.Schema.Messages (FactoryDroidMessage)
import Factory.Droid.SessionState
  ( filterDisplayMessages,
    orderMessagesByParentChain,
    persistedHook,
    repairMessageParents,
  )

displayHistory :: [FactoryDroidMessage] -> [FactoryDroidMessage]
displayHistory = filterDisplayMessages . orderMessagesByParentChain . repairMessageParents

hookRows :: [FactoryDroidMessage] -> [FactoryDroidMessage]
hookRows = filter persistedHook
```

Repair preserves the first parent for duplicate IDs, cuts self-links/cycles and retains unresolved parent IDs. Ordering follows the newest leaf's chain, then disconnected history; unlinked histories use stable timestamps, while linked equal-time leaf IDs use the documented UTF-16 rule. These transformations retain the supplied payloads and extensions apart from the intended parent/content changes.

`persistedHook` requires a nonempty event name and present commands/status. Names are not trimmed; whitespace names and present empty command lists qualify. It does not establish visibility, role or execution authority. Apply `filterDisplayMessages` separately to exclude hidden/LLM-only messages and strip system tags from non-assistant text. Only affected text is trimmed; empty messages remain when they contain persisted-hook metadata. Typed codecs reject malformed message/hook fields rather than reproducing arbitrary JavaScript truthiness or replacing malformed content with an empty list.

### Todos and tool progress

`sessionTodos` exposes the selected normalized `TodoWrite` list. Live updates require a non-error result for a tracked call; an older late result cannot replace a newer accepted list. Reload reconstructs the latest successful list, falling back to the latest unfailed pending list, and retains pending-call correlation. Empty or invalid todo updates do not erase the selected list. `parseTodos` accepts the published textual/checklist, string-array and object-array forms; object rows are validated before automatic numbering. Numeric values in mixed string arrays follow JavaScript display coercion, without changing exact numbers in retained tool inputs.

```haskell
import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Notifications (ToolProgressUpdate)
import Factory.Droid.SessionState qualified as State

readProgress :: Daemon.DaemonConnection -> Text -> IO (Either State.MessageStateError (Maybe [State.TodoItem], [ToolProgressUpdate]))
readProgress connection identifier = do
  state <- Daemon.getSessionState connection identifier
  pure $ do
    _ <- State.checkedSessionMessages state
    pure (State.sessionTodos state, State.allToolProgress state)
```

The example is compiled, not executed. `sessionToolProgress` retains updates by tool-use ID; `allToolProgress` orders them stably by reported timestamp, with omission treated as zero. Consecutive duplicates ignore timestamp only. `sessionToolPhases` advances monotonically, retaining the first terminal interpretation rather than promoting uncertain settlement to execution evidence. `sessionRetry` records retries and clears on the specified progress/working/turn/error events; it does not implement a retry policy.

A reported tool result schedules progress cleanup after sixty seconds. A distinct later update cancels that cleanup; a timestamp-only duplicate does not. `getSessionState` and `waitSessionStateChange` share the existing monotonic expiry path with optimistic submissions, without a persistent timer worker. Reload clears transient progress, phase and retry state. Pure `clearToolProgress` modifies only the supplied model. Inferred text/thinking/tool/permission/error transitions update the existing `SessionReadiness` owner; they are not a second working-state cache or proof of remote idleness. See [todo/progress verification](docs/development.md#todo-and-tool-progress-state).

### Hooks and progressive selection

`sessionHooks` retains typed start metadata and reported completions in observation order. Repeated starts replace the same hook without changing its position; unknown completions are no-ops. `HookOutcome` distinguishes `HookRunning`, `HookReported` and `HookLeaseExpired`. Expiry uses the reported command timeout, with a sixty-second minimum and thirty-second grace; a later completion report remains authoritative. Expiry is not reported success and does not execute, stop or retry the hook. Very large timeout values remain exact; beyond a native wait horizon, the lease is checked when state is observed rather than narrowed into a timer.

```haskell
import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.SessionState qualified as State

readDisplay :: Daemon.DaemonConnection -> Text -> IO (Either State.MessageStateError ([State.DisplayEntry], [State.HookObservation]))
readDisplay connection identifier = do
  state <- Daemon.getSessionState connection identifier
  pure $ do
    entries <- State.checkedSessionDisplay state
    pure (entries, State.visibleSessionHooks state)

showMore :: Daemon.DaemonConnection -> Text -> IO Bool
showMore = Daemon.expandSessionDisplay
```

This example is compiled, not executed. Display entries distinguish observed messages, optimistic submissions and tool calls awaiting an assistant identity. Pending entries keep registration order. The display projection excludes hidden/model-only messages and hidden pending inputs; non-assistant text drops complete system-reminder/notification blocks, while assistant quotations remain intact. Empty persisted hook rows remain meaningful. Raw message, input and hook fields remain explicitly accessible and sensitive.

Progressive selection defaults to a thirty-entry window for an initial history larger than thirty messages. Expansion doubles the window, returning whether it remains limited; it reveals cached data without fetching remote history. `Daemon.setSessionProgressiveDisplay` enables/disables this policy. `Daemon.setSessionDisplayCutoff` changes local cutoff metadata; with progressive selection enabled, it prunes cached rows before the boundary while retaining the latest todo-bearing message and its result context. Hidden counts describe actual pruned cache rows, not the peer's reported removal count. Clearing the cutoff resets its count but does not restore data until a reload. `SessionCompactedEvent` uses the same path; a null boundary does not clear an existing cutoff.

`sessionHydrationFloor` identifies the settled boundary from the last load, considering pending tool results and persisted executing hooks. It is not a remote execution receipt. `visibleSessionHooks` honors an explicit hidden flag in either the start or completion report. These are native data-selection operations, not a browser renderer, and they do not change remote history, trust or permissions. See [combined state verification](docs/development.md#session-message-state-closure).

### Resume snapshots and connection access

A successful `withResumedSession` publishes the handle only after load and validation. `daemonLoadedState (sessionInfo session)` contains an immutable `Schema.Daemon.Session.LoadedSessionState`: messages/title, settings, optional pagination/busy/working-state fields, queued submissions and a typed optional mission snapshot. Newly initialized sessions have no loaded-state receipt. This projection retains other report fields as extensions; it is not a complete load-result codec or live queue/store.

```haskell
module LoadExample (loadQueue) where

import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Control (QueuedUserMessage)
import Factory.Droid.Schema.Daemon.Session (LoadedSessionState (..))

loadQueue :: Daemon.DaemonOptions -> Text -> IO (Maybe [QueuedUserMessage])
loadQueue options identifier =
  Daemon.withResumedSession options identifier $ \session ->
    pure (Daemon.daemonLoadedState (Daemon.sessionInfo session) >>= loadedQueuedMessages)
```

This example is compiled, not executed. Queued entries carry their original request ID and a reused `AddUserMessageParams` input. Omitted queues differ from empty queues; malformed known snapshot/queue fields prevent handle publication. The reported working state is not the SDK handle's lease status. Outer receipt displays are redacted, but explicit payloads/JSON and reused types are not universally sanitized.

Inside the session callback, `Daemon.sessionConnection session` borrows its existing authenticated connection for queue resolution and other resource operations. It creates no socket, dispatcher or new ownership scope; using it after the original physical connection scope exits fails. A borrowed attachment does not own that connection, so detaching it leaves the original owner's connection usable. Explicit reloads and coalesced readiness checks are described above; broader load options and live queue/event-replay managers remain separate work. See [load verification](docs/development.md#daemon-loaded-state-and-borrowed-connection-delivery).

### Worktree session creation

`daemonWorktree = Just True` requests daemon-owned worktree creation/reuse; `Just False` explicitly disables it, while `Nothing` leaves the daemon default in effect. `daemonWorktreeDirectory` is an optional remote placement override; empty and omitted values remain distinct. A non-Git cwd can yield no worktree. These fields are initialization-only; even explicit false/empty values are rejected on resume before connection.

```haskell
module WorktreeExample (createInWorktree) where

import Factory.Droid.Daemon qualified as Daemon

createInWorktree :: Daemon.DaemonOptions -> IO Daemon.DaemonSessionInfo
createInWorktree options =
  Daemon.withSession (options {Daemon.daemonWorktree = Just True}) $ \session ->
    pure (Daemon.sessionInfo session)
```

Compilation does not execute this example. Calling it asks the remote daemon to create/reuse a worktree and session; ordinary scope exit disconnects without deleting either. `sessionInfo` is an immutable attachment receipt, not live cwd observation: it retains authenticated identity, initial effective cwd and optional `Schema.Session.SessionWorktreeInfo`. The latter preserves branch/path, creation-versus-reuse flag and optional root/lifecycle/parent metadata. It differs from saved `SessionWorktreeMetadata`. On resume, reported cwd wins; a legacy reported worktree path is used only if cwd is absent. Without either, location is `Nothing`, not a guessed local path.

The baselined SDKs do not expose the newer managed-worktree/profile/cleanup methods found in the supplied schema corpus. Their existing codecs are retained, without inventing public operations. Global worktree defaults are available through the default-settings APIs below; live cwd observations are separate from the immutable creation receipt. See [worktree evidence](docs/development.md#daemon-worktree-session-delivery).

### Session settings and titles

Owned daemon sessions expose `getSettings`, `updateSettings` and `renameSession`. These are distinct from global defaults. Updates reuse `Schema.Settings.UpdateSessionSettingsParams`, including omission/null resets, false/empty values and tool policy; title changes preserve the actual legacy `SuccessResult` flag.

```haskell
module SessionControlsExample (configure) where

import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.RPC (SuccessResult)
import Factory.Droid.Schema.Settings
  ( UpdateSessionSettingsParams (..)
  , emptySettingsUpdate
  )

configure :: Daemon.DaemonSession -> Text -> IO SuccessResult
configure session title = do
  _ <- Daemon.updateSettings session
    (emptySettingsUpdate {updateSettingsCompactionThresholdEnabled = Just False})
  Daemon.renameSession session title
```

Compilation does not execute the mutations. ACKs wait for a validated `settings_updated` or `session_title_updated` event matching the owned session and new RPC ID; legacy replies return directly. One thirty-second budget covers the exchange. Raw `Client.updateDaemonSessionSettingsRaw`/`renameDaemonSessionRaw` bindings expose immediate replies and caller-owned deadlines instead.

`getSettings` reads the shared last-observed view without an RPC or a future-event wait. Inside a session observer it includes that notification; legacy replies alone do not update it. No requested settings or title are optimistically cached. **Do not call ACK-waiting setters from serial notification callbacks**: they can wait for later notifications on that same dispatcher. The getter is safe there; run setters from normal caller/stream-callback threads.

The existing mutation lease/error policy is retained. Ordinary RPC-result errors remain explicit; uncertain cancellation, transport failure or malformed deferred completion invalidates the handle and attempts interruption. See [owned-control verification](docs/development.md#daemon-owned-settings-and-title-delivery).

### Skills, commands and context

`Daemon.listSkills`, `listCommands` and `getContextBreakdown` query an owned session under read-only leases. They reuse the `Schema.Discovery` and `Schema.Context` reports; each call goes to the daemon rather than a local cache.

```haskell
module DiscoveryExample (inspect) where

import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Context (GetContextBreakdownResult)
import Factory.Droid.Schema.Discovery (ListCommandsResult, ListSkillsResult)

inspect :: Daemon.DaemonSession -> IO
  (ListSkillsResult, ListCommandsResult, GetContextBreakdownResult)
inspect session =
  (,,) <$> Daemon.listSkills session
       <*> Daemon.listCommands session
       <*> Daemon.getContextBreakdown session
```

This example is compiled, not executed. Paths, skill content/resources, disabling sources and command executable flags remain metadata: the SDK neither reads those paths nor executes their contents. Empty lists, optional `projectAvailable`, and exact context numbers—including reported zero/fractional values—are retained without recomputing totals.

`Daemon.setSkillDisabled` accepts `SetSkillDisabledParams`; false requests enablement, and an omitted settings level remains distinct from explicit user/project scope. The returned success flag is preserved rather than treated as unconditional success. This is a daemon-side mutation under the existing lease/invalidation policy. Read cancellation leaves the session usable while its channel is healthy; uncertain mutation cancellation requests interruption and invalidates it.

Low-level `Client.listDaemonSkills`, `listDaemonCommands`, `getDaemonContextBreakdown` and `setDaemonSkillDisabled` retain caller IDs/deadlines. Ordinary query replies can be awaited from serial notification callbacks; they do not require future notification completion. The baselined SDKs have no `daemon.list_models`, `daemon.list_tools` or `daemon.get_context_stats` RPCs; this port does not invent them. Model availability remains in existing reports. See [discovery verification](docs/development.md#daemon-discovery-and-context-delivery).

## Daemon default settings

`Daemon.getDefaultSettings` reads daemon-global defaults through `DaemonConnection`, without loading a session or reading local settings files. `Daemon.updateSessionDefaults` accepts `Schema.Daemon.Settings.UpdateSessionDefaultsParams` and preserves both the returned success flag and daemon-resolved defaults.

```haskell
module DefaultsExample (resetWorktreeDefaults) where

import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Daemon.Settings
  ( UpdateSessionDefaultsParams (..)
  , UpdateSessionDefaultsResult
  , emptySessionDefaultsUpdate
  )

resetWorktreeDefaults :: Daemon.DaemonConnection -> IO UpdateSessionDefaultsResult
resetWorktreeDefaults connection =
  Daemon.updateSessionDefaults connection
    (emptySessionDefaultsUpdate
      { updateDefaultsRunInWorktree = Just Nothing
      , updateDefaultsWorktreeDirectory = Just Nothing
      })
```

This example is compiled, not executed: calling it changes remote defaults. `Nothing` omits an update; nullable fields use `Just Nothing` to reset/inherit and `Just (Just value)` to set an explicit value. False, empty strings/maps/lists and fractional numbers are retained. `updateDefaultsSubagentInheritTiers` requests tier resets; the SDK does not calculate inheritance locally. Prefer `compactionModel` over the retained deprecated `compactionModelMode`.

Reports include available models, management/source metadata, subagent/mission defaults, spec-save paths and resolution history. When present, `defaultsAvailableAutonomy` is the daemon's selectable-level list; do not construct a picker from the full enum or infer permission from missing metadata. Report fallbacks are field-local; malformed strict fields produce `RpcInvalidResult`. Updates validate enums strictly. Ordinary calls use the existing thirty-second deadline; low-level `Client.getDaemonDefaultSettings`/`updateDaemonSessionDefaults` retain caller-owned deadlines. Cancellation stops waiting, not an already applied remote write. See [default-settings contracts and evidence](docs/development.md#daemon-default-settings-delivery).

### Custom-model configuration

`Daemon.listCustomModels`, `upsertCustomModel` and `deleteCustomModel` operate on daemon-global configuration through `DaemonConnection`, without loading a session. Their types live in `Schema.Daemon.Settings`. Listings preserve invalid rows (`isValid = False`), raw provider model names, key-presence/mask metadata and returned ordering; these are not normalized model-catalog entries.

```haskell
module CustomModelExample (saveGuarded) where

import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Daemon.Settings
  ( CustomModelSummary (..)
  , UpdateCustomModelsResult
  , UpsertCustomModelParams (..)
  )

saveGuarded :: Daemon.DaemonConnection -> CustomModelSummary
  -> UpsertCustomModelParams -> IO UpdateCustomModelsResult
saveGuarded connection current params =
  Daemon.upsertCustomModel connection
    (params
      { upsertCustomModelIndex = Just (customModelRawIndex current)
      , upsertCustomModelExpectedModel = Just (customModelId current)
      })
```

This example is compiled, not executed; calling it mutates remote configuration. `defaultUpsertCustomModelParams` accepts validated `NonEmptyText` model/provider values and omits optional fields. An absent index creates; a supplied index edits. Preserve the raw reported model name as the edit/delete guard, not a normalized catalog ID. The daemon enforces the guard; the SDK does not retry or reindex after rejection.

On edit, an omitted API key is forwarded absent so the daemon retains it. An explicit empty key remains an empty string; the SDK does not invent clearing semantics. API-key null is invalid. `maxOutputTokens` and `noImageSupport` alone admit nullable resets, distinct from omission and explicit zero/false. Other omitted fields follow daemon semantics, not local merging. Wire indices/limits use `Scientific`; the daemon decides which indices/values are operationally valid.

Actual success flags and returned rows are preserved, including false success. Record `Show` is redacted; explicit fields and JSON remain sensitive. No local settings/credential lookup, provider request or optimistic cache is added. Cancellation stops waiting, not an applied configuration change. Low-level `Client.listDaemonCustomModels`, `upsertDaemonCustomModel` and `deleteDaemonCustomModel` retain caller deadlines. See [custom-model verification](docs/development.md#daemon-custom-model-delivery).

## Daemon management

Management operations use `DaemonConnection` and `Schema.Daemon.Management` records, plus the existing `Schema.RPC.CommandAck` for logout. They act on the daemon host only when explicitly invoked; they are not local management implementations.

| Operation | Result / boundary |
| --- | --- |
| `logout` | Send `daemon.logout` with `{}` and require literal `accepted: true`; acknowledgment is not proof of completed revocation or disconnection. Never sent by scope cleanup. |
| `triggerUpdate` | Preserve `triggered` and optional message; triggering is not update completion or a verified new version. |
| `installSshKey` | Accept `NonEmptyText` public-key input and preserve `installed`; SSH format/authentication policy belongs to the daemon. No local key file is read or written. |
| `getProxyToken` | Return an opaque bearer token. The request omits `params`, rather than sending `{}` or null; no expiry, renewal or cache is inferred. |
| `startRelay` | Return relay URL/computer ID without connecting this SDK to that relay. |
| `stopRelay` | Require a strictly empty reply (`RelayStopResult`); unexpected fields are rejected. |
| `getRelayStatus` | Preserve connected state and optional URL/client count/computer ID without filling absent values from a cache. |

```haskell
module ManagementExample (inspectRelay, requestLogout) where

import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Daemon.Management (RelayStatus)
import Factory.Droid.Schema.RPC (CommandAck)

inspectRelay :: Daemon.DaemonOptions -> IO RelayStatus
inspectRelay options = Daemon.withConnection options Daemon.getRelayStatus

requestLogout :: Daemon.DaemonConnection -> IO CommandAck
requestLogout = Daemon.logout
```

The example is compiled, not executed. Logout, update, SSH installation and relay start/stop are explicit remote mutations; scope cleanup does not issue them. `onRelayStatusChanged` uses the existing connection observer/error/unsubscribe rules. Register before starting/stopping if early notifications matter; ordinary status queries are safe inside callbacks. Malformed selected events are reported, not silently converted into status.

`Client.logoutDaemon` supplies the same logout operation with caller-owned metadata/deadlines and no parameters argument: the request's closed empty object cannot be replaced by caller extensions. `Daemon.logout` uses thirty seconds and returns the existing `CommandAck`, including extensions. Missing, false or null `accepted` is invalid. Cancellation stops waiting, not remote revocation; an early disconnect stays a channel error. The daemon facade publishes authentication/pending/deferred clearing before returning an accepted result; rejection preserves that state. Scope ownership is unchanged, and `connectionUser` remains the original authentication receipt. See [pending and deferred interactions](#pending-and-deferred-interactions) for the controller contract.

Tokens, key strings, URLs and explicit JSON remain sensitive. Management record displays redact their fields; the reused `CommandAck` retains its existing display behavior and does not sanitize extensions. Low-level management bindings preserve caller metadata/deadlines, including reserved-field protection. High-level calls use the usual thirty-second budget; cancellation cannot undo remote work. Actual relay transport/authentication remains separate. See [management verification](docs/development.md#daemon-management-delivery).

## Daemon crons

The six cron operations use the existing authenticated `DaemonConnection`; the daemon owns scheduling and execution. `Schema.Daemon.Cron` supplies the operation records, with distinct session-prompt and new-session variants so that scope, payload and inactive-session policy remain consistent.

| Operation | Contract |
| --- | --- |
| `listCrons` | `ListCronsParams` has optional session and inactive filters; `defaultListCronsParams` sends `{}`. Results retain full cron records. |
| `createCron` | `CreateSessionCron` or `CreateRootCron` returns the reported record. `defaultCreateCronOptions` selects `CronTool` and omits immediate execution and explicit run policy. |
| `updateCron` | A partial `UpdateCronParams` accepts only activate/pause status changes. The result requires `cron`, but its value can be null when no record is returned. |
| `deleteCron` | `DeleteCronParams` retains an optional session qualifier and the actual `deleted` flag, including false. |
| `holdSessionCrons` | `HoldSessionCronsParams` supplies a session and unmodified reason; the result reports a nonnegative integer count. |
| `resumeSessionCrons` | Reuses `Schema.Session.SessionIdParams` and reports a nonnegative integer count. |

```haskell
module CronExample (createReminder, pauseReminder) where

import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Daemon.Cron

createReminder :: Daemon.DaemonConnection -> Text -> Text
  -> CronText -> CronText -> IO CreateCronResult
createReminder connection sessionId cwd expression prompt =
  Daemon.createCron connection
    (CreateSessionCron
      (defaultCreateCronOptions (CronSchedule expression True mempty))
      (CronSessionScope sessionId cwd mempty)
      (SessionCronPayload prompt mempty mempty))

pauseReminder :: Daemon.DaemonConnection -> Text -> IO UpdateCronResult
pauseReminder connection identifier =
  Daemon.updateCron connection
    ((defaultUpdateCronParams identifier) {updateCronStatus = Just PauseCron})
```

The example is compiled, not executed. Construct `CronText` with `mkCronText`: it applies ECMAScript whitespace trimming and rejects empty results, but does not interpret cron syntax. Only expressions and prompts receive that normalization; identifiers, paths, reasons and opaque timestamps retain their spelling. Stored schedules require literal UTC. Missing optional values, explicit false and empty strings remain distinct from invalid nulls; an omitted update payload differs from `Just (CronPayloadPatch Nothing)`, which sends a closed empty patch.

Creation options represent an explicit run-policy declaration by `Just` extension fields; `Nothing` omits it. The constructor fixes its policy to hold for session prompts or background execution for new-session prompts. Stored session records additionally carry `storageDir`; their scope does not imply local filesystem ownership. Canonical open-record extensions are retained, while the closed prompt patch rejects undeclared fields. Cron-specific displays are redacted; explicit fields and JSON remain sensitive.

`onCronStateChanged` observes `daemon.cron.state_changed` through the existing dispatcher. Its created/updated/deleted reason and ID list are required; an optional record list distinguishes absence from an empty snapshot. Register before mutation to observe early events, keep callbacks brief, and restrict reentrant work to ordinary queries. Unsubscribe is idempotent. No local cron cache, timer, new session, automatic retry or rollback is created. High-level calls use thirty seconds; corresponding `Client` bindings retain caller IDs, envelopes and deadlines. Cancellation cannot undo remote effects. See [cron verification](docs/development.md#daemon-cron-delivery).

## Software Factory

Software Factory operations use the existing authenticated daemon connection. `Schema.Daemon.SoftwareFactory` contains public workstream, signal, change, activity and event projections, not the backend's internal storage rows. Requests do not create a local worker pool, run activities, render dashboard content or maintain a resource cache.

| Operation | Contract |
| --- | --- |
| `sfListWorkstreams` | Optional state filter; returns workstream rows. `SfListWorkstreamsParams Nothing mempty` sends `{}`. |
| `sfGetWorkstream` | `SfWorkstreamTarget` carries an opaque ID or slug. No match omits the result field; null is invalid. |
| `sfCreateWorkstream` | Title and goal are required; `defaultSfCreateWorkstreamParams` leaves all other options absent. |
| `sfUpdateWorkstream` | Partial update from `defaultSfUpdateWorkstreamParams`; omitted fields are not synthesized. Only the supplied icon-clear field accepts explicit null. |
| `sfDeleteWorkstream` | Returns the pre-deletion row, reported automation counts and actual directory-deletion flag. |
| `sfListSignals`, `sfListChanges` | Share `SfListParams` with their respective status types; filters and limits are optional. |
| `sfListActivities` | Optional workstream/change/review-queue filters and limit; results include change context. |
| `sfResolveActivityReview` | Approved, declined or commented decision, optional comment, and an optional reported follow-up activity. |
| `sfListEvents` | Optional workstream, unread-only and limit filters. |
| `sfMarkEventsRead` | Omitted IDs mean all unread events for the workstream; explicit empty IDs remain an empty list. |
| `sfMarkEventsUnread` | IDs are required, including an explicitly empty list. |

```haskell
module SoftwareFactoryExample (lookupWorkstream, createWorkstream, commentOn) where

import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Daemon.SoftwareFactory

lookupWorkstream :: Daemon.DaemonConnection -> Text -> IO SfGetWorkstreamResult
lookupWorkstream connection identifier =
  Daemon.sfGetWorkstream connection (SfWorkstreamTarget identifier mempty)

createWorkstream :: Daemon.DaemonConnection -> Text -> Text -> IO SfWorkstreamResult
createWorkstream connection title goal =
  Daemon.sfCreateWorkstream connection (defaultSfCreateWorkstreamParams title goal)

commentOn :: Daemon.DaemonConnection -> Text -> Text -> IO SfResolveActivityReviewResult
commentOn connection identifier comment =
  Daemon.sfResolveActivityReview connection
    (SfResolveActivityReviewParams identifier SfComment (Just comment) mempty)
```

This example is compiled, not executed. Identifiers, text and timestamps retain their spelling; no trimming, date parsing or URL/HTML execution is inferred. Mutation concurrency/port values, list limits and deletion/mark counts retain the source contract's `Scientific` number domain; reported workstream concurrency/ports are integers. False, empty and absent values are distinct. Review follow-ups are data, not newly owned activity execution.

Entry projections omit internal signal payloads and the excluded worker/claim/retry fields; activities retain their declared session and change-context fields. Known newer access/approval/template/dashboard metadata fields are optional. Activity kinds additionally support the supplied `change_review` variant. The supplied nullable update icon uses `Nothing` for omission, `Just Nothing` for clearing, and `Just (Just text)` for a value; reported icons remain nonnull when present. These additions do not imply version negotiation. The retained `userId` update field is deprecated and ignored by the baselined store; no effective ownership transfer is asserted.

Corresponding low-level bindings are named `Client.listDaemonSfWorkstreams`, `getDaemonSfWorkstream`, and so forth. They retain caller IDs, envelopes and deadlines; high-level calls use thirty seconds. Cancellation stops waiting, not remote changes, and never triggers replay or compensation. Displays are redacted, but explicit public content, connector JSON and outer extensions remain sensitive. Hydration/publication methods found only in newer schemas are not baselined SDK capabilities. See [Software Factory verification](docs/development.md#software-factory-delivery).

## Daemon session controls

`Daemon.getRewindInfo`, `executeRewind`, `compactSession`, `forkSession`, `killWorkerSession` and `closeSession` operate through `DaemonConnection` on explicit remote targets. They reuse `Schema.Control` records except for `Schema.Daemon.Session.DaemonCloseSessionParams`. Returned successor IDs and restoration/removal counts are data: no automatic load, filesystem operation or local replacement transition is performed.

```haskell
module DaemonControlExample (inspect, fork) where

import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Control
  ( GetRewindInfoParams (..), GetRewindInfoResult
  , ForkSessionParams (..), ForkSessionResult )

inspect :: Daemon.DaemonConnection -> Text -> Text -> IO GetRewindInfoResult
inspect connection identifier message =
  Daemon.getRewindInfo connection (GetRewindInfoParams identifier message mempty)

fork :: Daemon.DaemonConnection -> Text -> IO ForkSessionResult
fork connection identifier =
  Daemon.forkSession connection identifier (ForkSessionParams Nothing Nothing mempty)
```

This example is compiled, not executed. Use the returned IDs explicitly when attaching to a successor; these connection-level operations do not apply the local SDK's replacement/rollback policy. Existing owned-session lifecycle notifications still invalidate that connection. Ordinary scope exit sends no close request.

`killWorkerSession` takes the orchestrator ID and `KillWorkerSessionParams` identifying the worker; it does not terminate a local process. `closeSession` takes `defaultDaemonCloseSessionParams identifier`, optionally updated with `daemonClosePreserveEmptyDraft`. That flag is from the newer supplied schema: absence keeps the older request shape, false is retained, and null is invalid. This is not a negotiated compatibility guarantee.

Compaction uses a 240-second high-level budget; other controls use thirty seconds, and all low-level `Client` deadlines remain caller-owned. Cancellation stops waiting but cannot undo remote work. The 240-second value is source/configuration-checked, not a four-minute elapsed-time test. Reused control records retain their existing `Show` behavior; paths, titles and tags are not sanitized. See [control verification](docs/development.md#daemon-session-lifecycle-control-delivery).

## Diagnostic reports

`Daemon.submitBugReport` and `Client.submitDaemonBugReport` send an explicit daemon report request for the supplied session ID. `Client.submitBugReport` remains the local counterpart. All reuse `SubmitBugReportParams`/`SubmitBugReportResult`; comments, optional client logs and optional source metadata are caller-provided.

```haskell
module DiagnosticExample (reportProblem) where

import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Control
  ( SubmitBugReportParams (..), SubmitBugReportResult )

reportProblem :: Daemon.DaemonConnection -> Text -> Text
  -> IO SubmitBugReportResult
reportProblem connection identifier comment =
  Daemon.submitBugReport connection identifier
    (SubmitBugReportParams comment Nothing Nothing mempty)
```

The example is compiled, not executed. Invoking these operations can upload diagnostic content. The SDK does not read local log files, collect environment/platform data, infer source attribution or sanitize caller text. Review the content before submission; daemon-side collection and handling are outside this API's guarantees. Cancellation stops waiting and does not undo an upload or trigger a retry.

`BugReportSource` retains the supplied JSON Schema's code-point bounds. Submission additionally runs `Schema.Sources.validateBugReportSource`, matching the baselined CLI/TypeScript UTF-16 limits: 100 units for version/CLI version/OS version, 32 for platform/architecture. Invalid source raises `BugReportSourceTooLong field limit` before the report request is sent, without including the value. For example, 50 emoji fit a 100-unit field, but 51 do not. No text is truncated or normalized; the standalone codec and generic `Client.call` do not imply this runtime preflight.

Daemon reports use the ordinary thirty-second budget; low-level deadlines remain caller-owned. Existing report/source record displays are not redacted. Offline peers verify payloads and error behavior, not real upload, persistence or live interoperability. See [diagnostic verification](docs/development.md#diagnostic-submission-and-runtime-limits).

## Daemon Git inspection and checkout

`listGitBranches`, `checkoutGitBranch`, `getGitBranchDivergence`, `getGitDiff` and `resolvePullRequestStatuses` use `DaemonConnection` and `Schema.Daemon.Git` records. They request work from the daemon; they do not run local Git or provider CLIs.

```haskell
module GitExample (inspectDiff) where

import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Daemon.Git (GitDiffResult, defaultGitDiffParams)

inspectDiff :: Daemon.DaemonSession -> IO GitDiffResult
inspectDiff session =
  Daemon.getGitDiff (Daemon.sessionConnection session)
    (defaultGitDiffParams (Daemon.sessionId session))
```

The example is compiled, not executed. Checkout preserves checked-out versus needs-resolution results, including an optional pull failure and missing-only untracked-file default. Stash/commit resolution is an explicit caller request, never an automatic SDK choice. Divergence distinguishes tracked nonnegative integer counts, no remote and unavailable; nullable current branch remains distinct from omission.

Diffs accept the baseline legacy bare report and normalize it to `GitDiffAvailable`. Missing committed/local/unstaged fields receive their declared defaults; explicit null or malformed fields fail. Unavailable results remain separate data, with invalid/missing reason values mapped only to the reference's unknown reason. The SDK does not parse/apply patches or recompute counts.

`mkPullRequestLookupBatch` permits zero through twenty lookups and rejects larger batches rather than silently truncating or fanning out. Optional invalid lookup reasons are discarded as specified; other fields stay strict. Nullable status is not a resolved `PullRequestNone`, and an omitted diff PR status means unresolved—not no PR. Branch, remote URL, provider and lifetime reports are retained for callers; no SDK cache, host inference, lookup retry or freshness policy is added. Cancellation cannot roll back checkout/provider work. See [Git verification](docs/development.md#daemon-git-inspection-delivery).

### Publishing, readiness and semantic diffs

`gitPush`, `gitCommit` and `createPullRequest` are explicit daemon-side publishing requests. Actual success flags and returned PR metadata are retained; the SDK does not run local Git/provider commands or infer ticket links, base branches or publication success.

`inspectMissionReadiness` reports repository/remote/empty state and an optional typed warning with levels one through five. Inspection does not acknowledge the warning or grant folder trust; `acknowledgeMissionReadinessWarning` is a separate explicit operation.

```haskell
module GitResourceExample (readCached) where

import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Daemon.Git
  ( SemanticDiffTarget (..), SemanticDiffCacheResult )

readCached :: Daemon.DaemonConnection -> Text -> Text
  -> IO SemanticDiffCacheResult
readCached connection current base =
  Daemon.getSemanticDiffCache connection
    (SemanticDiffTarget current base mempty)
```

The example is compiled, not executed. Cache misses preserve required null content/hash fields, distinct from empty strings. `saveSemanticDiffCache` is an explicit remote write. `generateSemanticDiff` returns opaque content, a truncation flag and a session ID as data; it does not adopt a session or save the result automatically. Its high-level budget is 180 seconds, matching the baseline; low-level deadlines remain caller-owned. This duration is source/configuration-checked, not a three-minute elapsed-time test.

These calls can cause real remote publication, warning acknowledgement or model work when invoked. Offline fixtures establish wire/error/cancellation behavior only. Cancellation stops waiting and does not roll back remote work. No new local cache, renderer, retry or policy engine is supplied. See [resource verification](docs/development.md#daemon-publishing-readiness-and-semantic-diff-delivery).

## Existing automations

`Daemon.listAutomations`, `runAutomation`, `pauseAutomation`, `resumeAutomation`, `getAutomationHistory`, `getAutomationVisual`, `renameAutomation` and `deleteAutomation` use `DaemonConnection` and `Schema.Daemon.Automation` records. The creation/configuration operations below complete the sixteen baselined automation RPCs.

```haskell
module AutomationExample (history) where

import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Daemon.Automation
  ( AutomationHistoryParams (..)
  , AutomationHistoryResult
  , defaultAutomationAddress
  )

history :: Daemon.DaemonConnection -> Text -> IO AutomationHistoryResult
history connection identifier =
  Daemon.getAutomationHistory connection
    (AutomationHistoryParams (defaultAutomationAddress identifier) Nothing Nothing)
```

This example is compiled, not executed. `defaultAutomationAddress` mirrors the baseline controller by supplying the legacy directory alias from its ID argument. Direct address records preserve an omitted alias or a distinct directory name. Do not conflate an entry's directory `id` with its optional backend `uuid`, or infer remote ownership solely from `computerId`; `machineId` is a separate report.

Listings retain invalid entries and optional pending setups. Status, schedule, reasoning, privacy and timestamp strings remain peer data; numeric history values use `Scientific`. Run returns preparation data—prompt, cwd/model and optional scaffold reminder—not proof that an agent session ran. The reminder is already embedded in the returned prompt and is not prepended again. Scaffold content/fingerprints, visual HTML and URLs are not written, rendered, fetched or executed by the SDK.

Pause/resume status and false/error results are preserved. Optional base paths, limits, offsets and visual session IDs are not guessed; low-level calls retain caller deadlines. Ordinary high-level calls use the existing thirty-second budget. Cancellation stops waiting and cannot undo remote work. New record displays redact content; explicit fields/JSON and reused types retain their own sensitivity. See [lifecycle verification](docs/development.md#daemon-automation-lifecycle-delivery).

## Automation creation and configuration

`createAutomation`, `forkAutomation`, `updateAutomationModel`, `updateAutomationPrivacy`, `updateAutomationPrompt`, `updateAutomationSchedule`, `applyAutomationConfig` and `updateAutomation` use the same daemon connection and typed automation/scaffold records.

```haskell
module AutomationConfigExample (clearModel) where

import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Daemon.Automation
  ( AutomationAddress, UpdateAutomationModelParams (..) )
import Factory.Droid.Schema.RPC (SuccessOrErrorResult)

clearModel :: Daemon.DaemonConnection -> AutomationAddress
  -> IO SuccessOrErrorResult
clearModel connection target =
  Daemon.updateAutomationModel connection
    (UpdateAutomationModelParams target Nothing)
```

The example is compiled, not executed. The dedicated model setter requires a `model` field: `Nothing` sends null to clear it, rather than omitting it. Other setters preserve raw empty strings. Full apply/update requests require name, schedule and prompt through `AutomationConfiguration`; they are not arbitrary partial patches. Empty reasoning effort and working directory values are forwarded for the daemon's clearing semantics. `defaultCreateAutomationParams` and `defaultAutomationConfiguration` omit optional values rather than invent defaults.

Creation/forking can cause daemon-side scaffolding or first-run work; `skipFirstRun` is an explicit caller option, not silently forced by the SDK. Prompt, skill/fingerprint, privacy and creator data remain explicit. Actual returned IDs, false/error outcomes and optional apply-failure reasons are preserved. The SDK does not build files, start a separate agent, reconcile config locally or retry a mutation.

The privacy setter alone omits `factoryProtocolVersion`, matching the baselined SDK. It preserves request ID, other metadata and deadlines; caller body extensions cannot reintroduce that reserved field. Subsequent calls retain their configured version. This is a method contract, not version negotiation or a fallback. No real automation, privacy or model change is exercised by offline verification. See [configuration verification](docs/development.md#daemon-automation-configuration-delivery).

## Plugins and marketplaces

Owned daemon sessions expose the baseline plugin/marketplace operations using `Schema.Daemon.Plugin` data and the existing request leases.

| Operations | Inputs / results |
| --- | --- |
| `listAvailablePlugins`, `listInstalledPlugins` | Available/installed metadata; installed scope is optional, with empty distinct from omitted. |
| `installPlugin`, `uninstallPlugin`, `setPluginEnabled` | Explicit marketplace/name or plugin target/scope; actual success, optional IDs and error strings are retained. |
| `updatePlugin` | Optional plugin ID/scope; `UpdateBatch` retains every per-plugin success/error entry. |
| `listMarketplaces`, `addMarketplace`, `removeMarketplace`, `updateMarketplace` | Metadata, typed source variants, explicit removal name and optional update name; batch results are not collapsed into one success. |

```haskell
module PluginExample (inspect) where

import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Daemon.Plugin
  ( ListInstalledPluginsResult, ListMarketplacesResult )

inspect :: Daemon.DaemonSession -> IO
  (ListInstalledPluginsResult, ListMarketplacesResult)
inspect session =
  (,) <$> Daemon.listInstalledPlugins session Nothing
      <*> Daemon.listMarketplaces session
```

The example is compiled, not executed. Source variants are GitHub, URL, local and Git-subdirectory. `mkMarketplaceGitRef` trims ECMAScript whitespace and rejects an empty result; it does not impose Git's ref-name grammar. `mkMarketplaceGitSha` requires forty ASCII hex digits, accepts either case and preserves that case. Paths, URLs, scopes and timestamp strings remain daemon data, not locally interpreted paths or clocks.

Input and reported sources are different types. Their source objects project declared fields as the baseline SDK does; a reported local source has no path and discards unexpected path/ref data. Other records retain open-schema extensions. New record displays redact content, while explicit JSON/fields and reused `SuccessOrErrorResult` displays retain their existing sensitivity.

Do not default missing `active`, `managed`, `reason`, `provisionedBy` or `removable` fields into policy. In particular, removal UI/automation must require explicit removable permission rather than infer it from absent provenance. Explicit mutation methods forward the request; the daemon enforces policy. Empty optional update targets are not expanded through a local cache. No Git clone, filesystem installation, plugin execution, optimistic update or retry is performed by the SDK. Cancellation follows the existing read-versus-mutation policy and does not undo external installation work. See [plugin verification](docs/development.md#daemon-plugin-and-marketplace-delivery).

## Daemon catalogs, history and archival

`Daemon.withConnection` authenticates without initializing or loading a session. Its catalog operations return wire data, not live session handles. The following example is compiled without execution; calling it contacts the selected daemon.

```haskell
module CatalogExample (inspectSaved) where

import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Daemon.Session

inspectSaved :: Daemon.DaemonOptions -> IO [AvailableSessionInfo]
inspectSaved options = Daemon.withConnection options $ \connection -> do
  page <- Daemon.listAvailableSessions connection defaultListAvailableSessionsParams
  pure (availableSessions page)
```

The connection exposes `listOpenedSessions`, `listAvailableSessions`, `getSessionMessages`, `searchSessions`, `archiveSession` and `unarchiveSession`. Parameter/result records are in `Schema.Daemon.Session`; the corresponding `Client.*Daemon*` bindings retain caller-selected request IDs and deadlines. High-level operations use the existing thirty-second request path and do not add retries, load guards or session retirement.

Available-list and message-page limits default to 50 and 20. `mkSessionPageLimit` accepts finite values from 1 through 100, including fractions as the reference schema permits. List timestamps/cursors use seconds; search timestamps and time filters use milliseconds. Cursor fields are explicit: the SDK does not walk pages or locally filter results. Message retrieval reuses `FactoryDroidMessage`. Invalid worktree metadata alone becomes absent, and mission optional nulls normalize to absence; other malformed declared fields still fail. Unknown extensions remain accessible data.

Use `defaultArchiveSessionParams sessionId` for ordinary archival, or set its optional `archiveForce` explicitly. A returned false success is preserved. Archive timestamps remain opaque strings, not validated clocks. Archival acts on saved state on the daemon; ordinary connection cleanup does not archive or delete anything.

`onArchiveStateChanged connection callback` observes archive changes across session IDs on that connection. A missing timestamp reports unarchived state; malformed payloads and terminal channel failures are explicit `Left DaemonEventError` values. Callbacks run on dispatcher intake, may issue ordinary RPC queries, and must not wait for later notifications. Ordinary callback exceptions are isolated; unsubscribe is idempotent and prevents later admission, while an admitted callback may finish. Scope cleanup cancels and joins owned delivery. This is not a catalog cache or a complete daemon state manager. See [catalog verification](docs/development.md#daemon-session-catalog-delivery).

### Queued-message resolution

`Daemon.resolveQueuedUserMessage connection sessionId params` deletes a queued message or changes its placement. It reuses `Schema.Control.ResolveQueuedMessageParams`, with `DeleteQueuedMessage` or `UpdateQueuedMessage QueueEndOfTurn`/`QueueEndOfLoop`. The explicit session ID overrides parameter extensions.

```haskell
module QueueExample (deleteQueued) where

import Data.Aeson (Object)
import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Control
  ( QueueResolution (DeleteQueuedMessage),
    ResolveQueuedMessageParams (..),
  )

deleteQueued :: Daemon.DaemonConnection -> Text -> Text -> IO Object
deleteQueued connection sessionId requestId =
  Daemon.resolveQueuedUserMessage
    connection
    sessionId
    (ResolveQueuedMessageParams requestId DeleteQueuedMessage mempty)
```

Compilation does not execute the remote mutation. Legacy full replies return directly, preserving extensions. An `accepted = true` reply waits for a valid `create_message` matching both the session and the **new RPC ID**, not the original queued-message ID, then returns an empty object. The shared submission path registers listeners before sending and applies one thirty-second budget across reply and completion. `Client.resolveDaemonQueuedUserMessageRaw` instead returns the immediate RPC object (possibly an ACK), with the caller's ID/context/deadline and no completion wait.

Cancellation stops waiting, not an applied remote mutation. A successful resolution updates the connection-owned queue: deletion retires the original queue identity; placement changes move a still-present entry to the front without recreating an already confirmed entry. Failures leave local queue state unchanged. See [RPC verification](docs/development.md#daemon-queue-resolution-delivery) and [queue-state verification](docs/development.md#queue-state-delivery).

### Local and restored queues

`QueueEntry` composes the existing `QueuedUserMessage` receipt with a native kind and an observation timestamp. It preserves complete `AddUserMessageParams`, rather than rebuilding a send from rendered text and losing options or attachments. Queue kinds distinguish locally deferred, locally paused, daemon-discardable, daemon end-of-loop and manual-compaction-deferred work. Queue observation timestamps are not daemon creation times.

```haskell
import Data.Aeson (Object)
import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Control (AddUserMessageParams, QueuedUserMessage (..))
import Factory.Droid.SessionState qualified as State

saveForLater :: Daemon.DaemonConnection -> Text -> Text -> AddUserMessageParams -> IO ()
saveForLater connection identifier requestId input =
  Daemon.queueUserMessage connection identifier State.QueueDeferredAfterInterrupt (QueuedUserMessage requestId input)

sendSaved :: Daemon.DaemonConnection -> Text -> Text -> IO Object
sendSaved = Daemon.sendQueuedUserMessage

readQueue :: Daemon.DaemonConnection -> Text -> IO [State.QueueEntry]
readQueue = Daemon.getQueuedMessages
```

This example is compiled, not executed. Enqueueing is local; `sendSaved` explicitly submits one local entry through the normal ACK-aware path. Daemon-backed entries must use resolution rather than resubmission. Sending retains or creates a separate persisted-message ID before removing the local row. Failure/cancellation restores a paused entry only if confirmation or a newer queue observation has not superseded it, preserving the original exception. The SDK never silently replays uncertain sends.

`queueUserMessages`, `replaceDaemonQueuedMessages`, `dequeueQueuedMessage(s)`, `restoreQueuedMessages`, `clearQueuedMessages`, `markQueuedMessageProcessed` and `pauseQueuedMessages` expose local transitions. Upsert retains the first position and latest value for a request ID; front restoration retains input order. Confirmed/retired identities cannot be resurrected by enqueue or late restoration. For bulk kind selection, `Nothing` means all kinds and `Just []` means none. Raw queue snapshots remain explicit sensitive data.

Accepted loads replace daemon-backed entries while retaining local work. Missing remote entries remain daemon-backed while the reported agent loop is busy; when idle and delivery is uncertain, they become locally paused for explicit action. Only fresh explicit persisted-message identity establishes delivery; matching text or clocks does not. A newer load generation controls queue publication together with settings, mission state and readiness. No load performs automatic resubmission.

Discard notifications and successful interruption pause/discard through the same model: discardable text is removed, image/document/path-bearing entries are retained as paused, and end-of-loop work remains available unless its text was explicitly restored. Legacy submissions without confirmation enter the observed queue when their placement/busy-state contract requires it; real early confirmations cannot be requeued by the later RPC result. These operations add no scheduler, automatic dequeue worker, renderer or remote rollback.

### Queue review classification

`isReviewableQueuedMessage`, `queueDisplayGroup` and `queueReviewPriority` classify the existing queue kind without inspecting text or changing storage. `QueueDisplayGroup` is separate from wire `QueuePlacement`: a display group is not a send policy. `isDaemonQueuedMessage` and `queueKindForPlacement` retain their existing behavior.

| Kind | Display group | Reviewable | Priority |
| --- | --- | --- | --- |
| `QueueDeferredAfterInterrupt` | `QueueSteeringGroup` | No | `Nothing` |
| `QueuePaused` | `QueueQueuedGroup` | Yes | `Just 1` |
| `QueueDaemonDiscardable` | `QueueSteeringGroup` | Yes | `Just 0` |
| `QueueDaemonEndOfLoop` | `QueueQueuedGroup` | Yes | `Just 1` |
| `QueueDeferredDuringCompaction` | `QueueQueuedGroup` | No | `Nothing` |

Filter reviewable entries before ordering by their optional priority. This example produces a stable, ascending-priority view while retaining complete entries:

```haskell
module QueueReviewExample (reviewQueue) where

import Data.List (sortOn)
import Factory.Droid.SessionState qualified as State

reviewQueue :: State.SessionState -> [(State.QueueDisplayGroup, State.QueueEntry)]
reviewQueue state =
  [ (State.queueDisplayGroup (State.queueEntryKind entry), entry)
  | entry <-
      sortOn (State.queueReviewPriority . State.queueEntryKind) $
        filter (State.isReviewableQueuedMessage . State.queueEntryKind) (State.sessionQueue state)
  ]
```

Priority zero is reviewable, not missing. Classification and this pure view neither dequeue nor send, restore or replay an entry; execution still requires the explicit queue operations above.

## Daemon workspace and file operations

The authenticated `DaemonConnection` also exposes `checkFolderTrust`, `trustFolder`, `validateWorkingDirectory`, `changeWorkingDirectory`, `listFiles`, `searchFiles`, `getWorkspaceFileContent`, `pushCwdFileToUrl` and `pullUrlToCwdFile`. Parameter/result records live in `Schema.Daemon.Workspace`; directory validation/change reuse `Schema.Control` results. The target session must be known to the daemon. These calls do not implicitly load a session, inspect local paths or change the SDK process cwd. A validated `changeWorkingDirectory` result also updates the observed cwd described below; validation and trust reports do not.

```haskell
module WorkspaceExample (inspectFile) where

import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Daemon.Workspace

inspectFile :: Daemon.DaemonConnection -> Text -> Text -> IO GetWorkspaceFileContentResult
inspectFile connection savedSession path =
  Daemon.getWorkspaceFileContent connection
    (GetWorkspaceFileContentParams savedSession path Nothing Nothing mempty)
```

This example is compiled without execution; invoking it contacts the daemon. Paths, URLs, file contents, encodings and fingerprints remain explicit wire data. A trust check does not grant trust, and `promptRequired = False` does not imply `isTrusted = True`. `trustFolder` is a separate explicit remote mutation. File-list hidden flags default to false. High-level file search defaults to 50 results, while the low-level `Client.searchDaemonFiles` follows the schema's 60; explicit values, including zero, are retained.

Transfers are executed by the daemon against caller-supplied presigned URLs. The high-level RPC budget is fifteen minutes, matching the reference URL lifetime; low-level callers retain their chosen `CallOptions`. Cancelling a wait or closing the connection does not guarantee that remote transfer work stopped or rolled back. The SDK does not upload/download local files, decode base64 implicitly or verify reported content metadata against bytes.

`onSetupStepProgress` uses the same connection-wide dispatcher/error/unsubscribe semantics as `onArchiveStateChanged`. Progress text is data, not a command or completion signal. Connection-level observers must be installed before the operations they need to observe; they do not retroactively cover initialization done by a different `withSession` scope. See [workspace verification](docs/development.md#daemon-workspace-and-file-delivery).

### Observed working directory

`getWorkingDirectory connection sessionId` reads the last remote cwd observation without an RPC. It is not a trust decision or a certificate that the session remains active. Creation uses the accepted worktree path or requested cwd, following the existing initialization contract. A validated load uses reported cwd, then the established legacy worktree-path fallback; absence of both becomes `Nothing`. Empty strings remain explicit values. The immutable `sessionInfo` receipt does not change.

```haskell
module CwdExample (changeAndObserve) where

import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Daemon.Workspace (ChangeSessionWorkingDirectoryParams (..))

changeAndObserve :: Daemon.DaemonConnection -> Text -> Text -> IO (Maybe Text)
changeAndObserve connection identifier requested = do
  _ <- Daemon.changeWorkingDirectory connection (ChangeSessionWorkingDirectoryParams identifier requested mempty)
  Daemon.getWorkingDirectory connection identifier
```

The example is compiled, not executed. The change publishes the daemon's `resolvedPath`, not the requested path, through ordered intake before returning. Its thirty-second budget includes that publication barrier. Do not invoke the change from a serial notification callback; the getter is safe there. A later cwd event remains authoritative, including after a cancelled request whose remote effect was uncertain. Failed or malformed RPC replies do not invent a new observation.

`WorkingDirectoryEvent` updates the model before session callbacks and is also understood by `SessionState.applySessionEventAt`. A malformed cwd notification makes the getter raise `DroidInvalidEvent` until a validated load, change result or event repairs the observation. `SessionState.sessionWorkingDirectory` distinguishes unknown, inherited, reported and invalid state; invalid state retains the previous raw value, and state `Show` is redacted.

Child discovery inherits a nonempty parent cwd only while the child has no observation. That inheritance remains provisional. Reported empty/absent values and invalid observations are not replaced by a parent guess, unlike the source SDK's falsy-value seeding. These operations neither normalize paths nor call trust/validation APIs implicitly, and never call process `chdir`. See [cwd contracts, regressions and full fess](docs/development.md#authoritative-cwd-delivery).

## Daemon terminals

`createTerminal`, `writeTerminalData`, `resizeTerminal`, `closeTerminal` and `listTerminals` operate on an explicit session ID through `DaemonConnection`. They reuse the base/scoped records in `Schema.Daemon.Terminal`; the supplied session ID overrides any base-record extension. `defaultCreateTerminalParams terminalId` leaves dimensions, cwd and environment for the daemon to choose. Explicit caller fields are not replaced by retained terminal metadata.

```haskell
module TerminalExample (inspectTerminals) where

import Data.Text (Text)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.Schema.Daemon.Terminal (ListTerminalsResult)

inspectTerminals :: Daemon.DaemonConnection -> Text -> IO ListTerminalsResult
inspectTerminals = Daemon.listTerminals
```

This example is compiled, not executed. Creation returns either `TerminalCreated` or `TerminalAlreadyExists`; write/resize/close preserve actual `SuccessResult` flags. An acknowledgement is not an exit event. No local PTY or terminal-store entry is created, and ordinary connection cleanup does not implicitly close remote terminals.

Register `onTerminalEvent connection sessionId callback` before creation or input to receive early data/exit notifications for all terminals in that session. Foreign-session payloads are discarded before decoding; unrelated event kinds are ignored; malformed owned terminal events become `InvalidDaemonEvent`. Terminal IDs, text/control sequences, exit codes and signals remain explicit data. Callback errors, cancellation, unsubscribe and scope ownership follow the existing dispatcher rules.

`listTerminals` retains nullable PID and optional serialized/plain screen state, dimensions, timestamps and cursor visibility. Its receipt does not itself replace retained terminal state; independent notifications are still observed. The approved timestamp contract uses strict wire strings, preserving offset, case and fractional precision rather than JavaScript coercion. Invalid timestamps fail the listing/restoration without replacing retained state. Leap-second checks validate position, not IERS occurrence announcements; see the [date-time boundary](docs/parity.md#date-time-validation-boundary) and [terminal RPC verification](docs/development.md#daemon-terminal-delivery).

### Retained terminals and output writers

`loadTerminals connection sessionId` publishes a validated listing through the existing ordered intake before returning. Session loads, including child hydration, restore terminals by default; `daemonRestoreTerminalsOnLoad = False` disables that additional query. New-session initialization does not perform it. Explicit terminal restoration has a thirty-second exchange-and-publication budget; automatic restoration stays within the enclosing sixty-second session-load boundary.

`addTerminal`, `getTerminals`, `updateTerminalStatus`, `removeTerminalFromStore`, `setActiveTerminalId` and `getActiveTerminalId` manage local metadata only. `SessionState.sessionTerminals` preserves insertion order. Active selection is independent of membership; removing a metadata entry does not infer a new active terminal, close the remote terminal or unregister an output sink.

```haskell
module TerminalStateExample (withTerminalOutput, saveFrame) where

import Control.Exception (bracket)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Factory.Droid.Daemon qualified as Daemon
import Factory.Droid.SessionState qualified as State

withTerminalOutput :: Daemon.DaemonConnection -> Text -> Text -> (Text -> IO ()) -> IO a -> IO a
withTerminalOutput connection identifier terminal sink action =
  Daemon.withResumedSessionOn connection identifier $ \session ->
    bracket (Daemon.registerTerminalWriteHandler session terminal sink) id (const action)

saveFrame :: Daemon.DaemonConnection -> Text -> Text -> State.TerminalSerializedState -> IO ()
saveFrame connection identifier terminal frame = do
  milliseconds <- (floor . (* 1000) <$> getPOSIXTime) :: IO Integer
  Daemon.storeTerminalState connection identifier terminal
    (frame {State.terminalSerializedTimestamp = State.TerminalEpochMilliseconds (fromInteger milliseconds)})
```

The example is compiled, not executed. `registerTerminalWriteHandler` is an **output sink**, distinct from remote input `writeTerminalData`. Registration waits for a nonempty buffered prefix to flush; later data is delivered serially under the attachment lifetime. The returned unsubscribe is idempotent and token-specific. Detach cancels and joins admitted writer IO without closing a remote terminal; an admitted write can already have produced an effect. Writers have no caller-thread guarantee. Ordinary RPC queries are permitted, but registering writers, loading/restoring sessions or waiting for later intake from a serial notification callback is not.

`getTerminalSerializedState` and `getTerminalBufferedData` expose restoration inputs, not a renderer. `storeTerminalState` retains the supplied timestamp and clears the prior buffer; the example wrapper deliberately captures current milliseconds instead. `TerminalWireTimestamp` preserves a reported timestamp spelling, while `TerminalEpochMilliseconds` carries exact local numeric time. `clearTerminalBufferedData` preserves the snapshot; `clearTerminalRestorationState` explicitly discards both. Apply these clears only when their data has been consumed or is intentionally discarded. The sink helper does not interpret or replay a serialized screen.

Only tracked terminal metadata retains unmounted output; raw `onTerminalEvent` and an explicitly registered sink can still receive other terminal IDs. Successful writes acknowledge only their captured prefix. Newer output and resets are protected; failures leave tracked bytes buffered, unregister that writer and expose `TerminalWriterFailed` through `sessionTerminalError`. A later successful write clears that failure; listing terminals does not prove the writer recovered. Explicit retry can duplicate effects from a partially completed sink write.

A newer restoration or local metadata change wins over an older listing. Reported screen state replaces only the pre-request buffered prefix; later output remains. A listing without screen state preserves the existing snapshot and buffer, rather than copying the source SDK's data loss. A main session receipt can already have been accepted when subsequent terminal restoration fails; the error prevents a ready handle, not a rollback of accepted observations. No PTY, ANSI parser, renderer, implicit terminal creation or remote cleanup is supplied. See [restoration contracts, regressions and full fess](docs/development.md#terminal-restoration-delivery).

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

External MCP configuration/management and early-event delivery are verified against offline peers. SDK-hosted Haskell tools are described below; live authentication verification remains separate. See [external MCP evidence](docs/development.md#external-mcp-configuration-delivery).

## Hosted Haskell tools

`Factory.Droid.MCP.Tool` defines native raw, typed and structured handlers; `Factory.Droid.MCP.Server` owns authenticated loopback HTTP servers. Put server handles in `droidHostedMcpServers` or `daemonHostedMcpServers` to acquire them before session startup and release them after the session scope. All three styles are [verified through native local/daemon peers](docs/development.md#hosted-session-integration-acceptance), including replacement and cleanup. The example below makes no model turn, but calling it can contact the CLI and configured services. Offline verification compiles it without execution.

Hosted validation requires the trusted native `factory-droid-validator` executable. From this repository or the unpacked Cabal source distribution, install it with:

```sh
cargo install --path native/schema-validator --locked
```

Put the installation's `bin` directory on `PATH`, or set `schemaValidatorExecutable` in `hostedSchemaValidator` explicitly. The Nix development shell declares the worker package as a dependency. Rust is a build dependency; neither Rust, Python nor Node.js is needed as an interpreter at runtime. `factory-droid-validator --licenses` prints the embedded dependency notices.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module HostedExample (inspectHosted) where

import Factory.Droid
import Factory.Droid.MCP.Server
import Factory.Droid.MCP.Tool
import Factory.Droid.MCP.Validator
import Factory.Droid.Schema.MCP (ListMcpServersResult)

inspectHosted :: FilePath -> IO ListMcpServersResult
inspectHosted directory = do
  echo <- either (const (ioError (userError "Invalid tool definition"))) pure
    (rawTool "echo" "Echo supplied arguments" openObjectSchema (pure . structuredResult))
  let validation = defaultSchemaValidatorOptions {schemaValidatorTimeoutMicros = 5000000}
  server <- newMcpServer
    ((defaultMcpServerOptions "haskell-echo") {hostedSchemaValidator = validation}) [echo]
  let options = (defaultDroidOptions directory) {droidHostedMcpServers = [server]}
  withDroidSession options listDroidMcpServers
```

`rawTool` receives only the argument object. `typedTool` additionally decodes it once with `FromJSON`; `structuredTool` serializes a typed return value with `ToJSON` and validates its advertised object output schema. Use `withToolOutputSchema schema tool` to attach an output schema to a raw or typed handler returning `McpToolResult`, preserving its rich content and metadata. Successful results must supply matching structured content; explicit tool errors bypass output-schema validation. String-like output uses `textResult`; `structuredResult` supplies both text JSON and structured content. `jsonContent` validates rich text/image/audio/resource-link/embedded-resource blocks. Argument or handler failures become sanitized error tool results; invalid result envelopes remain protocol errors.

Embedded resources validate a text-or-blob union: one complete valid branch suffices, including empty text. Fields belonging only to the other branch do not invalidate it; shared URI, MIME-type and metadata fields are still checked. Native content retains the supplied object and extensions rather than adopting the reference Zod parser's unknown-field stripping.

MCP `annotations.lastModified` requires a valid four-digit-year calendar date, uppercase `T`, seconds, an optional fractional part, and uppercase `Z` or a colon-separated numeric offset. Minute-only spellings, leap seconds, lowercase separators and compact offsets are rejected. Accepted strings retain their exact spelling and precision; the SDK does not add missing seconds. This MCP boundary does not change the broader RFC 3339 scalar or terminal timestamp policies.

Server handles use identity equality. Repeated and concurrent `startMcpServer` calls share the current runtime; explicit start retains caller ownership until `closeMcpServer`. `withMcpServer` and session integration acquire leases: overlapping scopes share an endpoint, and the last SDK-owned lease stops it. Already manually started servers are borrowed, not silently transferred. Replacement and rollback retain the original scope's endpoint and lease. Close is repeat-safe, waits for admitted handler finalizers and is shared by concurrent closers; restart waits for closure and rotates the bearer token. Self-close from a handler is rejected to avoid deadlock. Handlers must cooperate with asynchronous cancellation.

The endpoint binds only `127.0.0.1` on an ephemeral port, requires a random bearer token, and validates Host, optional Origin, Accept and JSON content type before reading a bounded body. Defaults are 4 MiB per request, 10 MiB per response and a thirty-second tool deadline; limits are configurable. Replies use stateless JSON, not a persistent MCP session or cross-POST cancellation registry. Scope closure and request deadlines cancel owned work; client disconnect does not undo effects or promise immediate cancellation. No browser, permissive CORS or public bind is introduced. Remote daemon targets cannot use SDK-owned loopback servers without an explicit forwarding arrangement; pass a caller-managed external configuration for such arrangements.

**Schema construction and compilation are distinct.** `mkMcpSchema` retains an object schema unchanged after checking its root `type = "object"` and 64 KiB encoded-size limit. `validateMcpSchema` performs full native compilation; `newMcpServer` compiles both input and output definitions before publishing a server handle. Invalid definitions and missing workers fail explicitly. Direct invocation uses `invokeTool`, or `invokeToolWithValidator` with explicit worker settings; infrastructure failures return `McpSchemaValidationFailed`, rather than accepting data or blaming the handler.

Validation supports exact JSON numbers, regular expressions including lookbehind, local/embedded references with correct JSON Pointer and URI-fragment escaping, ref siblings, combinators and evaluated-property constraints. Format/content fields remain annotations. External HTTP/file retrieval is disabled: provide schema resources within the document rather than granting implicit network or filesystem access. This policy is explicit, not a restricted-validator fallback.

The declared `$schema` is retained. Content keywords are non-asserting even in Draft 6/7: the worker disables the pinned backend's built-in encoding and JSON-media checks through its standard options, without disabling type/bound checks, schema-definition validation or dialect-specific reference rules. Negating an annotation-only schema still rejects the instance; it does not authorize a handler. This schema policy is separate from the required Base64 validation of rich MCP image/audio/blob content.

Each nontrivial validation owns one process and a deadline covering encoding, launch and execution; the default is thirty seconds, with a combined schema/value frame limit of 10 MiB. The existing hosted request deadline additionally covers input validation, handler work and output validation. Timeout, cancellation and execution failure terminate/reap the worker without reusing uncertain state. Engine resource failures and panics never become successful validation through negation. The already-object argument satisfies the exact, otherwise unconstrained `openObjectSchema` by construction; no process is needed for that instance check. See [native validator evidence](docs/development.md#native-hosted-validator-integration) and the narrowly modified [worker-local dependency](native/schema-validator/VENDOR.md).

## Factory REST resources

`Factory.Droid.REST` provides the thirteen baselined computer, machine-template, metrics and remote-session helpers. Credentials are explicit; no CLI login files or credential environment variables are read. Reuse a `RestClient` across related calls. This example is compiled during verification, not executed: calling it contacts Factory.

```haskell
module RestExample (inspectComputers) where

import Data.Text (Text)
import Factory.Droid.REST
import Factory.Droid.Schema.REST (Computer)

inspectComputers :: Text -> IO [Computer]
inspectComputers apiKey = do
  client <- newRestClient (defaultRestOptions apiKey)
  listComputers client
```

| Resource | Functions |
| --- | --- |
| Templates | `listMachineTemplates`, `getMachineTemplate` |
| Computers | `listComputers`, `getComputer`, `getComputerByName`, `createComputer`, `updateComputer`, `deleteComputer`, `restartComputer`, `refreshComputer` |
| Operations | `getComputerMetrics`, `retryInstallDeps` |
| Remote sessions | `listRemoteSessions` |

Creation/update records and response types live in `Factory.Droid.Schema.REST`; `defaultCreateComputerParams`, `defaultUpdateComputerParams` and `defaultPageOptions` supply omitted optional fields. Paginated replies retain their cursor and `hasMore` flag. Computer extensions are retained; other response records discard unknown fields as the reference does. Remote-session listing is neither local saved-session discovery nor daemon session management.

The default endpoint is `https://api.factory.ai`, with verified TLS, a thirty-second whole-request deadline and a 10 MiB decoded-response limit. `RestOptions` permits an explicit base URL, deadline and response limit; an HTTP URL explicitly chooses plaintext. URI credentials, queries, fragments, invalid ports and empty/dot-only resource identifiers are rejected. Path/query values are encoded as data. Unlike ambient `fetch` policy, the native client does not follow redirects, consult proxy environment variables or automatically retry requests; credentials stay at the chosen endpoint and uncertain mutations are not replayed.

Active responses close on completion, error or cancellation. The standard HTTP manager controls reusable idle connections and their automatic cleanup; there is no misleading explicit pool-close operation. Cleanup does not delete remote computers. `RestError` distinguishes transport, deadline, size, JSON/shape, authentication and API-status failures. Error-message fields remain available explicitly, but `Show` redacts them and credentials. See [REST verification](docs/development.md#native-factory-rest-delivery).

## Moving from Python or TypeScript

Migrate the operation and its ownership contract, rather than reproducing an SDK class hierarchy:

| Existing pattern | Native Haskell counterpart |
| --- | --- |
| One prompt or a retained client/session | `runDroid` for one prompt; `withDroidSession`, `withResumedDroidSession` or daemon `with*` scopes for retained state and cleanup. |
| Async iterator or event subscription | `sendDroidEvents` for turn events, or `withDroidStream` for an explicit feed. Callbacks run within the documented scope; prompts on one local session remain serialized. |
| Dynamic option/result objects | Typed records plus retained extension objects. Preserve each field's omission/null distinction; `Nothing` is not a universal JSON-null operation. |
| Hosted tool functions | Native handlers through `MCP.Tool` / `MCP.Server`, with the separate native validator executable installed. Owned child IPC additionally requires the C-only launcher. |
| Promise/task cancellation | `IO` exceptions and scoped asynchronous cancellation. Cleanup releases owned resources; stopping a wait does not undo effects already accepted by a peer. |

Keep explicit permission decisions and daemon credentials separate from metadata. Supplied transports remain borrowed. Native sessions omit unsupported optional SDK-language attribution rather than impersonating Python or TypeScript. The [parity matrix](docs/parity.md) records the approved contract differences; a compiling example is not proof of live interoperability.

## Build and example

```sh
nix develop path:.
cabal build all --ghc-options=-Werror
cabal test all --test-show-details=direct
```

The supported SDK compiler is **GHC 9.12**, pinned to **9.12.4** in the default Nix shell and macOS/Linux CI. The `ghc912` shell selects the same compiler. In the local direnv workflow, regenerate the shell with `de --max-jobs 4 path:.`, confirm `direnv exec . ghc --numeric-version`, and run project tools through `direnv exec .`. The `de` command is a local environment helper, not an SDK dependency.

The user selected this stable compiler matrix on 2026-09-13; GHC 9.14 is no longer a release target. Cabal restricts `base` to the 4.21 series without relaxing dependency bounds. GHC 9.12.4 has previous passing builds and full test suites on macOS ARM64 and GNU/Linux ARM64; current release checks remain separately recorded. `linuxChecks` and `linuxChecks912` provision the same public Linux 9.12.4 tool closure without adding foreign executables to the host PATH. See the [compiler record](docs/development.md#compiler-matrix-checkpoint) and [Linux execution record](docs/development.md#linux-execution-checkpoint).

The compiled example makes **two model requests** using the existing CLI login and may incur charges. Each turn has a two-minute timeout:

```sh
cabal run droid-example -- /path/to/working-directory
```

The offline high-level tests require no Factory credentials:

```sh
cabal test all --test-show-details=direct \
  --test-option=-p --test-option='High-level local Droid path'
```

## Observability

`Factory.Droid.Observability` provides optional structured logging, exact counter/histogram events and immutable trace-context providers. `defaultDroidObservability` disables all sinks. Configure `observabilityLogger`, `observabilityMetrics` and `observabilityTracing` with `droidLogger`, `droidMetricSink` and `droidTraceContextProvider`; no telemetry service, environment switch or background worker is required.

```haskell
module ObservabilityExample (observedPrompt) where

import Data.Text (Text)
import Factory.Droid
import Factory.Droid.Observability (DroidObservability)

observedPrompt :: DroidObservability -> FilePath -> Text -> IO Text
observedPrompt observability directory prompt =
  withObservedDroidSession observability (defaultDroidOptions directory) Nothing defaultDroidHandlers $ \session ->
    resultText <$> sendPrompt session prompt (const (pure ()))
```

The example is compiled only. Supply `Just sessionId` to resume; `withObservedDroidSessionOn` provides the same configuration over a borrowed `ObjectTransport`. Existing local constructors remain unchanged and quiet by default. Set `daemonObservability` / `daemonClientObservability` for daemon connections. Low-level callers can use `withObservedRpcChannel` and `withObservedJsonLinesProcess`.

RPC exchanges and connection/process scopes emit `.start`, `.returned` or `.failed` events, duration histograms in milliseconds and count events. `droid.process.startup` measures acquisition/setup before the caller's action. Scope durations include the scope's action; exchange durations include writer/trace work but exclude the optional before-request guard. `returned` means the underlying call returned, not that a remote operation succeeded. Channel failures report payload-free categories; `droid.rpc.remote_error` logs/counts error responses using only method and code, never their message or data.

Set `observabilityLogTransport = True` for metadata-only `in`, `out` and `out_failed` records. `loggedObjectTransport` supports explicit context and an optional caller-supplied renderer; default events contain direction, transport kind and field count, not payloads, credentials, trace values, argv or stderr. A custom renderer explicitly opts into caller-chosen content and must handle its own redaction. There is no automatic raw log file.

`emitDroidLog` / `recordDroidMetric` return whether the configured sink completed, not whether a backend persisted data. Attributes retain scalar JSON values, including null/false/zero/exact numbers; unsupported nested values are omitted. `sdkLogEvent` reduces output/preview/stderrTail strings to UTF-8 byte lengths. Explicit messages, other attributes and `DroidSerializedError` values remain caller-controlled and can be sensitive; redacted `Show` is not a general scrubber.

Trace providers run afresh, and only successfully evaluated `traceparent`/`tracestate` fields are merged into the existing `_meta`. Missing fields do not erase the carrier; empty strings remain explicit. Provider failure leaves the original carrier unchanged. Mandatory write admission is rechecked after provider I/O, so a blocked provider cannot authorize a stale generation.

Sinks/providers run on the existing emitting, writer or reader thread. They must be cooperative, thread-safe, and must not reenter the same observed operation or await its future traffic. Ordinary callback failures are isolated and optionally reported through the component's failure observer; standard asynchronous exceptions propagate. An already-failed operation retains its original exception over a later telemetry failure, and cleanup remains with the existing owner. See [batch evidence and limitations](docs/development.md#observability-integration).

### Computer-connect SLI

`Connection.recordComputerConnectSli options predicate action` observes a connect operation without performing or retrying it itself. Each invocation supplies a fresh UUID attempt ID, a trigger (`"unknown"` when omitted), and `setConnectStartType`. A false predicate records failure with `not_fully_connected` but returns the same value; action or predicate exceptions are classified and rethrown unchanged. This is distinct from `observeDroidOperation`, whose `returned` outcome still does not mean remote success.

Configure optional `computerConnectMetricSink` and `computerConnectSpanSink` explicitly. `DroidSpanAttributesSink` / `setDroidSpanAttributes` extend observability with active-span attribute delivery, separately from trace-context injection. They neither create a span nor install a global backend; successful delivery means only that the supplied callback returned.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module ComputerConnectMetricsExample (pollObserved) where

import Data.Aeson (Object)
import Data.Text (Text)
import Factory.Droid.Connection qualified as Connection
import Factory.Droid.Observability qualified as Obs

pollObserved :: (Obs.DroidMetricEvent -> IO ()) -> (Object -> IO ()) -> Connection.ConnectionController -> Maybe Text -> IO Bool
pollObserved recordMetric setSpan controller startType =
  let options = (Connection.defaultComputerConnectSliOptions "native")
        { Connection.computerConnectMetricSink = Just (Obs.droidMetricSink recordMetric)
        , Connection.computerConnectSpanSink = Just (Obs.droidSpanAttributesSink setSpan)
        }
  in Connection.recordComputerConnectSli options id $ \attempt -> do
       Connection.setConnectStartType attempt startType
       Connection.pollUntilConnected controller Connection.defaultConnectionPollOptions
```

This example is compiled, not executed. Supply start type from known facts; the SDK does not infer whether a computer was cold or warm. Here one SLI invocation surrounds the whole poll, not each internal retry. It emits `factory_app_computer_connect_sli_attempt_count` followed by `factory_app_computer_connect_sli_duration_ms`. The latest nonempty start type, surface, optional provider type, trigger and outcome are metric labels; attempt ID/trigger and final outcome/reason are active-span attributes. Empty provider/start labels are omitted, while an explicit empty trigger remains explicit.

Native `RpcRequestTimedOut` has no method field. Set `computerConnectRequestMethod` only when that failing request's method is known: `daemon.authenticate` then yields `daemon_auth_timeout`; an anonymous timeout remains `daemon_timeout`. Structured `ConnectionFailure` reasons, including already-reported compute limits, are preserved; native transport/auth classification is reused and standard asynchronous cancellation is `aborted`. Exception text is not inspected to invent missing context.

Duration is monotonic milliseconds, potentially fractional, including initial span delivery; it is not JavaScript wall-clock arithmetic. Existing sink failure isolation and cancellation rules apply. Keep resource brackets inside the action: reporting can cancel after a successful action, and an already-failed action retains its original exception over later reporting failure. No telemetry persistence, model success or hard connection/time bound is implied.

## Scope

The repository implements local and daemon sessions, external and hosted MCP with native validation, REST/resources/coordinated state, owned and supplied IPC, relay orchestration, configuration, stream/helpers, saved-session scanning/selection/native timestamps and explicit observability. Current repairs and remaining functional/native-contract work are tracked in [HANDOFF.md](docs/HANDOFF.md). Per-unit GHC 9.12.4 builds and full-suite receipts cover the observed macOS/GNU/Linux ARM64 targets. Earlier live interoperability and separately unpacked source-distribution checks are historical evidence for their recorded inputs, not release signoff for the repaired tree; final packaged-release validation remains pending. The [parity matrix](docs/parity.md) and retained evidence distinguish scope and unexecuted operations/platforms. Exhaustive schema/codec coverage remains a separate [opt-in backlog](docs/exhaustive-codec-backlog.md), not a completion gate.

See [development notes](docs/development.md) for retained build evidence. Licenses and upstream notices are in [LICENSE](LICENSE) and [NOTICE](NOTICE).
