-- | Standalone single-consumer turns, fed from caller-owned notification sources.
-- No process, protocol reader, subscription or remote interruption is created.
module Factory.Droid.Stream
  ( StreamFeed,
    DroidStream,
    DroidLegacyStream,
    DroidIdleCompletion (..),
    withDroidLegacyStream,
    DroidStreamOptions (..),
    defaultDroidStreamOptions,
    DroidStreamFrame (..),
    DroidStreamResult (..),
    DroidStreamError (..),
    DroidStreamMode (..),
    DroidEvent (..),
    DroidResult (..),
    withDroidStream,
    feedDroidEvent,
    feedDroidNotification,
    feedDroidError,
    consumeDroidStream,
    getDroidStreamResult,
    getDroidStreamFailure,
    droidStreamCompleted,
    closeDroidStream,
    StreamState,
    initialStreamState,
    decodeNotification,
    decodeDaemonNotification,
    stepStream,
    eventToolName,
    DroidStreamSummary (..),
    summarizeStream,
    adaptDroidSummaryOutput,
    DroidOutput,
    DroidOutputError (..),
    DroidOutputResult (..),
    rawDroidOutput,
    jsonDroidOutput,
    adaptDroidStreamOutput,
  )
where

import Factory.Droid.Internal.Output
import Factory.Droid.Internal.Stream

-- | Local output decoding, independent of the wire turn outcome. This does not
-- validate JSON Schema keywords or change the stream's terminal receipt.
adaptDroidStreamOutput :: DroidOutput a -> DroidStreamResult -> DroidOutputResult a
adaptDroidStreamOutput output = adaptOutput output . streamTurnResult

-- | Decode a manually summarized turn without fabricating a wire completion
-- or token-usage counters. The supplied summary remains unchanged.
adaptDroidSummaryOutput :: DroidOutput a -> DroidStreamSummary -> Either DroidOutputError a
adaptDroidSummaryOutput output summary = snd (adaptOutputData output (summaryStructuredOutput summary) (summaryText summary))
