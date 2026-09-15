{-# LANGUAGE OverloadedStrings #-}

module TerminalStateSpec (terminalStateTests) where

import Data.Aeson (Result (..), Value (..), fromJSON)
import Data.Map.Strict qualified as Map
import Factory.Droid.Schema.Daemon.Terminal (TerminalExit (..))
import Factory.Droid.Schema.Primitives (Rfc3339Timestamp)
import Factory.Droid.SessionState qualified as State
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

terminalStateTests :: TestTree
terminalStateTests =
  testGroup
    "Terminal retained state"
    [ testCase "metadata replacement retains insertion order and independent active selection" $ do
        let first = State.addSessionTerminal (State.defaultTerminalMetadata "z" State.TerminalConnecting) State.emptySessionState
            both = State.addSessionTerminal (State.defaultTerminalMetadata "a" State.TerminalConnected) first
            replacement = State.setActiveTerminalId (Just "external") (State.addSessionTerminal (State.defaultTerminalMetadata "z" State.TerminalError) both)
        map State.terminalMetadataId (State.sessionTerminals replacement) @?= ["z", "a"]
        fmap State.terminalMetadataStatus (Map.lookup "z" (State.sessionTerminalsById replacement)) @?= Just State.TerminalError
        State.sessionActiveTerminalId (State.removeSessionTerminal "external" replacement) @?= Just "external"
        State.sessionActiveTerminalId (State.setActiveTerminalId Nothing replacement) @?= Nothing,
      testCase "exit preserves fractional code and empty signal without creating unknown terminals" $ do
        let exited = State.observeTerminalExit (TerminalExit "main" (-1.25) "" mempty) initial
        case State.sessionTerminals exited of
          [terminal] -> do
            State.terminalMetadataStatus terminal @?= State.TerminalDisconnected
            State.terminalMetadataExitCode terminal @?= Just (-1.25)
            State.terminalMetadataSignal terminal @?= Just ""
          _ -> assertFailure "Missing retained terminal"
        State.setSessionTerminalStatus "unknown" State.TerminalConnected exited @?= exited
        State.observeTerminalExit (TerminalExit "unknown" 0 "" mempty) exited @?= exited,
      testCase "serialized snapshots clear prior output and retain exact dimensions and false cursor state" $ do
        let state = State.storeTerminalSerializedState "main" snapshot (State.appendTerminalBufferedData "main" "old" initial)
        State.getTerminalSerializedState "main" state @?= Just snapshot
        State.getTerminalBufferedData "main" state @?= Nothing
        State.storeTerminalSerializedState "missing" snapshot state @?= state,
      testCase "buffers distinguish absence and empty data and preserve control sequences and Unicode" $ do
        State.getTerminalBufferedData "main" initial @?= Nothing
        let empty = State.appendTerminalBufferedData "main" "" initial
            combined = State.appendTerminalBufferedData "main" "\NUL\ESC[31m\x1f600" (State.appendTerminalBufferedData "main" "line\r\n" empty)
        State.getTerminalBufferedData "main" empty @?= Just ""
        State.getTerminalBufferedData "main" combined @?= Just "line\r\n\NUL\ESC[31m\x1f600"
        State.appendTerminalBufferedData "missing" "ignored" combined @?= combined,
      testCase "buffer-only clearing preserves serialized state while restoration clearing removes both" $ do
        let stored = State.storeTerminalSerializedState "main" snapshot initial
            pending = State.appendTerminalBufferedData "main" "later" stored
            bufferCleared = State.clearTerminalBufferedData "main" pending
            restored = State.clearTerminalRestorationState "main" pending
        State.getTerminalSerializedState "main" bufferCleared @?= Just snapshot
        State.getTerminalBufferedData "main" bufferCleared @?= Nothing
        State.getTerminalSerializedState "main" restored @?= Nothing
        State.getTerminalBufferedData "main" restored @?= Nothing
        State.clearTerminalRestorationState "missing" pending @?= pending,
      testCase "writer acknowledgement removes only its captured prefix and remains idempotent" $ do
        let buffered = State.appendTerminalBufferedData "main" "\x1f600\NUL" initial
        claim <- maybe (assertFailure "Missing buffer claim") pure (State.terminalBufferSnapshot "main" buffered)
        State.terminalBufferSnapshotText claim @?= Just "\x1f600\NUL"
        let later = State.setSessionTerminalStatus "main" State.TerminalDisconnected (State.appendTerminalBufferedData "main" "later" buffered)
            acknowledged = State.acknowledgeTerminalBuffer "main" claim later
        State.getTerminalBufferedData "main" acknowledged @?= Just "later"
        State.acknowledgeTerminalBuffer "main" claim acknowledged @?= acknowledged,
      testCase "buffer resets and terminal replacement revoke old writer acknowledgements" $ do
        let buffered = State.appendTerminalBufferedData "main" "old-prefix" initial
        claim <- maybe (assertFailure "Missing buffer claim") pure (State.terminalBufferSnapshot "main" buffered)
        let reset = State.appendTerminalBufferedData "main" "new" (State.storeTerminalSerializedState "main" snapshot buffered)
            replaced = State.appendTerminalBufferedData "main" "new" (State.addSessionTerminal (State.defaultTerminalMetadata "main" State.TerminalConnected) (State.removeSessionTerminal "main" buffered))
        State.acknowledgeTerminalBuffer "main" claim reset @?= reset
        State.acknowledgeTerminalBuffer "main" claim replaced @?= replaced,
      testCase "clearing all terminal state resets selection but cannot revive an old buffer claim" $ do
        let selected = State.setActiveTerminalId (Just "main") (State.appendTerminalBufferedData "main" "old" initial)
        claim <- maybe (assertFailure "Missing buffer claim") pure (State.terminalBufferSnapshot "main" selected)
        let cleared = State.clearSessionTerminals selected
            fresh = State.appendTerminalBufferedData "main" "fresh" (State.addSessionTerminal (State.defaultTerminalMetadata "main" State.TerminalConnecting) cleared)
        State.sessionTerminals cleared @?= []
        State.sessionActiveTerminalId cleared @?= Nothing
        State.acknowledgeTerminalBuffer "main" claim fresh @?= fresh,
      testCase "reported timestamps retain their wire spelling rather than pretending to be coerced dates" $ do
        timestamp <- case fromJSON @Rfc3339Timestamp (String "2026-09-10T01:02:03.123456789012345+02:00") of
          Success value -> pure value
          Error err -> assertFailure err
        let reported = snapshot {State.terminalSerializedTimestamp = State.TerminalWireTimestamp timestamp}
        State.getTerminalSerializedState "main" (State.storeTerminalSerializedState "main" reported initial) @?= Just reported,
      testCase "terminal observation errors remain explicit and retained state displays are redacted" $ do
        let failed = State.invalidateTerminalState (State.TerminalWriterFailed "secret-id") initial
        State.sessionTerminalError failed @?= Just (State.TerminalWriterFailed "secret-id")
        let (ticket, pending) = State.beginTerminalRestoration failed
        State.sessionTerminalError (State.restoreSessionTerminals ticket [] pending) @?= Just (State.TerminalWriterFailed "secret-id")
        State.sessionTerminalError (State.clearTerminalError failed) @?= Nothing
        show snapshot @?= "TerminalSerializedState <redacted>"
        show (State.defaultTerminalMetadata "secret-id" State.TerminalConnected) @?= "TerminalMetadata <redacted>"
        show failed @?= "SessionState <redacted>"
    ]

initial :: State.SessionState
initial = State.addSessionTerminal (State.defaultTerminalMetadata "main" State.TerminalConnected) State.emptySessionState

snapshot :: State.TerminalSerializedState
snapshot = State.TerminalSerializedState "\ESC[31mserialized" 80.125 24.25 (State.TerminalEpochMilliseconds 0) (Just False)
