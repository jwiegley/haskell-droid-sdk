#!/usr/bin/env python3
"""Exercise actual attachment IO with GHCi breakpoints, without SDK test hooks."""

import os
from pathlib import Path
import re
import signal
import subprocess
import sys
import tempfile


PROBE = r'''
:set -XGHC2024 -XOverloadedStrings
:load src/Factory/Droid/Input.hs
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Either
import Data.IORef
import System.Directory
import System.Posix.Files (createNamedPipe)
import System.Posix.IO (createPipe, fdWrite)
import System.Timeout
let probePath = @PATH@
BS.writeFile probePath "\x89PNG\r\n\x1a\n"
savedFd <- newIORef (Nothing :: Maybe Fd)
:break readBoundedFd
changed <- do { r <- try @DroidAttachmentError (void (imageFromFile probePath)); Just held <- readIORef savedFd; closed <- try @IOException (getFdStatus held); pure (r, isLeft closed) }
writeIORef savedFd (Just fd)
BS.appendFile probePath "x"
:delete *
:continue
putStrLn (if changed == (Left AttachmentChanged, True) then "MUTATION_AND_FD_CLEANUP_PASS" else error "mutation or descriptor cleanup failed")
BS.writeFile probePath "\x89PNG\r\n\x1a\n"
readThread <- newIORef (Nothing :: Maybe ThreadId)
:break readBoundedFd
cancelled <- do { tid <- myThreadId; writeIORef readThread (Just tid); r <- try @SomeException (void (imageFromFile probePath)); Just held <- readIORef savedFd; closed <- try @IOException (getFdStatus held); pure (either (\err -> fromException err == Just ThreadKilled) (const False) r, isLeft closed) }
writeIORef savedFd (Just fd)
_ <- forkIO (readIORef readThread >>= mapM_ (\tid -> throwTo tid ThreadKilled))
:delete *
:continue
putStrLn (if cancelled == (True, True) then "CANCELLATION_AND_FD_CLEANUP_PASS" else error "cancellation identity or descriptor cleanup failed")
BS.writeFile probePath "\x89PNG\r\n\x1a\n"
:break Factory.Droid.Input @OPEN@
fifo <- timeout 5000000 (try @DroidAttachmentError (void (imageFromFile probePath)))
removeFile probePath
createNamedPipe probePath 0o600
:delete *
:continue
putStrLn (if fifo == Just (Left AttachmentNotRegular) then "FIFO_REPLACEMENT_PASS" else error "FIFO replacement blocked or escaped validation")
removeFile probePath
BS.writeFile probePath "\x89PNG\r\n\x1a\n"
BS.writeFile (probePath <> "-replacement") "\x89PNG\r\n\x1a\n"
:break Factory.Droid.Input @OPEN@
replaced <- try @DroidAttachmentError (void (imageFromFile probePath))
renameFile (probePath <> "-replacement") probePath
:delete *
:continue
putStrLn (if replaced == Left AttachmentChanged then "IDENTITY_REPLACEMENT_PASS" else error "pre-open replacement escaped validation")
(reader, writer) <- createPipe
_ <- fdWrite writer "ab"
:break Factory.Droid.Input @READ@
chunks <- (do { first <- readBoundedFd reader 5; rest <- readBoundedFd reader 2; pure (first, rest) }) `finally` closeFd reader
putStrLn (if count == 2 then "SHORT_READ_OBSERVED" else error "short read was not exercised")
_ <- fdWrite writer "cdef"
closeFd writer
:delete *
:continue
putStrLn (if chunks == ("abcde", "f") then "SHORT_READ_BUDGET_PASS" else error "read continuation or byte budget failed")
:quit
'''

MARKERS = {
    "MUTATION_AND_FD_CLEANUP_PASS",
    "CANCELLATION_AND_FD_CLEANUP_PASS",
    "FIFO_REPLACEMENT_PASS",
    "IDENTITY_REPLACEMENT_PASS",
    "SHORT_READ_OBSERVED",
    "SHORT_READ_BUDGET_PASS",
}


def source_line(lines, fragment):
    matches = [index for index, line in enumerate(lines, 1) if fragment in line]
    if len(matches) != 1:
        raise RuntimeError(f"Update the probe breakpoint for {fragment!r}")
    return str(matches[0])


def main():
    root = Path(__file__).resolve().parent.parent
    lines = (root / "src/Factory/Droid/Input.hs").read_text().splitlines()
    with tempfile.TemporaryDirectory(prefix="droid-file-probe-", dir="/tmp") as directory:
        path = str(Path(directory) / "image")
        probe = PROBE.replace("@PATH@", f'"{path}"')
        probe = probe.replace("@OPEN@", source_line(lines, "bracket (openFd path ReadOnly"))
        probe = probe.replace("@READ@", source_line(lines, "if count == 0"))
        # Expose the SDK; keep Cabal's exact dependency unit IDs unchanged.
        command = ["cabal", "exec", "--offline", "--", "ghci", "-v0", "-ignore-dot-ghci", "-package", "droid-sdk"]
        with subprocess.Popen(
            command, cwd=root, text=True, stdin=subprocess.PIPE,
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, start_new_session=True,
        ) as process:
            try:
                log, _ = process.communicate(probe, timeout=60)
            except subprocess.TimeoutExpired:
                try:
                    os.killpg(process.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                log, _ = process.communicate()
                print(log, end="")
                raise RuntimeError("Attachment file probe exceeded 60 seconds") from None
        print(log, end="")
        failed = re.search(r"error:|\*\*\* Exception|not stopped|Cannot set breakpoint|does not exist|unknown command", log)
        if process.returncode or failed or not MARKERS.issubset(set(log.splitlines())):
            raise RuntimeError("Attachment file probe failed; inspect the transcript")
    print("All attachment FD/race probes passed.")


if __name__ == "__main__":
    try:
        main()
    except RuntimeError as error:
        print(error, file=sys.stderr)
        sys.exit(1)
