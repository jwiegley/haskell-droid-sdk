{-# LANGUAGE ForeignFunctionInterface #-}

module Factory.Droid.Internal.FileTime (fileBirthTime) where

import Data.Int (Int64)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Foreign (Ptr, alloca, peek)
import Foreign.C.Error (throwErrnoIfMinus1Retry)
import Foreign.C.Types (CInt (..))
import System.Posix.Types (Fd (..))

-- The descriptor remains owned by the caller. An unavailable field is not zero;
-- errors other than absent OS/filesystem support retain their errno.
fileBirthTime :: Fd -> IO (Maybe UTCTime)
fileBirthTime (Fd fd) = alloca $ \seconds -> alloca $ \nanoseconds -> do
  available <- throwErrnoIfMinus1Retry "fileBirthTime" (c_fileBirthTime fd seconds nanoseconds)
  if available == 0
    then pure Nothing
    else do
      sec <- peek seconds
      nsec <- peek nanoseconds
      pure (Just (posixSecondsToUTCTime (fromIntegral sec + fromIntegral nsec / 1000000000)))

foreign import ccall safe "hs_droid_file_birth_time"
  c_fileBirthTime :: CInt -> Ptr Int64 -> Ptr Int64 -> IO CInt
