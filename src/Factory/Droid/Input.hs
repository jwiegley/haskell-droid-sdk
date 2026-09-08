{-# LANGUAGE OverloadedStrings #-}

-- | Validated local attachment values. Signatures and byte limits are checked;
-- this module does not decode image pixels, parse PDF structure or run extractors.
module Factory.Droid.Input
  ( DroidInput (..),
    droidInput,
    DroidImage,
    DroidDocument,
    DroidAttachmentError (..),
    imageFromBytes,
    imageFromSource,
    imageFromFile,
    documentFromText,
    documentFromBytes,
    documentFromSource,
    documentFromFile,
    droidImageSource,
    droidDocumentSource,
  )
where

import Control.Exception (Exception, IOException, bracket, catch, throwIO)
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Internal qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Factory.Droid.Schema.Content (Base64ImageSource (..), Base64PDFSource (..), DocumentSource (..), ImageMediaType (..), PlainTextSource (..))
import Foreign.Ptr (plusPtr)
import System.FilePath (takeFileName)
import System.Posix.Files (FileStatus, deviceID, fileID, fileSize, getFdStatus, getFileStatus, isRegularFile, modificationTimeHiRes, statusChangeTimeHiRes)
import System.Posix.IO (OpenFileFlags (..), OpenMode (ReadOnly), closeFd, defaultFileFlags, fdReadBuf, openFd)
import System.Posix.Types (Fd)

-- | Images and documents retain order within their respective wire arrays.
data DroidInput = DroidInput
  { inputText :: !Text,
    inputImages :: ![DroidImage],
    inputDocuments :: ![DroidDocument]
  }
  deriving stock (Eq)

instance Show DroidInput where
  show _ = "DroidInput <redacted>"

droidInput :: Text -> DroidInput
droidInput text = DroidInput text [] []

newtype DroidImage = DroidImage Base64ImageSource deriving stock (Eq)

instance Show DroidImage where
  show _ = "DroidImage <redacted>"

newtype DroidDocument = DroidDocument DocumentSource deriving stock (Eq)

instance Show DroidDocument where
  show _ = "DroidDocument <redacted>"

-- | Explicit file causes may contain paths. Show redacts all payloads.
data DroidAttachmentError
  = AttachmentTooLarge !Int
  | AttachmentInvalidBase64
  | AttachmentInvalidImage
  | AttachmentImageTypeMismatch !ImageMediaType !ImageMediaType
  | AttachmentInvalidPDF
  | AttachmentInvalidUTF8
  | AttachmentInvalidPath
  | AttachmentNotRegular
  | AttachmentChanged
  | AttachmentFileError !IOException
  deriving stock (Eq)

instance Show DroidAttachmentError where
  show _ = "DroidAttachmentError <redacted>"

instance Exception DroidAttachmentError

-- | The returned ordinary wire records are explicit data, not safe log output.
droidImageSource :: DroidImage -> Base64ImageSource
droidImageSource (DroidImage source) = source

droidDocumentSource :: DroidDocument -> DocumentSource
droidDocumentSource (DroidDocument source) = source

imageFromBytes :: BS.ByteString -> ImageMediaType -> Either DroidAttachmentError DroidImage
imageFromBytes bytes media = do
  checkSize attachmentLimit bytes
  checkImage media bytes
  pure (DroidImage (Base64ImageSource (encodeBase64 bytes) media mempty))

-- | Validate without normalizing the supplied Base64 or discarding extensions.
imageFromSource :: Base64ImageSource -> Either DroidAttachmentError DroidImage
imageFromSource source = do
  bytes <- decodeBase64 attachmentLimit (imageSourceData source)
  checkImage (imageSourceMediaType source) bytes
  pure (DroidImage source)

imageFromFile :: FilePath -> IO DroidImage
imageFromFile path = do
  bytes <- readAttachmentFile path
  media <- maybe (throwIO AttachmentInvalidImage) pure (detectImage bytes)
  either throwIO pure (imageFromBytes bytes media)

-- | Arguments are text, optional display name and optional original MIME hint.
documentFromText :: Text -> Maybe Text -> Maybe Text -> Either DroidAttachmentError DroidDocument
documentFromText text name mime = documentFromSource (PlainTextDocument (PlainTextSource text name mime mempty))

-- | Binary documents must be PDF. The name is metadata, not a path to open.
documentFromBytes :: BS.ByteString -> Maybe Text -> Either DroidAttachmentError DroidDocument
documentFromBytes bytes name = pdfDocument bytes name Nothing

pdfDocument :: BS.ByteString -> Maybe Text -> Maybe Text -> Either DroidAttachmentError DroidDocument
pdfDocument bytes name path = do
  checkSize pdfLimit bytes
  checkPDF bytes
  pure (DroidDocument (PDFDocument (Base64PDFSource (encodeBase64 bytes) Nothing name path mempty)))

documentFromSource :: DocumentSource -> Either DroidAttachmentError DroidDocument
documentFromSource source = do
  case source of
    PDFDocument pdf -> decodeBase64 pdfLimit (pdfSourceData pdf) >>= checkPDF
    PlainTextDocument text -> do
      when (Text.length (plainTextSourceData text) > attachmentLimit) (Left (AttachmentTooLarge attachmentLimit))
      checkSize attachmentLimit (Text.encodeUtf8 (plainTextSourceData text))
  pure (DroidDocument source)

-- | Read PDF or UTF-8 text. Metadata checks detect observed file changes;
-- they do not make concurrent file reads transactional or bound filesystem latency.
documentFromFile :: FilePath -> IO DroidDocument
documentFromFile path = do
  bytes <- readAttachmentFile path
  let name = Just (Text.pack (takeFileName path))
  if "%PDF-" `BS.isPrefixOf` bytes
    then either throwIO pure (pdfDocument bytes name (Just (Text.pack path)))
    else do
      text <- either (const (throwIO AttachmentInvalidUTF8)) pure (Text.decodeUtf8' bytes)
      either throwIO pure (documentFromText text name (Just "text/plain"))

attachmentLimit, pdfLimit :: Int
attachmentLimit = 5 * 1024 * 1024
pdfLimit = 3 * 1024 * 1024

checkSize :: Int -> BS.ByteString -> Either DroidAttachmentError ()
checkSize limit bytes = when (BS.length bytes > limit) (Left (AttachmentTooLarge limit))

checkImage :: ImageMediaType -> BS.ByteString -> Either DroidAttachmentError ()
checkImage declared bytes = case detectImage bytes of
  Nothing -> Left AttachmentInvalidImage
  Just actual -> unless (declared == actual) (Left (AttachmentImageTypeMismatch actual declared))

detectImage :: BS.ByteString -> Maybe ImageMediaType
detectImage bytes
  | "\x89PNG\r\n\x1a\n" `BS.isPrefixOf` bytes = Just ImagePNG
  | "\xff\xd8\xff" `BS.isPrefixOf` bytes = Just ImageJPEG
  | "GIF87a" `BS.isPrefixOf` bytes || "GIF89a" `BS.isPrefixOf` bytes = Just ImageGIF
  | BS.length bytes >= 12 && BS.take 4 bytes == "RIFF" && BS.take 4 (BS.drop 8 bytes) == "WEBP" = Just ImageWebP
  | otherwise = Nothing

checkPDF :: BS.ByteString -> Either DroidAttachmentError ()
checkPDF bytes = unless ("%PDF-" `BS.isPrefixOf` bytes) (Left AttachmentInvalidPDF)

encodeBase64 :: BS.ByteString -> Text
encodeBase64 = Text.decodeUtf8 . Base64.encode

decodeBase64 :: Int -> Text -> Either DroidAttachmentError BS.ByteString
decodeBase64 limit text = do
  when (Text.length text > 4 * ((limit + 2) `div` 3)) (Left (AttachmentTooLarge limit))
  when (Text.any (> '\x7f') text) (Left AttachmentInvalidBase64)
  bytes <- either (const (Left AttachmentInvalidBase64)) Right (Base64.decode (Text.encodeUtf8 text))
  checkSize limit bytes
  pure bytes

readAttachmentFile :: FilePath -> IO BS.ByteString
readAttachmentFile path = readFileBytes `catch` (throwIO . AttachmentFileError)
  where
    readFileBytes = do
      when ('\0' `elem` path) (throwIO AttachmentInvalidPath)
      expected <- getFileStatus path
      checkFile expected
      -- Nonblocking open also covers replacement of the checked path by a FIFO.
      bracket (openFd path ReadOnly defaultFileFlags {nonBlock = True, cloexec = True}) closeFd $ \fd -> do
        before <- getFdStatus fd
        checkFile before
        unless (sameFileSnapshot expected before) (throwIO AttachmentChanged)
        bytes <- readBoundedFd fd (fromIntegral (fileSize before) + 1)
        after <- getFdStatus fd
        either throwIO pure (checkSize attachmentLimit bytes)
        unless (sameFileSnapshot before after && fromIntegral (BS.length bytes) == fileSize after) (throwIO AttachmentChanged)
        pure bytes
    checkFile status = do
      unless (isRegularFile status) (throwIO AttachmentNotRegular)
      when (fileSize status > fromIntegral attachmentLimit) (throwIO (AttachmentTooLarge attachmentLimit))

sameFileSnapshot :: FileStatus -> FileStatus -> Bool
sameFileSnapshot before after =
  deviceID before == deviceID after
    && fileID before == fileID after
    && fileSize before == fileSize after
    && modificationTimeHiRes before == modificationTimeHiRes after
    && statusChangeTimeHiRes before == statusChangeTimeHiRes after

readBoundedFd :: Fd -> Int -> IO BS.ByteString
readBoundedFd fd limit = BS.createUptoN limit $ \buffer ->
  let go used
        | used == limit = pure used
        | otherwise = do
            count <- fdReadBuf fd (buffer `plusPtr` used) (fromIntegral (limit - used))
            if count == 0 then pure used else go (used + fromIntegral count)
   in go 0
