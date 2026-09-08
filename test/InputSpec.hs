{-# LANGUAGE OverloadedStrings #-}

module InputSpec (inputTests) where

import Control.Exception (bracket, throwIO, try)
import Control.Monad (void)
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Word (Word8)
import Factory.Droid.Input
import Factory.Droid.Schema.Content
import ProcessSpec (bounded)
import System.Directory (removePathForcibly)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (createNamedPipe, createSymbolicLink)
import System.Posix.Temp (mkdtemp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

inputTests :: TestTree
inputTests =
  testGroup
    "Validated local attachments"
    [ testGroup
        "recognized image signatures"
        [ testCase (show media <> show bytes) $ do
            image <- either throwIO pure (imageFromBytes bytes media)
            imageSourceMediaType (droidImageSource image) @?= media
            imageFromSource (droidImageSource image) @?= Right image
        | (media, bytes) <- [(ImagePNG, png), (ImageJPEG, "\xff\xd8\xff"), (ImageGIF, "GIF87a"), (ImageGIF, "GIF89a"), (ImageWebP, "RIFF1234WEBP")]
        ],
      testProperty "encoded binary image data validates without changing its source" $ \(payload :: [Word8]) ->
        case imageFromBytes (png <> BS.pack payload) ImagePNG of
          Left _ -> False
          Right image -> imageFromSource (droidImageSource image) == Right image,
      testCase "unknown and short image signatures fail" $ do
        void (imageFromBytes "not an image" ImagePNG) @?= Left AttachmentInvalidImage
        void (imageFromBytes "RIFF1234WEB" ImageWebP) @?= Left AttachmentInvalidImage,
      testCase "declared image MIME must match its signature" $
        void (imageFromBytes png ImageJPEG) @?= Left (AttachmentImageTypeMismatch ImagePNG ImageJPEG),
      testCase "image source extensions and exact data are retained" $ do
        let source = Base64ImageSource "iVBORw0KGgo=" ImagePNG (KeyMap.singleton "private" (String "metadata"))
        image <- either throwIO pure (imageFromSource source)
        droidImageSource image @?= source,
      testGroup
        "strict image Base64"
        [ testCase (Text.unpack label) $
            void (imageFromSource (Base64ImageSource encoded ImagePNG mempty)) @?= Left AttachmentInvalidBase64
        | (label, encoded) <- [("newline", "iVBORw0KGgo=\n"), ("unpadded", "iVBORw0KGgo"), ("alphabet", "iVBORw0KGg_="), ("noncanonical pad bits", "iVBORw0KGgp="), ("unicode", "سلام"), ("excess padding", "iVBORw0KGgo==")]
        ],
      testCase "decoded image bytes need an image signature" $
        void (imageFromSource (Base64ImageSource "YWJj" ImagePNG mempty)) @?= Left AttachmentInvalidImage,
      testCase "image byte bounds are inclusive" $ do
        void (imageFromBytes (sizedImage limit) ImagePNG) @?= Right ()
        void (imageFromBytes (sizedImage (limit + 1)) ImagePNG) @?= Left (AttachmentTooLarge limit),
      testCase "oversized encoded image input fails before decoding" $
        void (imageFromSource (Base64ImageSource (Text.replicate (4 * ((limit + 2) `div` 3) + 1) "A") ImagePNG mempty)) @?= Left (AttachmentTooLarge limit),
      testCase "decoded size cannot evade the encoded image bound" $ do
        oversized <- either throwIO pure (imageFromBytes (sizedImage limit) ImagePNG)
        let source = droidImageSource oversized
            encoded = Text.dropEnd 4 (imageSourceData source) <> "AAAA"
        void (imageFromSource (source {imageSourceData = encoded})) @?= Left (AttachmentTooLarge limit),
      testCase "text metadata and empty text retain their spelling" $ do
        document <- either throwIO pure (documentFromText "" (Just "") (Just "text/custom"))
        droidDocumentSource document @?= PlainTextDocument (PlainTextSource "" (Just "") (Just "text/custom") mempty),
      testCase "text byte limit is UTF-8, not character count" $ do
        let text = Text.replicate (limit `div` 4) "😀"
        void (documentFromText text Nothing Nothing) @?= Right ()
        void (documentFromText (text <> "x") Nothing Nothing) @?= Left (AttachmentTooLarge limit)
        void (documentFromText (Text.replicate (limit + 1) "a") Nothing Nothing) @?= Left (AttachmentTooLarge limit),
      testCase "plain-text source extensions are retained" $ do
        let source = PlainTextDocument (PlainTextSource "a\0سلام" Nothing (Just "") (KeyMap.singleton "extra" Null))
        document <- either throwIO pure (documentFromSource source)
        droidDocumentSource document @?= source,
      testCase "binary document constructors require PDF signatures" $ do
        void (documentFromBytes "plain text" Nothing) @?= Left AttachmentInvalidPDF
        document <- either throwIO pure (documentFromBytes "%PDF-" (Just "name.pdf"))
        droidDocumentSource document @?= PDFDocument (Base64PDFSource "JVBERi0=" Nothing (Just "name.pdf") Nothing mempty),
      testCase "PDF byte bounds are inclusive" $ do
        void (documentFromBytes (sizedPDF pdfLimit) Nothing) @?= Right ()
        void (documentFromBytes (sizedPDF (pdfLimit + 1)) Nothing) @?= Left (AttachmentTooLarge pdfLimit),
      testCase "PDF source metadata is preserved without extraction or path access" $ do
        let source = PDFDocument (Base64PDFSource "JVBERi0=" (Just "parsed text") (Just "") (Just "/not/read") (KeyMap.singleton "extra" (Number 7)))
        document <- either throwIO pure (documentFromSource source)
        droidDocumentSource document @?= source,
      testCase "PDF source decoding rejects bad Base64 and signatures" $ do
        let source text = PDFDocument (Base64PDFSource text Nothing Nothing Nothing mempty)
        void (documentFromSource (source "JVBERi1=")) @?= Left AttachmentInvalidBase64
        void (documentFromSource (source "not base64")) @?= Left AttachmentInvalidBase64
        void (documentFromSource (source "YWJj")) @?= Left AttachmentInvalidPDF
        void (documentFromSource (source (Text.replicate (4 * ((pdfLimit + 2) `div` 3) + 1) "A"))) @?= Left (AttachmentTooLarge pdfLimit),
      testCase "image files are read as binary data and symlinks are permitted" $ bounded $ withInputDirectory $ \directory -> do
        let path = directory <> "/image.bin"
            link = directory <> "/image-link"
        BS.writeFile path png
        createSymbolicLink path link
        image <- imageFromFile link
        imageSourceData (droidImageSource image) @?= "iVBORw0KGgo=",
      testCase "PDF file names and caller paths are metadata" $ bounded $ withInputDirectory $ \directory -> do
        let path = directory <> "/résumé.pdf"
        BS.writeFile path "%PDF-"
        document <- documentFromFile path
        droidDocumentSource document @?= PDFDocument (Base64PDFSource "JVBERi0=" Nothing (Just "résumé.pdf") (Just (Text.pack path)) mempty),
      testCase "text files use strict UTF-8 with a text/plain MIME hint" $ bounded $ withInputDirectory $ \directory -> do
        let path = directory <> "/note.txt"
        BS.writeFile path "plain\0text"
        document <- documentFromFile path
        droidDocumentSource document @?= PlainTextDocument (PlainTextSource "plain\0text" (Just "note.txt") (Just "text/plain") mempty)
        BS.writeFile path (BS.pack [255, 254])
        expectAttachmentError AttachmentInvalidUTF8 (documentFromFile path),
      testCase "regular files are size-checked before reading" $ bounded $ withInputDirectory $ \directory -> do
        let path = directory <> "/oversized"
        BS.writeFile path (sizedImage (limit + 1))
        expectAttachmentError (AttachmentTooLarge limit) (imageFromFile path)
        BS.writeFile path (sizedPDF (pdfLimit + 1))
        expectAttachmentError (AttachmentTooLarge pdfLimit) (documentFromFile path),
      testCase "directories and FIFOs are rejected without waiting for writers" $ bounded $ withInputDirectory $ \directory -> do
        expectAttachmentError AttachmentNotRegular (imageFromFile directory)
        let fifo = directory <> "/fifo"
        createNamedPipe fifo 0o600
        expectAttachmentError AttachmentNotRegular (documentFromFile fifo),
      testCase "NUL paths cannot be truncated into a different existing file" $ bounded $ withInputDirectory $ \directory -> do
        let path = directory <> "/image"
        BS.writeFile path png
        expectAttachmentError AttachmentInvalidPath (imageFromFile (path <> "\0suffix")),
      testCase "file failures retain their underlying IO classification" $ bounded $ withInputDirectory $ \directory -> do
        outcome <- try @DroidAttachmentError (imageFromFile (directory <> "/missing"))
        case outcome of
          Left (AttachmentFileError cause) | isDoesNotExistError cause -> pure ()
          _ -> assertFailure "Expected a missing-file cause",
      testCase "attachment values and errors redact payloads" $ do
        image <- either throwIO pure (imageFromBytes png ImagePNG)
        document <- either throwIO pure (documentFromText "private text" Nothing Nothing)
        show image @?= "DroidImage <redacted>"
        show document @?= "DroidDocument <redacted>"
        show ((droidInput "private prompt") {inputImages = [image], inputDocuments = [document]}) @?= "DroidInput <redacted>"
        show (AttachmentFileError (userError "private path")) @?= "DroidAttachmentError <redacted>"
    ]

png :: BS.ByteString
png = "\x89PNG\r\n\x1a\n"

limit, pdfLimit :: Int
limit = 5 * 1024 * 1024
pdfLimit = 3 * 1024 * 1024

sizedImage :: Int -> BS.ByteString
sizedImage size = png <> BS.replicate (size - BS.length png) 0

sizedPDF :: Int -> BS.ByteString
sizedPDF size = "%PDF-" <> BS.replicate (size - 5) 0

withInputDirectory :: (FilePath -> IO a) -> IO a
withInputDirectory = bracket (mkdtemp "/tmp/droid-input-test-") removePathForcibly

expectAttachmentError :: DroidAttachmentError -> IO a -> IO ()
expectAttachmentError expected action = do
  outcome <- try @DroidAttachmentError action
  void outcome @?= Left expected
