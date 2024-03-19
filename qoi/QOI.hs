{-# LANGUAGE RecordWildCards, StrictData, TypeApplications, BinaryLiterals, BangPatterns #-}

module QOI 
( QoiImage
, bitmapOfQOI
, qoiFromFile
, imageDim
)
where


import Data.Binary
import Data.Binary.Get
import Data.Bits
import Data.Functor
import Data.Array
import Control.Applicative
import Control.Monad.Fix
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import Data.ByteString.Builder

import qualified Data.ByteString as B

data QoiHeader = Header
  { width       :: Word32
  , height      :: Word32
  , channels    :: Word8
  , colourSpace :: Word8
  } deriving (Show)


instance Binary QoiHeader where
  get = do
    start <- get @Word32
    if start /= 0x716F6966
       then fail $ "Wrong Magic! got: " <> show start
       else Header
        <$> get
        <*> get
        <*> get
        <*> get

  put Header{..} = do
    put width
    put height
    put channels
    put colourSpace


data QoiChunk = 
    QoiOpRGB   { red   :: Word8 
               , green :: Word8
               , blue  :: Word8
               }
  | QoiOpRGBA  { red   :: Word8
               , green :: Word8
               , blue  :: Word8
               , alpha :: Word8
               }
  | QoiOpIndex { index :: Word8
               }
  | QoiOpDiff  { diffRed   :: Word8
               , diffGreen :: Word8
               , diffBlue  :: Word8
               }
  | QoiOpRun   { run :: Word8
               } deriving (Show)


instance Binary QoiChunk where
  put = error "Encoder not written"

  get = do
    b1 <- get @Word8
    case b1 of
      0xFE -> QoiOpRGB <$> get <*> get <*> get
      0xFF -> QoiOpRGBA <$> get <*> get <*> get <*> get
      _ -> case b1 `shift` (-6) of
             0 -> return $! QoiOpIndex $ b1 .&. 0x3F
             1 -> return $! QoiOpDiff
               (((b1 .&. 0b00110000) `shiftR` 4) - 2)
               (((b1 .&. 0b00001100) `shiftR` 2) - 2)
               ((b1 .&. 0b00000011) - 2)
             2 -> do
               b2 <- get @Word8
               let !diffGreen = (b1 .&. 0x3F) - 32
                   !diffRed   = ((b2 .&. 0xF0) `shiftR` 4) + diffGreen - 8
                   !diffBlue  = (b2 .&. 0x0F) + diffGreen - 8
               return QoiOpDiff{..}
             3 -> return $! QoiOpRun (b1 .&. 0x3F)
             _ -> error "Impossible"
               

data QoiImage = Image QoiHeader [QoiChunk] deriving Show

data QoiEnd = End

instance Binary QoiEnd where
  put End = error "Encoder not written"
  get = do
    bytes <- get @Word64
    if bytes == 1 then return End
                  else empty

instance Binary QoiImage where
  put = error "Encoder not written"

  get = do
    header <- get @QoiHeader
    chunks <- fix $ \loop -> 
          lookAhead (get @QoiEnd $> [])
      <|> liftA2 (:) (get @QoiChunk) loop
    return $! Image header chunks



bmpFormat :: BitmapFormat
bmpFormat = BitmapFormat
  { rowOrder    = TopToBottom
  , pixelFormat = PxRGBA
  }

data Pixel = Pixel
  { r :: Word8
  , g :: Word8
  , b :: Word8
  , a :: Word8
  }

indexPosition :: Pixel -> Word8
indexPosition Pixel{..} = fromInteger $! ((r' * 3) + (g' * 5) + (b' * 7) + (a' * 11)) `mod` 64
  where r' = fromIntegral r
        g' = fromIntegral g
        b' = fromIntegral b
        a' = fromIntegral a


qoiFromFile :: FilePath -> IO QoiImage
qoiFromFile = decodeFile

imageDim :: QoiImage -> (Int, Int)
imageDim (Image Header{..} _) = (fromIntegral width, fromIntegral height)

bitmapOfQOI :: QoiImage -> Picture
bitmapOfQOI (Image Header{..} chunks) = bitmapOfByteString (fromIntegral width) (fromIntegral height) bmpFormat (buildBS chunks) True
  where initialPixel :: Pixel
        initialPixel = Pixel 0 0 0 255

        initialArray :: Array Word8 Pixel
        initialArray = array (0,63) [ (i, Pixel 0 0 0 0) | i <- [0..63]]

        buildBS :: [QoiChunk] -> B.ByteString
        buildBS _ = B.toStrict $! toLazyByteString $! pixels2bsb $! chunkToPixel initialArray initialPixel chunks
        {-# INLINE buildBS #-}

        pixel2bsb :: Pixel -> Builder
        pixel2bsb Pixel{..} = word8 r <> word8 g <> word8 b <> word8 a

        pixels2bsb :: [Pixel] -> Builder
        pixels2bsb = mconcat . map pixel2bsb

        chunkToPixel :: Array Word8 Pixel -> Pixel -> [QoiChunk] -> [Pixel]
        chunkToPixel _ _ [] = []
        chunkToPixel cache _  (QoiOpRGB r g b : xs) = 
          let !p = Pixel r g b 255 in
          let !cache' = cache // [(indexPosition p, p)]
           in p : chunkToPixel cache' p xs
        chunkToPixel cache _  (QoiOpRGBA r g b a : xs) = 
          let !p = Pixel{..} in
          let !cache' = cache // [(indexPosition p, p)]
           in p : chunkToPixel cache' p xs
        chunkToPixel cache _ (QoiOpIndex ind : xs) =
          let p = cache ! ind in
          let !cache' = cache // [(indexPosition p, p)]
           in p : chunkToPixel cache' p xs
        chunkToPixel cache lp (QoiOpDiff dr dg db : xs) = 
          let !p = Pixel (r lp + dr) (g lp + dg) (b lp + db) (a lp) in
          let !cache' = cache // [(indexPosition p, p)]
           in p : chunkToPixel cache' p xs
        chunkToPixel cache lp (QoiOpRun 0 : xs) = lp : chunkToPixel cache lp xs
        chunkToPixel cache lp (QoiOpRun n : xs) = lp : chunkToPixel cache lp ((QoiOpRun $! n - 1) : xs)
        {-# inline chunkToPixel #-}
