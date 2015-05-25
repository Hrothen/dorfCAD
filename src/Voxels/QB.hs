{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Voxels.QB(
      decodeQBTree
    ) where

import Control.Monad(unless)
import Control.Monad.ST
import Data.Bifunctor(Bifunctor(..))
import Data.Traversable(for)
import Data.Word(Word8(..), Word32(..))
import Data.Monoid((<>))
import GHC.Generics

import Data.Serialize
import Codec.Picture.Types

import Data.Vector(Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Data.ByteString(ByteString)
import qualified  Data.ByteString as B
import Data.Text(Text, pack)

import Voxels.Types(Handedness(..), Orientation(..))
import qualified Voxels.Types as VX

data QBinfo = QBinfo { version :: (Word8,Word8,Word8,Word8)
                     , colorFormat :: ColorFormat
                     , handedness :: Handedness
                     , compression :: Compression
                     , visMaskEncoded :: Mask
                     , matrixCount :: Word32
                     }
  deriving (Show, Generic)

instance Serialize QBinfo where
  get = isolate 24 $ do
    v1 <- getWord8
    v2 <- getWord8
    v3 <- getWord8
    v4 <- getWord8
    let v = (v1,v2,v3,v4)
    unless (v == (1,1,0,0)) $
      fail ("unsupported qb version " ++ show v)
    f <- get
    h <- get
    c <- get
    m <- get
    count <- getWord32le
    return $ QBinfo v f h c m count

defaultQBinfo = QBinfo (1,1,0,0) RGBA LeftHanded None Strict 1


data Matrix a = Matrix { name :: ByteString
                       , sizes :: (Word32,Word32,Word32)
                       , position :: (Int,Int,Int)
                       , voxels :: Vector a }
  deriving Eq


----------------------------------------------------------------------------------
--- | Serialization data for the file header
---   qb files use word32 values so we can't just use Generic deriving

-- instance Serialize QBtree where
--   get = decodeQBtree
--   put = encodeQBtree defaultQBInfo

instance Serialize Handedness where
  get = toEnum . fromIntegral <$> getWord32le
  put = putWord32be . fromIntegral . fromEnum

data ColorFormat = RGBA | BGRA deriving (Show, Eq, Enum)

instance Serialize ColorFormat where
  get = toEnum . fromIntegral <$> getWord32le
  put = putWord32be . fromIntegral . fromEnum

data Compression = None | RLE deriving (Show, Eq, Enum)

instance Serialize Compression where
  get = toEnum . fromIntegral <$> getWord32le
  put = putWord32be . fromIntegral . fromEnum

data Mask = Strict | PerVox deriving (Show, Eq, Enum)

instance Serialize Mask where
  get = toEnum . fromIntegral <$> getWord32le
  put = putWord32be . fromIntegral . fromEnum

----------------------------------------------------------------------------------
--- | Encoding and decoding to FlatVoxelChunks

class QBConvertable a where
  qbEncode :: ColorFormat -> a -> Word32
  qbDecode :: ColorFormat -> Word32 -> a


instance QBConvertable PixelRGBA8 where
  qbEncode RGBA p = packPixel p
  qbEncode BGRA (PixelRGBA8 r g b a) = packPixel (PixelRGBA8 b g r a)

  qbDecode RGBA w = unpackPixel w
  qbDecode BGRA w = PixelRGBA8 r g b a
    where (PixelRGBA8 b g r a) = unpackPixel w


encodeQBTree :: QBConvertable a => VX.FlatVoxelChunk a -> ByteString
encodeQBTree = undefined

decodeQBTree :: QBConvertable a => ByteString -> Either Text (VX.FlatVoxelChunk a)
decodeQBTree = first pack . runGet getQBChunk

getQBChunk :: QBConvertable a => Get (VX.FlatVoxelChunk a)
getQBChunk = do
  header <- get
  mats' <- V.replicateM (fromIntegral (matrixCount header)) (getMatrix header)
  let mats = V.filter ((/="PAL") . name) mats' --make magicavoxel files work
  --error (show $ fmap (\(n,(Matrix s p _)) -> show (n,s,p)) mats')
  let p@(x,y,z) = inferDims mats
      h = if handedness header == LeftHanded then CGLeft else CGRight
      m = flatten p mats
  return $ VX.FlatVoxelChunk x y z h m

getMatrix (QBinfo _ fmt _ cmp _ _) = do
  name <- getByteString . fromIntegral =<< getWord8
  --error (show name)
  -- getWord8 >>= skip . fromIntegral -- skip the matrix name, we don't use it
  sizeX <- getWord32le
  sizeY <- getWord32le
  sizeZ <- getWord32le

  let size = fromIntegral $ sizeX * sizeY * sizeZ

  posX <- getPosInt
  posY <- getPosInt
  posZ <- getPosInt

  matrix <- case cmp of
    None -> V.generateM size (\i-> qbDecode fmt <$> label ("pixel " ++ show i ++ " of " ++ show size) getWord32le)
    RLE  -> getMatrixRLE size fmt

  return $ Matrix name (sizeX,sizeY,sizeZ) (posX,posY,posZ) matrix

--readChunk (x',y'z') = do
--  let indicies = [x + y*x' + z*x'*y' | z <- [0..z'-1], y <- [0..y'-1], x <- [0..x'-1]


--
getPosInt :: Get Int
getPosInt = fromIntegral <$> getWord32le

--------------------------------------------------------------------------------
-- FIXME: Everything below this point is terrible

getMatrixRLE :: QBConvertable a => Int -> ColorFormat -> Get (Vector a)
getMatrixRLE sz fmt = go 0
  where go len | len == sz = return mempty
               | otherwise = getWord32le >>= \case
               6 -> go len
               2 -> do
                 size <- fromIntegral <$> getWord32le
                 pix  <- fmap (qbDecode fmt) getWord32le
                 let v = V.replicate size pix -- (qbDecode fmt dta)
                 fmap (mappend v) (go (len + size))
               d -> fmap (mappend ( V.singleton(qbDecode fmt d))) (go (len + 1))


inferDims :: Vector (Matrix a) -> (Word32,Word32,Word32)
inferDims ms | V.null ms = (0,0,0)
             | V.length ms == 1 = sizes $ V.head ms
             | otherwise = condense $ fmap toRanges ms
  where toRanges (Matrix _ (s1,s2,s3) (p1,p2,p3) _) = ( (p1,p1 + fromIntegral s1),
                                                        (p2,p2 + fromIntegral s2),
                                                        (p3,p3 + fromIntegral s3) )
        condense rgs = (fromIntegral (abs (x1-x2)),
                        fromIntegral (abs (y1-y2)),
                        fromIntegral (abs (z1-z2)))
          where ((x1,x2),(y1,y2),(z1,z2)) = V.foldl1' bar rgs

        bar (x1,y1,z1) (x2,y2,z2) = (foo x1 x2, foo y1 y2, foo z1 z2)
        foo (a1,b1) (a2,b2) = (min a1 a2, max b1 b2)

flatten :: QBConvertable a => (Word32,Word32,Word32) -> Vector (Matrix a) -> Vector a
flatten (x',y',z') mats | null mats = mempty
                        | V.length mats == 1 = voxels $ V.head mats
                        | otherwise = runST $ do
  let (x,y,z) = (fromIntegral x', fromIntegral y', fromIntegral z')

  chunk <- MV.replicate (x*y*z) (qbDecode RGBA 0)

  for mats $ \m ->
    V.sequence $ V.imap (\i v-> MV.unsafeWrite chunk (localToGlobal (x,y,z) i m) v) $ (voxels m)

  V.unsafeFreeze chunk


-- localToGlobal :: (Int,Int,Int) -> Int -> Matrix a -> Int
-- localToGlobal (w,h,d) i (Matrix (sx,sy,sz) (px,py,pz) _) = z'*w*h + y'*w + x'
--  where (x',y',z') = (x+xi,y+yi,z+zi)
--        (zi,y'')   = i `quotRem` (sx * sy)
--        xi = i `mod` sx
--        yi = y'' `quot` sx
--         -- zi = i `quot` (sx * sy)

localToGlobal (w,h,d) i (Matrix _ (mw,mh,md) (mx,my,mz) _) = z*w*h + y*w + x
  where (x,y,z) = (x'+mx,y'+my,z'+mz)
        (x',y',z') = VX.indexTo3D i (fromIntegral mw) (fromIntegral mh) (fromIntegral md)