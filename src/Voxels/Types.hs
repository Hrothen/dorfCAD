module Voxels.Types(
      VoxelSpace(..)
    , FlatVoxelChunk(..)
    , Handedness(..)
    , Orientation(..)
    , indexTo3D
    , voxelFoldSlice
    ) where

import Data.Word(Word32(..))
import Codec.Picture.Types

import Data.Vector(Vector(..))
import qualified Data.Vector as V

data Handedness = LeftHanded | RightHanded deriving (Show, Eq, Enum)

data Orientation = CGLeft  -- left handed with y axis pointing "up"
                 | CGRight -- right handed with y axis pointing "down"
                 | ULeft   -- left handed with z axis pointing "up"
                 | URight  -- right handed with z axis pointing "down"
  deriving (Show, Eq)

data FlatVoxelChunk a = FlatVoxelChunk { voxelWidth  :: Word32 --x
                                       , voxelHeight :: Word32 --y
                                       , voxelDepth  :: Word32 --z
                                       , orientation :: Orientation
                                       , voxelData   :: Vector a }

-- fold over a Voxel Chunk as a collection of horizontal slices
-- this is not a good implementation
voxelFoldSlice :: (a -> Int -> Int -> Int -> b -> a) -> a -> FlatVoxelChunk b -> a
voxelFoldSlice f acc v = case orientation v of
  CGLeft -> cgfold f acc v
  CGRight -> cgfold f acc v
  ULeft -> ifold' f acc v
  URight -> ifold' f acc v


cgfold f a (FlatVoxelChunk w h d o v) =
  let transformedMat = V.fromList [v V.! fromIntegral (x + z*w + y*w*d) | z <- [0..d-1], x <- [0..w-1], y <- [0..h-1]]
  in ifold' f a (FlatVoxelChunk w d h o transformedMat)


indexTo3D :: Int -> Int -> Int -> Int -> (Int,Int,Int)
indexTo3D i w h d = (x,y,z)
  where x = i `mod` w
        (z,y') = i `quotRem` (w * h)
        y = y' `quot` h


class VoxelSpace v where
  width  :: v a -> Int
  height :: v a -> Int
  depth  :: v a -> Int
  ifold' :: (a -> Int -> Int -> Int -> b -> a) -> a -> v b -> a


instance VoxelSpace FlatVoxelChunk where
  width  = fromIntegral . voxelWidth
  height = fromIntegral . voxelHeight
  depth  = fromIntegral . voxelDepth

  ifold' f a vs = V.ifoldl' f' a (voxelData vs)
    where
      f' acc i v = let (x,y,z) = indexTo3D i (width vs) (height vs) (depth vs)
                   in f acc x y z v
