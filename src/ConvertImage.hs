{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE GADTs #-}


module ConvertImage(
      Position
    , buildCsv
    ) where

import Control.Applicative((<**>))
import Control.Monad(when)
import Data.Foldable(fold)
import Codec.Picture.Types
import Data.List(intercalate)
import Data.Maybe(isNothing,fromJust)
import Data.Monoid ((<>))
import Data.Either(partitionEithers)
import Data.Char(toLower,toUpper)

import Data.ByteString.Builder

import Control.Monad.Except
import Control.Monad.Reader

import Data.Text(Text)
import qualified Data.Text as T

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Types
import Voxels.Types

type Position = Maybe (Int,Int)


-- This header goes at the top of each Blueprint and tells
-- quickfort where to start and in what mode to run

phaseUtf8 :: Phase -> Builder
phaseUtf8 Dig   = stringUtf8 "dig"
phaseUtf8 Build = stringUtf8 "build"
phaseUtf8 Place = stringUtf8 "place"
phaseUtf8 Query = stringUtf8 "query"

header :: Position -> Phase -> Int -> Builder
header pos phase width = let
  buffer = stringUtf8 $ flip replicate ',' (width - 1)

  start = maybe mempty (\p->stringUtf8 " start" <> renderTuple p) pos
  phs   = phaseUtf8 phase

    in charUtf8 '#' <> phs <> start <> buffer <> charUtf8 '\n'

renderTuple :: (Int,Int) -> Builder
renderTuple (x,y) = charUtf8 '(' <> intDec x <> charUtf8 ';'
                                 <> intDec y <> charUtf8 ')'
--------------------------------------------------------------------------------

foldIntersperse :: Monoid m => m -> V.Vector m -> m
foldIntersperse s v = V.head v <> fold [s <> e | e <- V.tail v]

imageToBuilder :: Pixel p => (p -> Builder) -> Image p -> Builder
imageToBuilder f img = pixelFold worker mempty img
  where worker acc x _ p = acc <> f p <> delim x
        delim x | x == imageWidth img - 1 = charUtf8 '\n'
                | otherwise               = charUtf8 ','


chunkToBuilder :: Pixel p => (p -> Builder) -> Builder -> FlatVoxelChunk p -> Builder
chunkToBuilder f s vox = voxelFoldSlice worker mempty vox
  where worker acc x y z p = acc <> sep <> f p <> delim
          where
          sep | x == 0 && y == 0 && z /= 0 = s
              | otherwise = mempty
          delim | x == width vox - 1 = charUtf8 '\n'
                | otherwise = charUtf8 ','


voxelsToBuilder :: FlatVoxelChunk PixelRGBA8 -> EnvR Builder
voxelsToBuilder vox = chunkToBuilder <$> lookupPixel <*> seperator <*> pure vox


buildCsv :: Int -> Position -> Phase -> FlatVoxelChunk PixelRGBA8 -> EnvR Builder
buildCsv reps pos phase vox = do

    body' <- voxelsToBuilder vox
    sep   <- seperator

    let h    = header pos phase (width vox)
        body = foldIntersperse sep $ V.replicate (reps + 1) body'

    return (h <> body)

--------------------------------------------------------------------------------
--- | experiment

--imageToVector :: Image PixelRGBA8 -> Vector PixelRGBA8
--imageToVector = fmap unpackPixel . uncurry unsafeFromForeignPtr0 . first castForeignPtr . unsafeToForeignPtr0 . imageData