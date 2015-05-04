{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
DeriveDataTypeable, OverloadedStrings, StandaloneDeriving #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE GADTs #-}
module ConvertImage(
      Position
    , buildCsv
    ) where

import Control.Applicative((<**>))
import Control.Monad(when)
import Data.Typeable(Typeable)
import Data.Data(Data)
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

type Position = Maybe (Int,Int)


-- This header goes at the top of each Blueprint and tells
-- quickfort where to start and in what mode to run

phaseUtf8 :: Phase -> Builder
phaseUtf8 Dig   = stringUtf8 "dig"
phaseUtf8 Build = stringUtf8 "build"
phaseUtf8 Place = stringUtf8 "place"
phaseUtf8 Query = stringUtf8 "query"

headerE :: Position -> Phase -> EnvR Builder
headerE pos phase = do
    buffer <- stringUtf8 . flip replicate ',' <$> width

    let start = maybe mempty (\p->stringUtf8 " start" <> renderTuple p) pos
        phs   = phaseUtf8 phase

    return $ charUtf8 '#' <> phs <> start <> buffer <> charUtf8 '\n'

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


imagesToBuilder :: V.Vector DynamicImage -> EnvR Builder
imagesToBuilder images = do
    sep    <- seperator
    pxlook <- lookupPixel

    imgs <- mapM validateImage images

    let slices    = fmap (imageToBuilder pxlook) imgs
        blueprint = foldIntersperse sep slices

    return blueprint


-- extracts an Image from a DynamicImage that matches
-- the required dimensions and format
validateImage :: DynamicImage -> EnvR (Image PixelRGBA8)
validateImage i = validateImageSize i >> validateImageFormat i


validateImageSize :: DynamicImage -> EnvR ()
validateImageSize i = do
    dims <- (,) <$> width <*> height
    let imgDims = (dynamicMap imageWidth i, dynamicMap imageHeight i)
    when (dims /= imgDims) $ throwError "Error: not all images have the same dimensions"

-- Convert an Image to RGBA8 or throw an error if the format doesn't
-- allow upscaling
validateImageFormat :: DynamicImage -> EnvR (Image PixelRGBA8)
validateImageFormat (ImageY8    a) = return $ promoteImage a
validateImageFormat (ImageYA8   a) = return $ promoteImage a
validateImageFormat (ImageRGB8  a) = return $ promoteImage a
validateImageFormat (ImageRGBA8 a) = return a
validateImageFormat _              = throwError "Error: one or more images are not encoded in RGBA8 color"


buildCsv :: Int -> Position -> Phase -> V.Vector DynamicImage -> EnvR Builder
buildCsv reps pos phase imgs = do
    h     <- headerE pos phase
    body' <- imagesToBuilder imgs
    sep   <- seperator

    let body = foldIntersperse sep $ V.replicate (reps + 1) body'

    return (h <> body)
