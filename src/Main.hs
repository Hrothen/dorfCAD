{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module Main(main) where

import Prelude hiding (repeat)
import Paths_dorfCAD

import Data.Bifunctor
import Data.Either(rights)
import Data.List(intercalate)
import Data.Char(toLower)
import Data.Monoid((<>))
import Data.Foldable(toList)
import Data.Traversable(for)
import Data.Maybe(fromJust, maybe)
import Codec.Picture(decodeImage)
import Codec.Picture.Types

import Data.ByteString(ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder

import System.Console.CmdArgs
import System.IO(IOMode(WriteMode),withFile)
import Control.Monad

import System.FilePath

import qualified Data.Set as S
import qualified Data.Map as M

import Data.Text(Text)
import qualified  Data.Text as T

import Data.Vector(Vector)
import qualified Data.Vector as V

import ConvertImage(Position,buildCsv)
import CommandLine(Opts(..), Orientation(..), InputMode(..),
                   options, phases, loadConfigFiles)
import Types
import Voxels.Types
import Voxels.QB


main = cmdArgs options >>= dorfMain

{-dorfMain opts = do
  images <- mapM B.readFile (V.fromList $ input opts)
  (aliases, configF) <- loadConfigFiles opts

  let dict = decodePhaseMap aliases configF

  case (mapM decodeImage images,dict) of
       (Left e1,Left e2) -> putStrLn e1 >> print e2
       (Left e1,Right _) -> putStrLn e1
       (Right _,Left e2) -> print e2
       (Right i,Right d) -> go i d opts-}

dorfMain opts = do
  dta <- mapM B.readFile (V.fromList $ input opts)
  (aliases, configF) <- loadConfigFiles opts

  {-let dict = decodePhaseMap aliases configF

      csv' = case mode opts of
        VoxelMode -> loadVoxels dta >>= voxCSV opts dict
        ImageMode -> loadImages dta >>= voxCSV opts dict-}

  let csv' = do{
    dict <- errText (decodePhaseMap aliases configF);

    case mode opts of
      VoxelMode -> loadVoxels dta >>= voxCSV opts dict;
      ImageMode -> loadImages dta >>= voxCSV opts dict
      }

  case csv' of
    Left  err -> print err
    Right csv -> writeBlueprints opts csv

-- convert error types
errText :: Show a => Either a b -> Either Text b
errText = first (T.pack . show)

loadVoxels :: Vector ByteString -> Either Text (FlatVoxelChunk PixelRGBA8)
loadVoxels dta | length dta == 1 = errText $ decodeQBTree $ V.head dta
               | otherwise = Left "Error: too many input files"


loadImages :: Vector ByteString -> Either Text (FlatVoxelChunk PixelRGBA8)
loadImages = mapM (errText . decodeImage)
                  >=> validateImages
                  >=> return . imagesToChunk


imagesToChunk :: Pixel a => Vector (Image a) -> FlatVoxelChunk a
imagesToChunk imgs = FlatVoxelChunk h w d ULeft voxels
  where
    d = fromIntegral $ V.length imgs
    w = fromIntegral $ imageWidth $ V.head imgs
    h = fromIntegral $ imageHeight $ V.head imgs

    voxels = foldMap imageToVector imgs

    imageToVector = pixelFoldMap V.singleton -- slow



validateImages :: Vector DynamicImage -> Either Text (Vector (Image PixelRGBA8))
validateImages images = let w = dynamicMap imageWidth  $ V.head images
                            h = dynamicMap imageHeight $ V.head images
                        in mapM (validateImage (w,h)) images

-- extracts an Image from a DynamicImage that matches
-- the required dimensions and format
validateImage :: (Int,Int) -> DynamicImage -> Either Text (Image PixelRGBA8)
validateImage dims i = validateImageSize dims i >> validateImageFormat i


validateImageSize :: (Int,Int) -> DynamicImage -> Either Text ()
validateImageSize dims i = do
    let imgDims = (dynamicMap imageWidth i, dynamicMap imageHeight i)
    if (dims /= imgDims)
      then Left "Error: not all images have the same dimensions"
      else Right ()
-- Convert an Image to RGBA8 or throw an error if the format doesn't
-- allow upscaling
validateImageFormat :: DynamicImage -> Either Text (Image PixelRGBA8)
validateImageFormat (ImageY8    a) = return $ promoteImage a
validateImageFormat (ImageYA8   a) = return $ promoteImage a
validateImageFormat (ImageRGB8  a) = return $ promoteImage a
validateImageFormat (ImageRGBA8 a) = return a
validateImageFormat _              = Left "Error: one or more images are not encoded in RGBA8 color"


voxCSV :: Opts -> PhaseMap -> FlatVoxelChunk PixelRGBA8 -> Either Text (Vector Builder)
voxCSV opts dict vxs = do
  let reps = repeat opts
      pos  = getPosition opts vxs

  for (V.fromList (phases opts)) $ \p->
    let b = buildCsv reps pos p vxs
        e = setupEnv vxs opts (fromJust $ M.lookup p dict)
    in runEnvR b e


{-go :: V.Vector DynamicImage -> PhaseMap -> Opts -> IO ()
go images dict opts = do
   let
       reps = repeat opts
       pos = getPosition opts images

       csvs = for (phases opts) $ \p->
           let b = buildCsv reps pos p images
               e = setupEnv images opts (fromJust $ M.lookup p dict)
           in runEnvR b e

   case csvs of
       Left err -> print err
       Right cs -> writeBlueprints opts cs-}


-- setupEnv :: V.Vector DynamicImage -> Opts -> PixelMap -> Env
{-setupEnv images opts pmap =
    let img = V.head images
        w = dynamicMap imageWidth img
        h = dynamicMap imageHeight img

        sep = case order opts of
          FromBottom -> stringUtf8 $ "#<" <> replicate w ',' <> "\n"
          FromTop    -> stringUtf8 $ "#>" <> replicate w ',' <> "\n"

    in Env w h pmap sep-}
setupEnv :: VoxelSpace v => v a -> Opts -> PixelMap -> Env
setupEnv vox opts pmap = Env pmap sep
  where w = width vox

        sep = case order opts of
          FromBottom -> stringUtf8 $ "#<" <> replicate (w - 1) ',' <> "\n"
          FromTop    -> stringUtf8 $ "#>" <> replicate (w - 1) ',' <> "\n"


{-getPosition :: Opts -> V.Vector DynamicImage -> Maybe (Int,Int)
getPosition opts imgs = do
    (x,y,z) <- start opts

    let r = repeat opts
        dir = order opts
        height = dynamicMap imageHeight $ V.head imgs

    if z > V.length imgs * (1 + repeat opts) || z < 1
    then return (x,y) --If the z level is invalid just ignore it
    else
      if not (absPos opts) then return (x, y + advance height z)
      else case order opts of
          FromBottom -> return (x, y + advance height (invert z r imgs))
          FromTop    -> return (x, y + advance height z)-}

getPosition :: VoxelSpace v => Opts -> v a -> Maybe (Int,Int)
getPosition opts vox = do
    (x,y,z) <- start opts

    let r = repeat opts
        dir = order opts
        h = height vox
        d = depth vox

    if z > d * (1 + repeat opts) || z < 1
    then return (x,y) --If the z level is invalid just ignore it
    else
      if not (absPos opts) then return (x, y + advance h z)
      else case order opts of
          FromBottom -> return (x, y + advance h (invert z r d))
          FromTop    -> return (x, y + advance h z)

advance :: Int -> Int -> Int
advance h z = (z - 1) * (h + 1)

-- flip z around
invert :: Int -> Int -> Int -> Int
invert z r d = (d * (r + 1)) - (z - 1)

writeBlueprints :: Foldable t => Opts -> t Builder -> IO ()
writeBlueprints opts bps = do
    let suffix p = '-': map toLower (show p)

        filename = case output opts of
                     "" -> takeBaseName (head $ input opts)
                     n  -> n

        outfile p = filename <> suffix p <.> "csv"

        files = fmap outfile (phases opts)

    mapM_ (\(n,f)-> withFile n WriteMode (flip hPutBuilder f)) $ zip files (toList bps)


