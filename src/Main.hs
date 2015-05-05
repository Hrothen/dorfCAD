{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module Main(main) where

import Prelude hiding (repeat)
import Paths_dorfCAD

import Data.Either(rights)
import Data.List(intercalate)
import Data.Char(toLower)
import Data.Monoid((<>))
import Data.Foldable(toList)
import Data.Traversable(for)
import Data.Maybe(fromJust, maybe)
import Codec.Picture(decodeImage)
import Codec.Picture.Types
import qualified Data.ByteString as B
import Data.ByteString.Builder
import System.Console.CmdArgs
import System.IO(IOMode(WriteMode),withFile)
import Control.Monad

import System.FilePath

import qualified Data.Set as S
import qualified Data.Map as M

import qualified Data.Vector as V

import ConvertImage(Position,buildCsv)
import CommandLine(Opts(..), Orientation(..),
                   options, phases, loadConfigFiles)
import Types


main = cmdArgs options >>= dorfMain

dorfMain opts = do
  images <- mapM B.readFile (V.fromList $ input opts)
  (aliases, configF) <- loadConfigFiles opts

  let dict = decodePhaseMap aliases configF

  case (mapM decodeImage images,dict) of
       (Left e1,Left e2) -> putStrLn e1 >> print e2
       (Left e1,Right _) -> putStrLn e1
       (Right _,Left e2) -> print e2
       (Right i,Right d) -> go i d opts


go :: V.Vector DynamicImage -> PhaseMap -> Opts -> IO ()
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
       Right cs -> writeBlueprints opts cs


setupEnv :: V.Vector DynamicImage -> Opts -> PixelMap -> Env
setupEnv images opts pmap =
    let img = V.head images
        w = dynamicMap imageWidth img
        h = dynamicMap imageHeight img

        sep = case order opts of
          FromBottom -> stringUtf8 $ "#<" <> replicate w ',' <> "\n"
          FromTop    -> stringUtf8 $ "#>" <> replicate w ',' <> "\n"

    in Env w h pmap sep


getPosition :: Opts -> V.Vector DynamicImage -> Maybe (Int,Int)
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
          FromTop    -> return (x, y + advance height z)

advance :: Int -> Int -> Int
advance h z = (z - 1) * (h + 1)

-- flip z around
invert :: Int -> Int -> V.Vector a -> Int
invert z r v = (V.length v * (r + 1)) - (z - 1)

writeBlueprints :: Foldable t => Opts -> t Builder -> IO ()
writeBlueprints opts bps = do
    let suffix p = '-': map toLower (show p)

        filename = case output opts of
                     "" -> takeBaseName (head $ input opts)
                     n  -> n

        outfile p = filename <> suffix p <.> "csv"

        files = fmap outfile (phases opts)

    mapM_ (\(n,f)-> withFile n WriteMode (flip hPutBuilder f)) $ zip files (toList bps)


