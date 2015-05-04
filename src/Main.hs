{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Main(main) where

import Prelude hiding (repeat)
import Paths_dorfCAD

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
import Types


data Orientation = FromBottom | FromTop
  deriving (Data, Eq, Show)

data Opts = Opts { start   :: Maybe (Int,Int,Int)
                 , absPos  :: Bool
                 , input   :: [String]
                 , output  :: String
                 , phases_ :: [Phase]
                 , repeat  :: Int
                 , config  :: Maybe String
                 , order   :: Orientation }
    deriving (Typeable, Data, Eq, Show)

options = Opts{ start  = def
                       &= typ "X,Y,Z"
                       &= help "Start position of the macro."
              , absPos = True
                       &= name "absolute-position"
                       &= explicit
                       &= typ "TRUE|FALSE"
                       &= help "use top left corner as origin regardless of other flags"
              , input  = def
                       &= args
                       &= typFile
              , output = def
                       &= typFile
                       &= help "Output filename"
              , phases_ = enum [ [] &= ignore
                               , [Dig] &= name "dig"
                               , [Build] &= name "build"
                               , [Place] &= name "place"
                               , [Query] &= name "query" ]
                       &= typ "[All|Dig|Build|Place|Query]"
                       &= explicit
                       &= help "Phase to create a blueprint for"
              , repeat = 0
                       &= typ "INTEGER"
                       &= help "Optional, specifies a number of times to repeat the blueprint"
              , config = def
                       &= typFile
                       &= help "Specify a config file to use instead of the default"
              , order  = enum [ FromBottom &= name "from-bottom"
                                           &= help "Order files from bottom to top"
                              , FromTop    &= name "from-top"
                                           &= help "Oder files from top to bottom" ]
                       &= explicit
              }
              &= program "mkblueprint"
              &= summary "dorfCAD v1.2, (C) Leif Grele 2014"

defAlias = "aliases"
defConfig = "config"


phases :: Opts -> [Phase]
phases opts = phases' $ phases_ opts
  where phases' [] = [Dig,Build,Place,Query]
        phases' ps = filterDuplicates ps


-- remove all duplicate values in a list by converting it to a Set
-- does not preserve order
filterDuplicates :: Ord a => [a] -> [a]
filterDuplicates = S.toList . S.fromList


loadConfigFiles opts = do

  aliases <- B.readFile =<< getDataFileName defAlias

  cfg <- case config opts of
           Nothing -> B.readFile =<< getDataFileName defConfig
           Just c  -> B.readFile c

  cfg <- B.readFile =<< maybe (getDataFileName defConfig) return (config opts)

  return (aliases,cfg)

main = cmdArgs options >>= \opts ->
    do
        images <- mapM B.readFile (V.fromList $ input opts)
        (aliases, configF) <- loadConfigFiles opts

        let dict = decodePhaseMap aliases configF

        case (mapM decodeImage images,dict) of
             (Left e1,Left e2) -> putStrLn e1 >> putStrLn (show e2)
             (Left e1,Right _) -> putStrLn e1
             (Right _,Left e2) -> putStrLn $ show e2
             (Right i,Right d) -> go i d opts


setupEnv :: V.Vector DynamicImage -> Opts -> PixelMap -> Env
setupEnv images opts pmap =
    let img = V.head images
        w = dynamicMap imageWidth img
        h = dynamicMap imageHeight img

        sep = case order opts of
          FromBottom -> stringUtf8 $ "#<" <> replicate w ',' <> "\n"
          FromTop    -> stringUtf8 $ "#>" <> replicate w ',' <> "\n"

    in (Env w h pmap sep)

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


getPosition :: Opts -> V.Vector DynamicImage -> Maybe (Int,Int)
getPosition opts imgs = do
    (x,y,z) <- start opts

    let r = repeat opts
        dir = order opts
        height = dynamicMap imageHeight $ V.head imgs

    if (z > V.length imgs * (1 + repeat opts) || z < 1)
    then return (x,y) --If the z level is invalid just ignore it
    else
      if not (absPos opts) then return (x, y + advance height z)
      else case order opts of
          FromBottom -> return (x, y + (advance height $ invert z r imgs))
          FromTop    -> return (x, y + advance height z)

advance :: Int -> Int -> Int
advance h z = (z - 1) * (h + 1)

-- flip z around
invert :: Int -> Int -> V.Vector a -> Int
invert z r v = (V.length v * (r + 1)) - (z - 1)

writeBlueprints :: Foldable t => Opts -> t Builder -> IO ()
writeBlueprints opts bps = do
    let suffix p = '-':(map toLower $ show p)

        filename = case output opts of
                     "" -> takeBaseName (head $ input opts)
                     n  -> n

        outfile p = filename <> suffix p <.> "csv"

        files = fmap outfile (phases opts)

    mapM_ (\(n,f)-> withFile n WriteMode (flip hPutBuilder f)) $ zip files (toList bps)


