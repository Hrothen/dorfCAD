{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main(main) where

import Prelude hiding (repeat)

import Data.List(intercalate)
import Data.Char(toLower)
import Data.Monoid((<>))
import Data.Foldable(toList)
import Data.Traversable(for)
import Data.Maybe(fromJust)
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


data Opts = Opts { start  :: Maybe (Int,Int),  input  :: [String],
                   output :: String, phases_ :: [Phase],
                   repeat :: Int,    config :: String }
    deriving (Typeable, Data, Eq, Show)

options = Opts{ start  = def
                       &= typ "X,Y"
                       &= help "Start position of the macro."
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
                       -- &= name "phase" -- &= name "p"
                       &= help "Phase to create a blueprint for"
              , repeat = 1
                       &= typ "INTEGER"
                       &= help "Optional, specifies a number of times to repeat the blueprint"
              , config = "config.json"
                       &= typFile
                       &= help "Specify a config file to use instead of the default"
              }
              &= program "mkblueprint"
              &= summary "dorfCAD v1.2, (C) Leif Grele 2014"

phases :: Opts -> [Phase]
phases opts = phases' $ phases_ opts
  where phases' [] = [Dig,Build,Place,Query]
        phases' ps = filterDuplicates ps

filterDuplicates :: Ord a => [a] -> [a]
filterDuplicates = S.toList . S.fromList


main = cmdArgs options >>= \opts ->
    do
        images <- mapM B.readFile (V.fromList $ input opts)

        dict <- decodePhaseMap <$> (B.readFile "alias.json") <*> B.readFile (config opts)

        case (mapM decodeImage images,dict) of
             (Left e1,Left e2) -> putStrLn e1 >> putStrLn (show e2)
             (Left e1,Right _) -> putStrLn e1
             (Right _,Left e2) -> putStrLn $ show e2
             (Right i,Right d) -> go' i d opts


setupEnv :: V.Vector DynamicImage -> Opts -> Env
setupEnv images opts =
    let img = V.head images
        w = dynamicMap imageWidth img
        h = dynamicMap imageHeight img

        sep = stringUtf8 $ "#>" <> replicate w ',' <> "\n"

    in (Env w h M.empty sep)

go' :: V.Vector DynamicImage -> PhaseMap -> Opts -> IO ()
go' images dict opts = do
    let env  = setupEnv images opts
        reps = repeat opts
        pos  = start opts


        csvs = for (phases opts) $ \p->
            let b = buildCsv reps pos p images
                e = env{px = fromJust $ M.lookup p dict}
            in runEnvR b e

    case csvs of
        Left err -> print err
        Right cs -> writeBlueprints opts cs

writeBlueprints :: Foldable t => Opts -> t Builder -> IO ()
writeBlueprints opts bps = do
    let suffix p = '-':(map toLower $ show p)

        filename = case output opts of
                     "" -> takeBaseName (head $ input opts)
                     n  -> n

        outfile p = filename <> suffix p <.> "csv"

        files = fmap outfile (phases opts)

    mapM_ (\(n,f)-> withFile n WriteMode (flip hPutBuilder f)) $ zip files (toList bps)


