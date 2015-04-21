{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
DeriveDataTypeable, OverloadedStrings, StandaloneDeriving #-}

module Main(main) where

import Prelude hiding (repeat)

import Data.Either(either)
import Data.List(isSuffixOf)
import Data.Maybe(isNothing,fromJust)
import Codec.Picture(decodeImage)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString as B
import System.Console.CmdArgs
import Control.Monad

import Config(CommandDictionary(..),constructDict)
import ConvertImage(Blueprint,Phase(..),convertpngs,phrases)


data Opts = Opts { start  :: [Int],  input  :: [String],
                   output :: String, phases :: String,
                   repeat :: Int,    config :: String }
    deriving (Typeable, Data, Eq)

options = Opts{ start  = def
                       &= help "Start position of the macro."
                       &= typ "X,Y"
              , input  = def
                       &= args
              , output = def
                       &= help "Name to use for blueprints, if not specified uses input name"
                       &= typFile
              , phases = def
                       &= help "Phase to create a blueprint for"
                       &= typ "[All|Dig|Build|Place|Query] OR <phase>,<phase>,..."
                       &= name "phase"
              , repeat = 1
                       &= help "Optional, specifies a number of times to repeat the blueprint"
                       &= typ "INTEGER"
              , config = "config.json"
                       &= help "Specify a config file to use instead of the default"
                       &= typFile
              }

-- instance Attributes Main where
--     attributes _ = group "Options" [
--         start          %> [ Help "Start position of the macro.",
--                             ArgHelp "X,Y",
--                             Short ['s'],
--                             Long ["start"] ],
--         input          %> [ Help "Images to be converted to blueprints.",
--                             ArgHelp "FILENAME,FILENAME, ...",
--                             Extra True ],
--         output         %> [ Help "Name to use for blueprints, if not specified uses input name",
--                             ArgHelp "TEXT",
--                             Short ['o'],
--                             Long ["output"] ],
--         phases         %> [ Help "Phase to create a blueprint for.",
--                             ArgHelp "[All|Dig|Build|Place|Query] OR <phase>,<phase>,...",
--                             Long ["phase"],
--                             Short ['p'] ],
--         repeat         %> [ Help "Optional, specifies a number of times to repeat the blueprint",
--                             ArgHelp "Int",
--                             Short ['r'],
--                             Long ["repeat"],
--                             Default (1::Int) ],
--         config         %> [ Help "Optional, specify a config file to use. Defaults to config.json",
--                             ArgHelp "PATH",
--                             Long ["config"],
--                             Default ("config.json"::String) ]
--         ]

-- instance RecordCommand Main where
--     mode_summary _ = "Simple program to convert RGBA encoded .png images into quickfort blueprints"

main = cmdArgs options >>= \opts ->
    do
        aliasStr          <- L.readFile "alias.json"
        configStr         <- L.readFile (config opts)
        -- don't bother to check if input files are valid, we want to exit if readFile fails
        -- and we don't have any cleanup to do.
        imgFileStrs       <- mapM B.readFile (input opts)
        outStr            <- return $ genOutfileName ( head $ input opts) (output opts)
        blueprints        <- return $ go aliasStr configStr imgFileStrs opts
        either (putStrLn) (mapM_ (writeFile' outStr)) blueprints
  where 
        genOutfileName i "" = (stripSuffix (head (words i))) ++ "-"
        genOutfileName _ s  = s ++ "-"

        stripSuffix ls | any (`isSuffixOf` ls) [".png",".bmp",".gif"] = take ((length ls) - 4) ls
                       | ".tiff" `isSuffixOf` ls = take ((length ls) - 5) ls 
                       | otherwise = ls
        
        writeFile' :: String -> Blueprint -> IO ()
        writeFile' outStr img = let name = outStr ++
                                           (L.unpack $ L.takeWhile (notDelimiter) (L.tail img)) ++
                                           ".csv"
                                in do L.writeFile name img
        notDelimiter c = (c /= ',') && (c /= ' ')

go :: L.ByteString -> L.ByteString -> [B.ByteString] -> Opts -> Either String [Blueprint]
go alias config imgFiles opts = do
    dict     <- constructDict alias config
    imgStrs  <- mapM decodeImage imgFiles
    sequence $ convertpngs reps startPos imgStrs phaseList dict
  where
    startPos  = toTup (start opts)
    phaseList = phases opts
    reps      = repeat opts
    toTup ls  | null ls = Nothing
              | length ls /= 2 = Nothing
              | otherwise = Just (head ls,last ls)

