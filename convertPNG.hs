{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
DeriveDataTypeable, OverloadedStrings, StandaloneDeriving #-}

module Main(main) where

import Data.Either(either)
import Data.Maybe(isNothing,fromJust)
import Codec.Picture.Png(decodePng)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString as B
import System.Environment(getArgs)
import System.Console.CmdLib
import Control.Monad

import Config(CommandDictionary(..),constructDict)
import ConvertImage(Blueprint,Phase(..),convertpngs,phrases)


data Main = Main { start :: [Int], input :: String,
                   output :: String, phases :: String }
    deriving (Typeable, Data, Eq)

instance Attributes Main where
    attributes _ = group "Options" [
        start          %> [ Help "Start position of the macro.",
                            ArgHelp "(X,Y)",
                            Short ['s'],
                            Long ["start"] ],
        input          %> [ Help "Images to be converted to blueprints.",
                            ArgHelp "FILENAME,FILENAME, ...",
                            Required True,
                            Short ['i'] ],
        output         %> [ Help "Name to use for blueprints, if not specified uses input name",
                            ArgHelp "TEXT",
                            Short ['o'],
                            Long ["output"] ],
        phases         %> [ Help "Phase to create a blueprint for.",
                            ArgHelp "[All|Dig|Build|Place|Query] OR <phase>,<phase>,...",
                            Long ["phase"],
                            Short ['p'] ]
        ]

instance RecordCommand Main where
    mode_summary _ = "Simple program to convert RGBA encoded .png images into quickfort blueprints"

main = getArgs >>= executeR Main {} >>= \opts ->
    do
        aliasStr          <- L.readFile "alias.json"
        configStr         <- L.readFile "pngconfig.json"
        imgFileStrs       <- mapM B.readFile $ phrases (input opts)
        outStr            <- return $ genOutfileName (input opts) (output opts)
        blueprints        <- return $ go aliasStr configStr imgFileStrs opts
        either (putStrLn) (mapM_ (writeFile' outStr)) blueprints
  where 
        genOutfileName i "" = head (words i) ++ "-"
        genOutfileName _ s  = s ++ "-"
        
        writeFile' :: String -> Blueprint -> IO ()
        writeFile' outStr img = let name = outStr ++
                                           (L.unpack $ L.takeWhile (','/=) (L.tail img)) ++
                                           ".csv"
                                in do L.writeFile name img

go :: L.ByteString -> L.ByteString -> [B.ByteString] -> Main -> Either String [Blueprint]
go alias config imgFiles opts = do
    dict     <- constructDict alias config
    imgStrs  <- mapM decodePng imgFiles
    sequence $ convertpngs startPos imgStrs phaseList dict
  where
    startPos  = toTup (start opts)
    phaseList = phases opts
    toTup ls  | null ls = Nothing
              | length ls /= 2 = Nothing
              | otherwise = Just (head ls,last ls)

