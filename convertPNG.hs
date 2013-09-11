{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
DeriveDataTypeable, OverloadedStrings, StandaloneDeriving #-}

module Main(main) where

import Data.Either(partitionEithers,rights)
import Data.Maybe(isNothing,fromJust)
import Codec.Picture.Png(decodePng)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString as B
import System.Environment(getArgs)
import System.Console.CmdLib
import Control.Monad
import Control.Exception

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
        aliasStr <- L.readFile "alias.json"
        configStr <- L.readFile "pngconfig.json"
        startPos <- return (toTup $ start opts)
        outStr <- return $ genOutfileName (input opts) (output opts)
        dict <- return (constructDict aliasStr configStr)
        imgFileStrs <- mapM B.readFile $ phrases $ input opts
        (errstrs,imgStrs) <- return $ partitionEithers $ map decodePng imgFileStrs
        (errs,imgs) <- catch (return $ partitionEithers
            $ convertpngs startPos imgStrs (phases opts) (fromJust dict))
            (\e-> do let errstr = show (e :: SomeException)
                     putStrLn ("error parsing file:" ++ errstr)
                     return ([],[]))
        if null errs
        then mapM_ (writeFile' outStr) imgs
        else putStrLn $ concat errs
  where genOutfileName i "" = head (words i) ++ "-"
        genOutfileName _ s  = s ++ "-"
        toTup ls | null ls = Nothing
                 | length ls /= 2 = Nothing
                 | otherwise = Just (head ls,last ls)
        writeFile' :: String -> Blueprint -> IO ()
        writeFile' outStr img = let name = outStr ++
                                           (L.unpack $ L.takeWhile (','/=) (L.tail img)) ++
                                           ".csv"
                                in do L.writeFile name img
