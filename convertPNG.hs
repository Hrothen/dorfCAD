{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
DeriveDataTypeable, OverloadedStrings, StandaloneDeriving #-}

module Main where

import qualified Data.Map.Strict as M
import qualified Data.Vector.Storable as V
import Data.Either(partitionEithers,rights)
import Data.Maybe(isNothing,fromJust)
import Data.List(unzip,intercalate,intersperse)
import Codec.Picture.Png(decodePng)
import Codec.Picture.Types
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString as B
import System.Environment(getArgs)
import System.Console.CmdLib
import Control.Monad

import Config(CommandDictionary(..),constructDict)


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
                            ArgHelp "FILENAME FILENAME ...",
                            Required True,
                            Short ['i'] ],
        output         %> [ Help "Name to use for blueprints, if not specified uses input name",
                            ArgHelp "TEXT",
                            Short ['o'],
                            Long ["output"] ],
        phases         %> [ Help "Phase to create a blueprint for.",
                            ArgHelp "[All|Dig|Build|Place|Query]",
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
        putStrLn "files loaded"
        dict <- return (constructDict aliasStr configStr)
        putStrLn "dictionary built"
        putStrLn $ show $ isNothing dict
        imgFileStrs <- mapM B.readFile $ words $ input opts
        imgStrs <- return $ rights $ map decodePng imgFileStrs
        putStrLn $ show (length imgStrs)  
        (errs,imgs) <- return $ partitionEithers
            $ convertpngs startPos imgStrs (phases opts) (fromJust dict)
        mapM_ putStrLn errs
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
        

type ImageString = String
type LBstr = L.ByteString
type Blueprint = L.ByteString
type Position = Maybe (Int,Int)

emptyCell = ""

-- This header goes at the top of each Blueprint and tells
-- quickfort where to start and in what mode to run
header :: Position -> Int -> Phase -> String
header pos w p = '#':mode ++ start ++ empties
  where empties = replicate (w-1) ','
        start | pos == Nothing = ""
              | otherwise = "start" ++ show (fromJust pos)
        mode  | p == Dig = "dig"
              | p == Build = "build"
              | p == Place = "place"
              | otherwise = "query"


convertpngs :: Position -> [DynamicImage] -> String -> CommandDictionary -> [Either String Blueprint]
convertpngs pos imgs phases dict | null err = convertImage
                                 | otherwise = map Left (intersperse "\n" err)
  where convertImage = map (\phase -> pngconvert pos imgs phase dict) p
        (err,images) = partitionEithers convertImage
        p = parsePhases phases

-- convert a list of images into a blueprint
pngconvert :: Position -> [DynamicImage] -> Phase -> CommandDictionary -> Either String Blueprint
pngconvert pos imgs phase dict | null errs = Left (intercalate "\n" errs)
                               | any (w/=) width || any (h/=) height = Left
                                "Error: not all images have the same dimensions"
                               | otherwise = Right $ toCSV pos w phase images
  where (errs,images) = partitionEithers csvList
        w = head width
        h = head height
        (width,height) = unzip $ map extractDims imgs
        extractDims i = (dynamicMap imageWidth i,dynamicMap imageHeight i)
        csvList = map (imageToList (translate dict phase)) imgs


-- concat a list of ImageStrings into a single csv Blueprint
toCSV :: Position -> Int -> Phase -> [ImageString] -> Blueprint
toCSV s w p imgs = L.pack $ header s w p ++ intercalate uplevel imgs
  where uplevel = "#>" ++ replicate (w-1) ','

-- convert a RGBA8 image to a list of lists of strings
imageToList :: (PixelRGBA8 -> Maybe String) -> DynamicImage -> Either String ImageString
imageToList dict (ImageRGBA8 img) = Right $ convertVector (imageData img)
  where convertVector = csvify (width) . (map ((',':) . replaceNothings . dict)) . (toPixelList . V.toList)
        width = imageWidth img
        replaceNothings = maybe emptyCell id -- replace a result of Nothing with an empty cell

        -- convert list of Word8 values into a list of RGBA8 Pixels
        toPixelList [] = []
        toPixelList (a:b:c:d:pixels) = (PixelRGBA8 a b c d) : toPixelList pixels
--catch non RGBA8 images and give an error message
imageToList _ _ = Left "Unsupported png format, use RGBA8 encoding"


-- take a list of comma delimited strings and return a string with newlines added
csvify :: (Int) -> [String] -> String
csvify _ [] = ""
-- we add a header to the csv later, and the last line of the file doesn't
-- need a newline, so we can prepend it for a small savings
csvify i ls = '\n' : (concat row) ++ csvify i rest
  where (row,rest) = splitAt i ls


parsePhases :: String -> [Phase]
parsePhases ""    = []
parsePhases "All" = [Dig,Build,Place,Query]
parsePhases s     = map read (words s)

data Phase = Dig
           | Build
           | Place
           | Query
    deriving (Typeable, Data, Eq, Read, Show)


translate :: CommandDictionary -> Phase -> PixelRGBA8 -> Maybe String
translate dict Dig   key = M.lookup key (des dict)
translate dict Build key = M.lookup key (bld dict)
translate dict Place key = M.lookup key (plc dict)
translate dict Query key = M.lookup key (qry dict)
