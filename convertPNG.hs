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
import Control.Exception

import Config(CommandDictionary(..),constructDict)

data ImageFormatException = ImageNotRGBA8
    deriving (Show, Typeable)

instance Exception ImageFormatException


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
        putStrLn $ show (phases opts)
        putStrLn (input opts)
        imgFileStrs <- mapM B.readFile $ phrases $ input opts
        (errstrs,imgStrs) <- return $ partitionEithers $ map decodePng imgFileStrs
        putStrLn $ show (length imgStrs)
        putStrLn $ concat errstrs
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
        

type ImageString = String
type LBstr = L.ByteString
type Blueprint = L.ByteString
type Position = Maybe (Int,Int)

-- string to put in empty cells, quickfort accepts an empty string or "#" here
emptyCell = "#"

-- This header goes at the top of each Blueprint and tells
-- quickfort where to start and in what mode to run
header :: Position -> Int -> Phase -> String
header pos w p = '#':mode ++ start ++ empties
  where empties = replicate (w) ','
        start | pos == Nothing = ""
              | otherwise = "start" ++ show (fromJust pos)

        mode  | p == Dig = "dig"
              | p == Build = "build"
              | p == Place = "place"
              | otherwise = "query"


convertpngs :: Position -> [DynamicImage] -> String -> CommandDictionary -> [Either String Blueprint]
convertpngs pos imgs phases dict | null err = convertImage
                                 | otherwise = map Left err
  where convertImage = map (\phase -> pngconvert pos imgs phase dict) p
        (err,images) = partitionEithers convertImage
        p = parsePhases phases

-- convert a list of images into a blueprint
pngconvert :: Position -> [DynamicImage] -> Phase -> CommandDictionary -> Either String Blueprint
pngconvert pos imgs phase dict | null errs == False = Left (intercalate "\n" errs)
                               -- | any (w/=) width || any (h/=) height = Left
                               -- "Error: not all images have the same dimensions"
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
imageToList :: (PixelRGBA8 -> String) -> DynamicImage -> Either String ImageString
imageToList dict (ImageRGBA8 img) = Right $ convertVector (imageData img)
  where convertVector = csvify (width) . (map ((++ ",") . dict)) . (toPixelList . V.toList)
        width = imageWidth img

        -- convert list of Word8 values into a list of RGBA8 Pixels
        toPixelList [] = []
        toPixelList (a:b:c:d:pixels) = (PixelRGBA8 a b c d) : toPixelList pixels
--catch non RGBA8 images and give an error message
imageToList _ _ = throw ImageNotRGBA8


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
parsePhases s     = map read (phrases s)

-- same as words, but cuts on commas instead of spaces
phrases :: String -> [String]
phrases s = case dropWhile {-partain:Char.-}isComma s of
                 "" -> []
                 s' -> w : phrases s''
                       where (w,s'') =
                              break {-partain:Char.-} isComma s'

isComma :: Char -> Bool
isComma ',' = True
isComma _   = False

data Phase = Dig
           | Build
           | Place
           | Query
    deriving (Typeable, Data, Eq, Read, Show)


translate :: CommandDictionary -> Phase -> PixelRGBA8 -> String
translate dict Dig   key = M.findWithDefault emptyCell key (des dict)
translate dict Build key = M.findWithDefault emptyCell key (bld dict)
translate dict Place key = M.findWithDefault emptyCell key (plc dict)
translate dict Query key = M.findWithDefault emptyCell key (qry dict)
