{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
DeriveDataTypeable, OverloadedStrings, StandaloneDeriving #-}
module ConvertImage(
      ImageString
    , Blueprint
    , Position
    , Phase(..)
    , header
    , phrases
    , convertpngs
    ) where

import Data.Typeable(Typeable)
import Data.Data(Data)
import Codec.Picture.Types
import Data.List(unzip,intercalate,intersperse)
import Data.Maybe(isNothing,fromJust)
import Data.Either(partitionEithers)

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Vector.Storable as V

import Config

type ImageString = String
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
              | otherwise = " start" ++ show (fromJust pos)

        mode  | p == Dig = "dig"
              | p == Build = "build"
              | p == Place = "place"
              | otherwise = "query"


convertpngs :: Int -> Position -> [DynamicImage] -> String -> CommandDictionary -> [Either String Blueprint]
convertpngs r pos imgs phases dict | null err = convertImage
                                   | otherwise = map Left err
  where convertImage = map (\phase -> pngconvert r pos imgs phase dict) p
        (err,images) = partitionEithers convertImage
        p = parsePhases phases

-- convert a list of images into a blueprint
pngconvert :: Int -> Position -> [DynamicImage] -> Phase -> CommandDictionary -> Either String Blueprint
pngconvert r pos imgs phase dict | null errs == False = Left (intercalate "\n" errs)
                               | any (w/=) width || any (h/=) height = Left
                               "Error: not all images have the same dimensions"
                               | otherwise = Right $ toCSV r pos w phase images
  where (errs,images) = partitionEithers csvList
        w = head width
        h = head height
        (width,height) = unzip $ map extractDims imgs
        extractDims i = (dynamicMap imageWidth i,dynamicMap imageHeight i)
        csvList = map (imageToList (translate dict phase)) imgs


-- concat a list of ImageStrings into a single csv Blueprint
toCSV :: Int -> Position -> Int -> Phase -> [ImageString] -> Blueprint
toCSV r s w p imgs = L.pack $ header s w p ++ intercalate uplevel repeatedImgs
  where uplevel = "\n#>" ++ replicate w ','
        repeatedImgs = take (r * (length imgs)) (cycle imgs)

-- convert a RGBA8 image to a list of lists of strings
imageToList :: (PixelRGBA8 -> String) -> DynamicImage -> Either String ImageString
imageToList dict (ImageRGBA8 img) = Right $ convertVector (imageData img)
  where convertVector = csvify (width) . (map ((++ ",") . dict)) . (toPixelList . V.toList)
        width = imageWidth img

        -- convert list of Word8 values into a list of RGBA8 Pixels
        toPixelList [] = []
        toPixelList (a:b:c:d:pixels) = (PixelRGBA8 a b c d) : toPixelList pixels
--catch non RGBA8 images and give an error message
imageToList _ _ = Left "Error: one or more images are not encoded in RGBA8 color, \
                       \did you remember to add an alpha channel?"


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