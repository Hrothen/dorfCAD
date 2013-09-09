{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
DeriveDataTypeable, OverloadedStrings, StandaloneDeriving #-}

import qualified Data.Map.Strict as M
import Control.Applicative((<$>),(<*>), empty)
import Numeric
import Data.Maybe
import qualified Data.Vector.Storable as V
import Data.Either
import Data.Maybe
import Data.Tuple
import Data.Aeson
import Data.List(unzip,intercalate,intersperse)
import Codec.Picture.Png(decodePng)
import Codec.Picture.Types
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString as B
import System.Environment(getArgs)
import System.Console.CmdLib
import Control.Monad


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
        dict <- return (constructDict aliasStr configStr)
        imgFileStrs <- mapM B.readFile $ words $ input opts
        imgStrs <- return $ rights $ map decodePng imgFileStrs
        (errs,imgs) <- return $ partitionEithers
            $ convertpngs startPos imgStrs (phases opts) (fromJust dict)
        if (not . null) errs
        then putStrLn $ concat errs
        else mapM_ (writeFile' outStr) imgs
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


-- the CommandDictionary describes a mapping from pixels to strings
-- it is accessed via the translate function
data CommandDictionary = CommandDictionary {
                         des :: M.Map PixelRGBA8 String
                       , bld :: M.Map PixelRGBA8 String
                       , plc :: M.Map PixelRGBA8 String
                       , qry :: M.Map PixelRGBA8 String }

translate :: CommandDictionary -> Phase -> PixelRGBA8 -> Maybe String
translate dict Dig   key = M.lookup key (des dict)
translate dict Build key = M.lookup key (bld dict)
translate dict Place key = M.lookup key (plc dict)
translate dict Query key = M.lookup key (qry dict)


data ConfigLists = ConfigLists { designate :: [(String,[String])]
                               , build     :: [(String,[String])]
                               , place     :: [(String,[String])]
                               , query     :: [(String,[String])] }

instance FromJSON ConfigLists where
    parseJSON (Object v) = ConfigLists <$>
                          v .: "designate" <*>
                          v .: "build" <*>
                          v .: "place" <*>
                          v .: "query"
    parseJSON _          = mzero


deriving instance Ord PixelRGBA8


constructDict :: L.ByteString -> L.ByteString -> Maybe CommandDictionary
constructDict alias config = do
    aliasLists <- decode alias :: Maybe ConfigLists
    commands <- decode config :: Maybe ConfigLists
    buildCommandDict (Just aliasLists) (Just commands)
  where 
    buildCommandDict :: Maybe ConfigLists -> Maybe ConfigLists -> Maybe CommandDictionary
    buildCommandDict Nothing _ = Nothing
    buildCommandDict _ Nothing = Nothing
    buildCommandDict (Just al) (Just cs) =
        buildCommandDict' ( expandList (designate al)
                          , expandList (build al)
                          , expandList (place al)
                          , expandList (query al) )
                          ( expandPixelList (designate cs)
                          , expandPixelList (build cs)
                          , expandPixelList (place cs)
                          , expandPixelList (query cs) )

    buildCommandDict' (a,b,c,d) (w,x,y,z) = do 
       designate' <- genMap a w
       build'     <- genMap b x
       place'     <- genMap c y
       query'     <- genMap d z
       Just (CommandDictionary designate' build' place' query')

    genMap :: [(String,String)] -> [(PixelRGBA8,String)] -> Maybe (M.Map PixelRGBA8 String)
    genMap al cs | length (takeWhile pred genList) == 0  = Just (M.fromList $ map noMaybe genList) 
                 | otherwise = Nothing
      where genList = map doLookup cs
            doLookup (pix,str) = (pix,M.lookup str dict)
            dict = M.fromList al

            pred (_,a) = isJust a

            noMaybe (a,b) = (a,fromJust b)

    expandList :: [(String,[String])] -> [(String,String)]
    expandList = concatMap (expand . swap)

    expandPixelList :: [(String,[String])] -> [(PixelRGBA8,String)]
    expandPixelList = concatMap (toPixel . expand . swap)
    
    -- expand a tuple holding a list of keys and a value into a list of key value pairs    
    expand :: ([String],String) -> [(String,String)]
    expand ([],_) = []
    expand ((k:ks),val) = (k,val) : expand (ks,val)

    -- TODO: need to make these check for malformed strings
    toPixel :: (String,String) -> (PixelRGBA8,String)
    toPixel (key,val) = (keyToPixel key,val)

    -- color representations:
    -- base ten: <val>:<val>:<val>:<val>
    -- hex: #<val><val><val><val> or 0x<val><val><val><val>
    keyToPixel = listToPixel . keyToPixel'

    keyToPixel' key | head key == '#' = parseHex $ tail key
                    | take 2 key == "0x" = parseHex $ drop 2 key
                    | otherwise = parse10 key
      where parseHex [] = []
            parseHex (a:b:ks) = (fst $ head (readHex (a:[b]))) : parseHex ks

            parse10 [] = []
            parse10 ks = (read a) : parse10 b
              where (a,b) = break (== ':') ks

    listToPixel (r:g:b:a:[]) = PixelRGBA8 r g b a
