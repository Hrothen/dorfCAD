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
import Data.List(unzip4)
import Codec.Picture.Png(decodePng)
import Codec.Picture.Types
import qualified Data.ByteString.Lazy as L
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
        configStr <- L.readFile "pngconfig_default.json"
        dict <- return (constructDict aliasStr configStr)
        imgStrs <- mapM B.readFile $ words $ input opts
        putStrLn "placeholder"
        

type ImageString = [[String]]
type LBstr = L.ByteString
type Blueprint = (LBstr,LBstr,LBstr,LBstr)

emptyCell = ""


convertpngs :: [DynamicImage] -> String -> CommandDictionary -> [Either String Blueprint]
convertpngs imgs phases dict = map (csvconvert p) convertImage
  where convertImage = map (\phase -> pngconvert imgs phase dict) p
        p = parsePhases phases

-- convert a list of images into a list of lists of strings, given a dictionary and
-- a phase to convert for
pngconvert :: [DynamicImage] -> Phase -> CommandDictionary -> [Either String ImageString]
pngconvert imgs phase dict = map (imageToList (translate dict phase)) imgs

-- convert a list of ImageStrings into four Bytestring encoded CSVs
csvconvert :: [Phase] -> [Either String ImageString] -> Either String Blueprint
csvconvert phases = undefined


-- convert a RGBA8 image to a list of lists of strings
imageToList :: (PixelRGBA8 -> Maybe String) -> DynamicImage -> Either String ImageString
imageToList dict (ImageRGBA8 img) = Right $ convertVector (imageData img)
  where convertVector = splitList (width) . (map (replaceNothings . dict)) . (toPixelList . V.toList)
        width = imageWidth img
        replaceNothings = maybe emptyCell id

        -- convert list of Word8 values into a list of RGBA8 Pixels
        toPixelList [] = []
        toPixelList (a:b:c:d:pixels) = (PixelRGBA8 a b c d) : toPixelList pixels
--catch non RGBA8 images and give an error message
imageToList _ _ = Left "Unsupported png format, use RGBA8 encoding"


-- split a list into lists of length i
splitList :: Int -> [a] -> [[a]]
splitList _ []  = []
splitList i ls  = row : splitList i rest
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


data CommandDictionary = CommandDictionary {
                         des :: M.Map PixelRGBA8 String
                       , bld :: M.Map PixelRGBA8 String
                       , plc :: M.Map PixelRGBA8 String
                       , qry :: M.Map PixelRGBA8 String }

translate :: CommandDictionary -> Phase -> PixelRGBA8 -> Maybe String
translate dict Dig key   = M.lookup key (des dict)
translate dict Build key = M.lookup key (bld dict)
translate dict Place key = M.lookup key (plc dict)
translate dict Query key = M.lookup key (qry dict)
translate _ _ _          = Nothing --might not be the best fallback


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

    --buildCommandDict' :: (a,a,a,a) -> (b,b,b,b) -> Maybe CommandDictionary
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

            pred (_,Nothing) = False
            pred (_,Just a)  = True

            noMaybe (a,b) = (a,fromJust b)

    expandList :: [(String,[String])] -> [(String,String)]
    expandList = concatMap (expand . swap)

    expandPixelList :: [(String,[String])] -> [(PixelRGBA8,String)]
    expandPixelList = map (toPixel) . expandList
        
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
