{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
DeriveDataTypeable, OverloadedStrings, StandaloneDeriving #-}

import qualified Data.Map.Strict as M
import Control.Applicative((<$>),(<*>), empty)
import Numeric
import Data.Maybe
import Data.Either
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

data Phase = All
           | Dig
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


deriving instance Ord PixelRGBA8

main = getArgs >>= executeR Main {} >>= \opts ->
    do
        aliasStr <- L.readFile "alias.json"
        configStr <- L.readFile "pngconfig_default.json"
        dict <- return (constructDict aliasStr configStr)
        imgStrs <- mapM B.readFile $ words $ input opts
        (strs,imgs) <- return ( partitionEithers $ map decodePng imgStrs )
        putStrLn $ show strs


parseDig :: [Int] -> (Int -> String) -> Maybe String
parseDig commands mapping = undefined

parseBuild :: [Int] -> (Int -> String) -> Maybe String
parseBuild commands mapping = undefined

parsePlace :: [Int] -> (Int -> String) -> Maybe String
parsePlace commands mapping = undefined

parseQuery :: [Int] -> (Int -> String) -> Maybe String
parseQuery commands mapping = undefined

--unzipImage4 :: L.ByteString -> Maybe ([Int],[Int],[Int],[Int])
--unzipImage4 str = convert $ decodePng str
--    where convert (Left s) = Nothing
--          convert (Right img) = Just (unzip4 $ imageToList img)



imageToList :: DynamicImage -> (PixelRGBA8 -> String) -> Either String [String]
imageToList (ImageRGBA8 img) dict = Right $ reverse $ pixelFold pixPred [] img
  where pixPred acc _ _ pix = (dict pix) : acc
imageToList _ _ = Left "Unsupported png format, use RGBA8 encoding"


--pixPred :: (ColorConvertible a  PixelRGBA16) => [LongPixel] -> x -> x -> a -> [LongPixel]
---pixPred acc _ _ pix = (promoteToTuple pix):acc

--promoteToTuple :: (ColorConvertible pixel PixelRGBA16) => pixel -> LongPixel
--promoteToTuple p = pixelToTuple p' 
--    where p' = (promotePixel p)::PixelRGBA16


--pixelToTuple :: PixelRGBA16 -> LongPixel
--pixelToTuple (PixelRGBA16 r g b a) = (fromIntegral r, fromIntegral g, fromIntegral b,
--                        fromIntegral a)