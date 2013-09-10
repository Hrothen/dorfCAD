{-# LANGUAGE StandaloneDeriving, OverloadedStrings #-}
module Config (
    CommandDictionary(..),
    constructDict
    ) where

import qualified Data.Map.Strict as M
import Control.Applicative((<$>),(<*>))
import qualified Data.ByteString.Lazy as L
import Data.Maybe
import Data.Tuple
import Numeric(readHex)
import Control.Monad(mzero)
import Data.Aeson
import Codec.Picture.Types


-- the CommandDictionary describes a mapping from pixels to strings
-- it is accessed via the translate function
data CommandDictionary = CommandDictionary {
                         des :: M.Map PixelRGBA8 String
                       , bld :: M.Map PixelRGBA8 String
                       , plc :: M.Map PixelRGBA8 String
                       , qry :: M.Map PixelRGBA8 String }


data ConfigLists = ConfigLists { designate :: M.Map String [String]
                               , build     :: M.Map String [String]
                               , place     :: M.Map String [String]
                               , query     :: M.Map String [String] }

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
    buildCommandDict (aliasLists) (commands)
  where 
    buildCommandDict :: ConfigLists -> ConfigLists -> Maybe CommandDictionary
    buildCommandDict al cs =
        buildCommandDict' ( expandList $ M.toList (designate al)
                          , expandList $ M.toList (build al)
                          , expandList $ M.toList (place al)
                          , expandList $ M.toList (query al) )
                          ( expandPixelList $ M.toList (designate cs)
                          , expandPixelList $ M.toList (build cs)
                          , expandPixelList $ M.toList (place cs)
                          , expandPixelList $ M.toList (query cs) )

    buildCommandDict' (a,b,c,d) (w,x,y,z) = do 
       designate' <- genMap a w
       build'     <- genMap b x
       place'     <- genMap c y
       query'     <- genMap d z
       Just (CommandDictionary designate' build' place' query')

    genMap :: [(String,String)] -> [(PixelRGBA8,String)] -> Maybe (M.Map PixelRGBA8 String)
    genMap _ [] = Just M.empty
    genMap [] _ = Just M.empty
    genMap al cs | length (filter pred genList) == 0  = Just (M.fromList $ map noMaybe genList) 
                 | otherwise = Nothing
      where genList = map doLookup cs
            doLookup (pix,str) = (pix,M.lookup str dict)
            dict = M.fromList al

            pred (_,a) = isNothing a

            noMaybe (a,b) = (a,fromJust b)

    expandList :: [(String,[String])] -> [(String,String)]
    expandList = concatMap (expand . swap)

    expandPixelList :: [(String,[String])] -> [(PixelRGBA8,String)]
    expandPixelList = map (toPixel) . expandList
    
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