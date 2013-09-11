{-# LANGUAGE StandaloneDeriving, OverloadedStrings #-}
module Config (
    CommandDictionary(..),
    constructDict
    ) where

import qualified Data.Map.Strict as M
import Control.Applicative((<$>),(<*>))
import qualified Data.ByteString.Lazy as L
import Data.Maybe
import Data.Either(either)
import Data.Tuple
import Data.Word(Word8(..))
import Numeric(readHex)
import Control.Monad(mzero)
import Data.Aeson
import Codec.Picture.Types
import Control.Exception
import Text.Regex.Posix((=~))

-- the CommandDictionary describes a mapping from pixels to strings
-- it is accessed via the translate function
data CommandDictionary = CommandDictionary {
                         des :: M.Map PixelRGBA8 String
                       , bld :: M.Map PixelRGBA8 String
                       , plc :: M.Map PixelRGBA8 String
                       , qry :: M.Map PixelRGBA8 String }
    deriving(Eq,Show)


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
toPixel :: (String,String) ->(PixelRGBA8,String)
toPixel (key,val) = (keyToPixel key,val)

-- color representations:
-- base ten: <val>:<val>:<val>:<val>
-- hex: #<val><val><val><val> or 0x<val><val><val><val>
-- keyToPixel :: String -> PixelRGBA8
-- keyToPixel = listToPixel . keyToPixel'
--  where keyToPixel' key | head key == '#' = parseHex $ tail key
--                        | take 2 key == "0x" = parseHex $ drop 2 key
--                        | otherwise = parse10 key

keyToPixel :: String -> PixelRGBA8
keyToPixel = listToPixel . keyToPixel'
  where keyToPixel' key | key == "" = throw (NoMethodError "absent key")
                        | fst hexResults /= "" = parseHex (snd hexResults)
                        | fst base10Results /= "" = parse10 (snd base10Results)
                        | otherwise = throw (NoMethodError ("malformed key: " ++ key))
          where matchHex     = key =~ hexPattern :: (String,String,String,[String])
                matchBaseTen = key =~ baseTenPattern :: (String,String,String,[String])
                hexResults = results matchHex
                base10Results = results matchBaseTen
                results (_,match,_,substrs) = (match,substrs) 



-- parse a list of hex strings to Word8 values
-- we're guaranteed that the list will look like [<hex prefix>,<val>,<val>,<val>]
-- from the pattern match that calls parseHex
parseHex :: [String] -> [Word8]
parseHex = (map toHex) . tail 
  where toHex = fst . head . readHex

parse10 :: [String] -> [Word8]
parse10  = map (readWithBounds)
    -- readWithBounds will either return the String as a Word8 or
    -- throw an error if the value is larger than 255
    where readWithBounds :: String -> Word8
          readWithBounds s | val > 255 = throw (NoMethodError ("key value too large: " ++ s))
                           | otherwise = fromInteger val
            where val = read s :: Integer

--listToPixel :: [Word8] -> PixelRGBA8
listToPixel (r:g:b:a:[]) = PixelRGBA8 r g b a
listToPixel e = throw (PatternMatchFail ("Malformed listToPixel, actually passed " ++ (show e)))

hexPattern :: String
hexPattern = "^(0x|#)([[:xdigit:]]{2,2})([[:xdigit:]]{2,2})([[:xdigit:]]{2,2})([[:xdigit:]]{2,2})"

baseTenPattern :: String
baseTenPattern = "^([0-9]{1,3}):([0-9]{1,3}):([0-9]{1,3}):([0-9]{1,3})"