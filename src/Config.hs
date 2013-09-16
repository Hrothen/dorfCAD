{-# LANGUAGE StandaloneDeriving, OverloadedStrings #-}
module Config (
    CommandDictionary(..),
    constructDict
    ) where

import qualified Data.Map.Strict as M
import Control.Applicative((<$>),(<*>))
import qualified Data.ByteString.Lazy as L
import Data.List(unlines)
import Data.Maybe
import Data.Either(partitionEithers)
import Data.Tuple
import Data.Word(Word8(..))
import Numeric(readHex)
import Control.Monad(mzero,liftM,mapM)
import Data.Aeson
import Codec.Picture.Types
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

configTup :: ConfigLists -> (M.Map String [String],M.Map String [String],
                             M.Map String [String],M.Map String [String])
configTup cl = (designate cl,
                build cl,
                place cl,
                query cl)

instance FromJSON ConfigLists where
    parseJSON (Object v) = ConfigLists <$>
                          v .: "designate" <*>
                          v .: "build" <*>
                          v .: "place" <*>
                          v .: "query"
    parseJSON _          = mzero


deriving instance Ord PixelRGBA8


constructDict :: L.ByteString -> L.ByteString -> Either String CommandDictionary
constructDict alias config = do
    aliasLists <- eitherDecode alias :: Either String ConfigLists
    commands   <- eitherDecode config :: Either String ConfigLists
    buildCommandDict aliasLists commands
  where 
    buildCommandDict :: ConfigLists -> ConfigLists -> Either String CommandDictionary
    buildCommandDict al cs =
        buildCommandDict' (tmap4 (expandList . M.toList) altup)
                          (tmap4 (expandPixelList . M.toList) cstup)
            where altup = configTup al
                  cstup = configTup cs
    buildCommandDict' (a,b,c,d) (w,x,y,z) = do 
       designate' <- genMap a w
       build'     <- genMap b x
       place'     <- genMap c y
       query'     <- genMap d z
       Right (CommandDictionary designate' build' place' query')


-- map a function over a 4 tuple
tmap4 :: (a -> b) -> (a,a,a,a) -> (b,b,b,b)
tmap4 f (t1,t2,t3,t4) = (f t1,f t2,f t3,f t4)


genMap :: [(String,String)] -> [(Either String PixelRGBA8,String)]
                            -> Either String (M.Map PixelRGBA8 String)
genMap _ []  = Right M.empty
genMap [] _  = Right M.empty
genMap al cs | not (null errorList) = Left (unlines errorList)
             | length (filter pred genList) == 0  = Right (M.fromList $ map noMaybe genList) 
             | otherwise =
                Left "Error generating pixel-string map: an alias is referenced\
                      \ in pngconfig.json that is not present in alias.json"
  where 
    (errorList,genList) = partitionEithers $ map (checkEither . doLookup) cs
    doLookup (pix,str) = (pix,M.lookup str dict)
    -- checkEither extracts the Either state from a tuple
    checkEither (Right p,s) = Right (p,s)
    checkEither (Left e,s) = Left e
    dict = M.fromList al

    pred (_,a) = isNothing a

    noMaybe (a,b) = (a,fromJust b)


expandList :: [(String,[String])] -> [(String,String)]
expandList = concatMap (expand . swap)

expandPixelList :: [(String,[String])] -> [(Either String PixelRGBA8,String)]
expandPixelList = map (toPixel) . expandList
    
-- expand a tuple holding a list of keys and a value into a list of key value pairs    
expand :: ([String],String) -> [(String,String)]
expand ([],_) = []
expand ((k:ks),val) = (k,val) : expand (ks,val)

-- TODO: need to make these check for malformed strings
toPixel :: (String,String) ->(Either String PixelRGBA8,String)
toPixel (key,val) = (keyToPixel key,val)

-- color representations:
-- base ten: <val>:<val>:<val>:<val>
-- hex: #<val><val><val><val> or 0x<val><val><val><val>
keyToPixel :: String -> Either String PixelRGBA8
keyToPixel = (liftM listToPixel) . keyToPixel'
  where keyToPixel' key | key               == "" = Left (fErr "attempted to pass null key string")
                        | fst hexResults    /= "" = Right $ parseHex (snd hexResults)
                        | fst base10Results /= "" = parse10 (snd base10Results)
                        | otherwise               = Left (fErr $ "malformed key: " ++ key)
          where matchHex      = key =~ hexPattern :: (String,String,String,[String])
                matchBaseTen  = key =~ baseTenPattern :: (String,String,String,[String])
                hexResults    = results matchHex
                base10Results = results matchBaseTen
                results (_,match,_,substrs) = (match,substrs)
                fErr s = "Error in keyToPixel: " ++ s



-- parse a list of hex strings to Word8 values
-- we're guaranteed that the list will look like [<hex prefix>,<val>,<val>,<val>]
-- from the pattern match that calls parseHex
parseHex :: [String] -> [Word8]
parseHex = (map toHex) . tail 
  where toHex = fst . head . readHex

parse10 :: [String] -> Either String [Word8]
parse10  = mapM (readWithBounds)
    -- readWithBounds will either return the String as a Word8 or
    -- throw an error if the value is larger than 255
    where readWithBounds :: String -> Either String Word8
          readWithBounds s | val > 255 = Left ("key value too large: " ++ s)
                           | otherwise = Right (fromInteger val)
            where val = read s :: Integer

-- listToPixel can't fail on an input of Right, the pattern matching in keyToPixel'
-- guarentees that parseHex and parse10 will return a list of the correct size
listToPixel :: [Word8] -> PixelRGBA8
listToPixel (r:g:b:a:[]) = PixelRGBA8 r g b a

hexPattern :: String
hexPattern = "^(0x|#)([[:xdigit:]]{2,2})([[:xdigit:]]{2,2})([[:xdigit:]]{2,2})([[:xdigit:]]{2,2})"

baseTenPattern :: String
baseTenPattern = "^([0-9]{1,3}):([0-9]{1,3}):([0-9]{1,3}):([0-9]{1,3})"