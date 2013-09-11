{-# LANGUAGE OverloadedStrings #-}
module FileTests where
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Data.Vector.Storable as V
import Data.Aeson
import Control.Applicative((<$>),(<*>), empty)
import Control.Monad
import qualified Data.Map as M
import Codec.Picture
import Codec.Picture.Types 
import Config

main = do
    alias <- L.readFile "alias.json"
    config <- L.readFile "pngconfig.json"
    dict <- return $ constructDict alias config
    putStrLn (show dict)
    image <- S.readFile "testcircle.png"
    putStrLn $ show $ vecList $ png $ decodePng image
  where png (Right i) = i
        vecList (ImageRGBA8 i) = V.toList (imageData i)

{-
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
-}