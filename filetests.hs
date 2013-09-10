{-# LANGUAGE OverloadedStrings #-}
module FileTests where
import qualified Data.ByteString.Lazy as L
import Data.Aeson
import Control.Applicative((<$>),(<*>), empty)
import Control.Monad
import qualified Data.Map as M

main = do
    alias <- L.readFile "alias.json"
    putStrLn $ getAlias alias
  where
    getAlias a = either (\str->str) (\_->"success") baz
      where baz = eitherDecode a :: Either String ConfigLists


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