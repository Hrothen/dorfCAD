{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Types where

import Data.Data
import Data.Char(isSpace)
import Numeric(readDec, readHex)

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Data.Text(Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.ByteString
import Text.Parsec.Indent(runIndent, withBlock)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Codec.Picture.Types

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Builder

import Control.Monad.Except
import Control.Monad.Reader


data Phase = Dig
           | Build
           | Place
           | Query
    deriving (Typeable, Data, Eq, Ord, Read, Show)

type PixelMap = Map PixelRGBA8 Builder

type PhaseMap = Map Phase PixelMap


decodePhaseMap :: B.ByteString -> B.ByteString -> Either ParseError PhaseMap
decodePhaseMap a s = do
     aliases <- runIndent "" $ runParserT (maps str) () "" a
     m <- runIndent "" $ runParserT (maps color) () "" s
     return $ M.map (M.map byteString) (M.intersectionWith mergeMaps aliases m)


maps p = M.fromList <$> (spaces >> manyTill (parseMap p) eof)


parseMap p = withBlock (\k m-> (k,M.fromList $ mconcat m)) phase (line p)


line p = do
    l <- lexeme (manyTill anyChar endOfLine)
    let m = parse line' "" l
    either (unexpected . show) return m
  where line' = do
          v    <- value
          keys <- commaSep p
          return $ map (,v) keys

-- merge maps A and B such that for (k,b) in B, if b is a key in A
-- then the pair (k,A `lookup` b) is a key,value pair in C
-- and if b is not a key in A, then the pair (k,b) is in C
mergeMaps :: (Ord a) => Map a a -> Map b a -> Map b a
mergeMaps a = M.map (\k -> M.findWithDefault k k a)


phase = lexeme  (choice [ string "designate" >> return Dig
                        , string "build"     >> return Build
                        , string "place"     >> return Place
                        , string "query"     >> return Query ] )
        <* colon


value = str <* colon

str = stringLiteral <|> anyStr

color = (try hex) <|> base10

hex = lexeme (char '0' >> (char 'x' <|> char 'X') >>
              (PixelRGBA8 <$> hdigit <*> hdigit <*> hdigit <*> hdigit))

hdigit = (fst . head . readHex) <$> count 2 hexDigit

base10 = do
    r <- word <* colon
    g <- word <* colon
    b <- word
    a <- option maxBound (colon *> word)
    return $ PixelRGBA8 r g b a

word = (fromIntegral . fst . head . readDec) <$> lexeme (many digit)

lexeme p = p <* spaces

colon = lexeme (char ':')

comma = lexeme (char ',')

commaSep p = lexeme (p `sepBy` comma)

stringLiteral = B.pack <$> lexeme ( between (char '"') (char '"') (many anyChar) )

anyStr = do
    s <- many $ noneOf ":,"
    return $ fst (B.spanEnd isSpace $ B.pack s)


----------------------------------------------------------------------------------
--- | Env

-- string to put in empty cells, quickfort accepts an empty string, "~", or "`" here
emptyCell = "~"

-- data Env = Env{ _width :: Int, _height :: Int, px :: PixelMap, s :: Builder}
data Env = Env{ px :: PixelMap, s :: Builder}
type EnvR = ExceptT Text (Reader Env)

runEnvR e = runReader (runExceptT e)

-- width :: EnvR Int
-- width = asks _width

-- height :: EnvR Int
-- height = asks _height

lookupPixel :: EnvR (PixelRGBA8 -> Builder)
lookupPixel = pxmap <$> asks px -- do --asks px >>= return . pxmap
    -- p <- asks px
    -- return (pxmap p)

pxmap :: PixelMap -> (PixelRGBA8 -> Builder)
pxmap m k = M.findWithDefault emptyCell k m

seperator :: EnvR Builder
seperator = asks s