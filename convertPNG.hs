{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Either
import Data.Tuple
import Data.Aeson
import Data.List(unzip4)
import Codec.Picture.Png(decodePng)
--import Codec.Picture.Types(DynamicImage(..),Pixel(..),PixelRGBA16(..),Image(..),
--                           ColorConvertible(..),pixelFold,promotePixel)
import Codec.Picture.Types
import qualified Data.ByteString as L
import System.Environment(getArgs)
import System.Console.CmdLib
import Control.Monad


data Main = Main { start :: [Int], input :: String,
                   output :: String, phases :: String,
                   singlePhasePNG :: Bool }
    deriving (Typeable, Data, Eq)

instance Attributes Main where
    attributes _ = group "Options" [
        start          %> [ Help "Start position of the macro.",
                            ArgHelp "(X,Y)",
                            Default "0,0",
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
                            Default "All",
                            Short ['p'] ],
        singlePhasePNG %> [ Help "If True, a pixel represents a single instruction. If False \
                                 \a pixel represents an instruction for each phase",
                            Long ["single-phase-png"],
                            Default False ]
        ]

instance RecordCommand Main where
    mode_summary _ = "Simple program to convert RGBA encoded .png images into quickfort blueprints"

data Phase = All
           | Dig
           | Build
           | Place
           | Query
    deriving (Typeable, Data, Eq, Read, Show)

data Decoder = MultiPhase  { mdig   :: M.Map Int String,
                             mbuild :: M.Map Int String,
                             mplace :: M.Map Int String,
                             mquery :: M.Map Int String }
             | SinglePhase { sdig   :: M.Map LongPixel String,
                             sbuild :: M.Map LongPixel String,
                             splace :: M.Map LongPixel String,
                             squery :: M.Map LongPixel String }

instance ColorConvertible PixelRGBA8 PixelRGBA16 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelRGBA8 r g b a) = PixelRGBA16 (fromIntegral r)
                                                    (fromIntegral g)
                                                    (fromIntegral b)
                                                    (fromIntegral a)

main = getArgs >>= executeR Main {} >>= \opts ->
    do
        {-
        putStrLn $ show (start opts)
        putStrLn (input opts)
        putStrLn (output opts)
        putStrLn $ show (phases opts)
        putStrLn $ show (singlePhasePNG opts)
        -}
        --imgFiles = words input opts
        decoderStr <- readFile "pngconfig.json"
        imgStrs <- mapM readFile $ words $ input opts
        (strs,imgs) <- liftM partitionEithers $ map decodePng imgStrs
        putStrLn $ show strs


convertMultiPhase :: [Either String DynamicImage] -> L.ByteString -> Maybe Phase ->
                      Either String BStringTuple
convertMultiPhase imgStr decStr phase | phase == Nothing = Left "Ivalid phase declaration"
                                      | strs /= null = Left $ unlines strs
                                      | decoder == Nothing = Left "Error parsing pngconfig.json"
                                      | otherwise = convertMultiPhase' imgs (fromJust decoder) (fromJust phase)
    where (strs,imgs) = partitionEithers $ map decodePng imgStr
          decoder     = decode decStr :: Maybe (M.Map String LongPixel)
          convertMultiPhase' = undefined

type LongPixel = (Int,Int,Int,Int)
type BStringTuple = (L.ByteString,L.ByteString,L.ByteString,L.ByteString)

-- color representations:
-- base ten: <val>:<val>:<val>:<val>
-- hex: #<val><val><val><val> or 0x<val><val><val><val>

invert :: (Ord b) => M.Map a b -> M.Map b a
invert = M.fromList . swap . M.toList


parseDig :: [Int] -> (Int -> String) -> Maybe String
parseDig commands mapping = undefined

parseBuild :: [Int] -> (Int -> String) -> Maybe String
parseBuild commands mapping = undefined

parsePlace :: [Int] -> (Int -> String) -> Maybe String
parsePlace commands mapping = undefined

parseQuery :: [Int] -> (Int -> String) -> Maybe String
parseQuery commands mapping = undefined

unzipImage4 :: L.ByteString -> Maybe ([Int],[Int],[Int],[Int])
unzipImage4 str = convert $ decodePng str
    where convert (Left s) = Nothing
          convert (Right img) = Just (unzip4 $ imageToList img)



imageToList :: DynamicImage -> Either String [LongPixel]
imageToList (ImageRGBA8 img) = Right $ reverse $ pixelFold pixPred [] img
imageToList (ImageRGBA16 img) = Right $ reverse $ pixelFold pixPred [] img
imageToList _ = Left "Unsupported png format, use RGBA8 or RGBA16 encoding"


pixPred :: (ColorConvertible a  PixelRGBA16) => [LongPixel] -> x -> x -> a -> [LongPixel]
pixPred acc _ _ pix = (promoteToTuple pix):acc

promoteToTuple :: (ColorConvertible pixel PixelRGBA16) => pixel -> LongPixel
promoteToTuple p = pixelToTuple p' 
    where p' = (promotePixel p)::PixelRGBA16


pixelToTuple :: PixelRGBA16 -> LongPixel
pixelToTuple (PixelRGBA16 r g b a) = (fromIntegral r, fromIntegral g, fromIntegral b,
                        fromIntegral a)