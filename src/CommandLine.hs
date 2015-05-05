{-# LANGUAGE DeriveDataTypeable #-}
module CommandLine(
      Opts(..)
    , Orientation(..)
    , options
    , phases
    , loadConfigFiles
    ) where

import Prelude hiding (repeat)

import Control.Monad(filterM)
import Data.Traversable(for)
import Data.Maybe(catMaybes)
import Data.List(uncons)
import Data.ByteString(ByteString)
import qualified Data.ByteString as B
import qualified Data.Set as S
import System.Console.CmdArgs
import System.FilePath
import System.EasyFile(getAppUserDataDirectory, doesFileExist)

import Types
import Paths_dorfCAD

data Orientation = FromBottom | FromTop
  deriving (Data, Eq, Show)


data Opts = Opts { start   :: Maybe (Int,Int,Int)
                 , absPos  :: Bool
                 , input   :: [String]
                 , output  :: String
                 , phases_ :: [Phase]
                 , repeat  :: Int
                 , config  :: Maybe String
                 , order   :: Orientation }
    deriving (Typeable, Data, Eq, Show)

options = Opts{ start  = def
                       &= typ "X,Y,Z"
                       &= help "Start position of the macro."
              , absPos = False
                       &= name "absolute-position"
                       &= explicit
                       &= typ "TRUE|FALSE"
                       &= help "use top left corner as origin regardless of other flags"
              , input  = def
                       &= args
                       &= typFile
              , output = def
                       &= typFile
                       &= help "Output filename"
              , phases_ = enum [ [] &= ignore
                               , [Dig] &= name "dig"
                               , [Build] &= name "build"
                               , [Place] &= name "place"
                               , [Query] &= name "query" ]
                       &= typ "[All|Dig|Build|Place|Query]"
                       &= explicit
                       &= help "Phase to create a blueprint for"
              , repeat = 0
                       &= typ "INTEGER"
                       &= help "Optional, specifies a number of times to repeat the blueprint"
              , config = def
                       &= typFile
                       &= help "Specify a config file to use instead of the default"
              , order  = enum [ FromTop    &= name "from-top"
                                           &= help "Order files from top to bottom"
                              , FromBottom &= name "from-bottom"
                                           &= help "Order files from bottom to top" ]
                       &= explicit
              }
              &= program "mkblueprint"
              &= summary "dorfCAD v1.2, (C) Leif Grele 2014"


phases :: Opts -> [Phase]
phases opts = phases' $ phases_ opts
  where phases' [] = [Dig,Build,Place,Query]
        phases' ps = filterDuplicates ps


-- remove all duplicate values in a list by converting it to a Set
-- does not preserve order
filterDuplicates :: Ord a => [a] -> [a]
filterDuplicates = S.toList . S.fromList


----------------------------------------------------------------------------------
--- | File loading

defAlias = "aliases"
usrAlias = "aliases-user"
defConfig = "config"
usrConfig = "config-user"

loadConfigFiles :: Opts -> IO (ByteString,ByteString)
loadConfigFiles opts = case config opts of
  Nothing -> do
    [a,c] <- loadDataFiles [[usrAlias,defAlias], [usrConfig,defConfig]]
    return (a,c)
  Just cfg -> do
    c <- B.readFile cfg
    [a] <- loadDataFiles [[usrAlias,defAlias]]
    return (a,c)

loadDataFiles :: [[FilePath]] -> IO [ByteString]
loadDataFiles fs = sequence [ getAppUserDataDirectory "dorfCAD", getDataDir ]
  >>= loadFilesWithFallbacks fs


loadFilesWithFallbacks :: [[FilePath]] -> [FilePath] -> IO [ByteString]
loadFilesWithFallbacks files dirs = for files $ \filenames -> do
  validName <- catMaybes <$> mapM (searchDir filenames) dirs
  if null validName
  then error $ errmsg filenames
  else B.readFile $ head validName

    where errmsg fs = "Could not find file matching one of: " ++ unwords fs ++
                      " in any of the directories: " ++ unwords dirs

searchDir :: [FilePath] -> FilePath -> IO (Maybe FilePath)
searchDir names dir = filterM (\n -> doesFileExist (dir </> n)) names
                  >>= return . fmap fst . uncons
