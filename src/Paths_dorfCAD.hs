-- Dummy module for development purposes
-- Cabal only installs data files during a cabal install
-- So we use this file to get correct paths during development
module Paths_dorfCAD where

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return