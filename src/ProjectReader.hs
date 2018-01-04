module ProjectReader
    ( readProject
    ) where

import PackageParser
import ErdlParser
import CommonTypes
import PackageDescription

-- import System.Directory (listDirectory)
import System.FilePath ((</>))
import qualified System.IO.Strict as IO


readProject :: FilePath -> IO PackageFileDescription
readProject dir = do
    let packageFilePath = dir </> ".package"
    content <- IO.readFile packageFilePath
    let parsed = parsePackageFile content
    case parsed of Right pfd -> return pfd
                   Left err -> fail $ show err
