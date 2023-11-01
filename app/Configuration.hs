module Configuration where

import Data.Map (Map)
import Data.Yaml (decodeFileThrow)

type Configuration = Map String String

loadConfiguration :: FilePath -> IO Configuration
loadConfiguration = decodeFileThrow