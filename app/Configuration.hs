module Configuration where

import Data.Map (Map)
import Data.Yaml (decodeFileThrow)

-- maps buildkite ids to a slack webhook urls
type Configuration = Map String String

loadConfiguration :: FilePath -> IO Configuration
loadConfiguration = decodeFileThrow
