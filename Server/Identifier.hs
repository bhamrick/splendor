module Server.Identifier where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Base64.URL as B64
import System.IO

newIdentifier :: IO String
newIdentifier = withFile "/dev/urandom" ReadMode $ \h -> do
    bytes <- BS.hGet h 20
    pure $ CBS.unpack (B64.encode bytes)
