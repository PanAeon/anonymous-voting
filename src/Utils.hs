module Utils(hash') where

import           Crypto.Random.Types (MonadRandom)
import Crypto.Hash
  
import           Crypto.Error(throwCryptoError)

import Crypto.Number.Generate(generateMax)
  
import Crypto.Number.ModArithmetic
import qualified Data.ByteArray as BA
import Crypto.Number.Serialize(os2ip)
  
-- publicly agreed secure hash function H (oracle)
hash' :: ByteString -> Integer
hash' =  os2ip . hashWith SHA256