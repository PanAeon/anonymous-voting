module Schnorr() where




import           Crypto.Random.Types (MonadRandom)
import  Crypto.ECC.Edwards25519
import Crypto.Hash
-- import           Crypto.Number.Serialize(os2ip)
import           Crypto.Error(throwCryptoError)

-- https://safecurves.cr.yp.to/
-- * Demonstrate the knowledge of the exponent without revealing it

data Proof = Proof {
     
             } deriving (Eq, Show)

prove :: MonadRandom m => Int -> (Point, Scalar) -> m Proof
prove i (gxi, xi) = 
  do
    v <- scalarGenerate
    
    let
      gv = toPoint v
      z = hash' "" 
    --   v' = scalarEncode v, then substract `mod` n?
      r = undefined -- v - (scalarMul xi  z)
    pure Proof
 
verify :: Point -> Proof -> Bool
verify public proof = undefined

--- Utils ---

-- ! FIXME: handle more gracefully? when does it fail?
hash' :: ByteString -> Scalar
hash' = throwCryptoError . scalarDecodeLong . hashWith SHA256

-- exHash = scalarDecodeLong (hashWith SHA256 ("foo" <> "bar" :: ByteString))
--   where
--     es2 = os2ip (hashWith SHA256 ("foo" <> "bar" :: ByteString))
-- exampleHash = os2ip (hashWith SHA256 ("foo" <> "bar" :: ByteString)) `mod` ecc_n
