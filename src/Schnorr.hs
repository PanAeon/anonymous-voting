module Schnorr() where




import           Crypto.Random.Types (MonadRandom)
import  Crypto.ECC.Edwards25519
import Crypto.Hash
import           Crypto.Number.Serialize(os2ip)
import           Crypto.Error(throwCryptoError)


-- * Demonstrate the knowledge of the exponent without revealing it

data Proof = Proof {
     
             } deriving (Eq, Show)

prove :: MonadRandom m =>  (Point, Scalar) -> m Proof
prove (public, secret) = undefined

verify :: Point -> Proof -> Bool
verify public proof = undefined

--- Utils ---

-- ! FIXME: handle more gracefully? (we know it can't fail, right?)
hash :: ByteString -> Scalar
hash = throwCryptoError . scalarDecodeLong . hashWith SHA256

exHash = scalarDecodeLong (hashWith SHA256 ("foo" <> "bar" :: ByteString))
  where
    es2 = os2ip (hashWith SHA256 ("foo" <> "bar" :: ByteString))
exampleHash = os2ip (hashWith SHA256 ("foo" <> "bar" :: ByteString)) `mod` ecc_n
