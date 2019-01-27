module Schnorr(g,n, SchnorrProof, prove, verify ) where




import           Crypto.Random.Types (MonadRandom)
import Crypto.Hash
  
import           Crypto.Error(throwCryptoError)

-- import qualified Data.ByteString as S
import Crypto.Number.Generate(generateMax)
  
import Crypto.Number.ModArithmetic
import qualified Data.ByteArray as BA
import Crypto.Number.Serialize(os2ip)
import Utils
  
-- ! important remarks https://florianjw.de/en/insecure_generators.html

{-
Then pick a random element hh from the entire group and compute 
  g:=hp−1qg := h^{\frac{p-1}{q}}. 
  gg will now be a generator of the subgroup with qq elements. 
  This is less trivial then the case for the safe-primes and encoding your 
  messages as group-elements will definitely be more complicated as well 
  (I’m however not going to cover the reason here), but it is at least better
   then the alternatives.
-}

{-
From wikipedia:
The order of G should have a large prime factor to prevent use of the Pohlig–Hellman algorithm to obtain a or b. 
For this reason, a Sophie Germain prime q is sometimes used to calculate 
p = 2q + 1, called a safe prime, since the order of G is then 
  only divisible by 2 and q. g is then sometimes chosen to generate 
  the order q subgroup of G, rather than G, so that the Legendre symbol 
  of ga never reveals the low order bit of a. A protocol using such a 
  choice is for example IKEv2.[12]

g is often a small integer such as 2. Because of the random self-reducibility
 of the discrete logarithm problem 
a small g is equally secure as any other generator of the same group. 
-}

-- https://safecurves.cr.yp.to/
-- *
-- * Demonstrate the knowledge of the exponent without revealing it
-- *

-- elliptic curves beat me, trying integer groups, otherwise
-- will need to write my own Curve25519 or similar

-- * Constants from:
-- * https://www.ietf.org/rfc/rfc3526.txt
-- * 3. 2048-bit MODP Group

{-
    This group is assigned id 14.

   This prime is: 2^2048 - 2^1984 - 1 + 2^64 * { [2^1918 pi] + 124476 }

   Its hexadecimal value is:

      FFFFFFFF FFFFFFFF C90FDAA2 2168C234 C4C6628B 80DC1CD1
      29024E08 8A67CC74 020BBEA6 3B139B22 514A0879 8E3404DD
      EF9519B3 CD3A431B 302B0A6D F25F1437 4FE1356D 6D51C245
      E485B576 625E7EC6 F44C42E9 A637ED6B 0BFF5CB6 F406B7ED
      EE386BFB 5A899FA5 AE9F2411 7C4B1FE6 49286651 ECE45B3D
      C2007CB8 A163BF05 98DA4836 1C55D39A 69163FA8 FD24CF5F
      83655D23 DCA3AD96 1C62F356 208552BB 9ED52907 7096966D
      670C354E 4ABC9804 F1746C08 CA18217C 32905E46 2E36CE3B
      E39E772C 180E8603 9B2783A2 EC07A28F B5C55DF0 6F4C52C9
      DE2BCBF6 95581718 3995497C EA956AE5 15D22618 98FA0510
      15728E5A 8AACAA68 FFFFFFFF FFFFFFFF

   The generator is: 2.

-}

n = 0xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF6955817183995497CEA956AE515D2261898FA051015728E5A8AACAA68FFFFFFFFFFFFFFFF
g = 0x2

-- id, gv, r
data SchnorrProof = SchnorrProof {
               _id :: Integer
             , _gv :: Integer
             , _r  :: Integer
             } deriving (Eq, Show)
  
  
-- ! FIXME: read https://weakdh.org/imperfect-forward-secrecy-ccs15.pdf  


subM :: Integer -> Integer -> Integer
subM a b
  | a > b = (a - b) `mod` n
  | otherwise = ((a `mod` n) + n - (b `mod` n)) `mod` n
  -- | otherwise = error "sub b > n not implemented"
  
prove :: MonadRandom m => Integer -> (Integer, Integer) -> m SchnorrProof
prove i (gxi, xi) = 
  do
    v <- generateMax n
    let
      gv = expSafe g v n
      z = hash' $  (show g)
                <> (show gv)
                <> (show gxi)
                <> (show i)
      r =  v - (xi * z) -- ! FIXME: is this right? Do we need subM??? the answer is we DON'T!
    pure $ SchnorrProof i gv r
   
verify :: Integer -> SchnorrProof -> Bool
verify gxi (SchnorrProof i gv r) = 
      gv > 0 && gv < n &&
      gv ==  (gr * (expSafe gxi z n)) `mod` n
  where
    gr = expSafe g r n
    z = hash' $  (show g)
              <> (show gv)
              <> (show gxi)
              <> (show i)


  
