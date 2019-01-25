module HRZ () where

import           Crypto.Random.Types (MonadRandom)
import Crypto.Number.Generate(generateMax)
import qualified Schnorr as Sch
import Crypto.Number.ModArithmetic
import Data.List((!!))
-- * From "Anonymous voting by two-round public discussion", 2010
-- * F. Hao P.Y.A. Ryan P.Zielinski
-- another impl: https://github.com/stonecoldpat/anonymousvoting
-- wiki page: https://en.wikipedia.org/wiki/Open_vote_network


-- data Proof = Proof 


-- 1. Each participant P[i] selects a random value as the secret x[i]

generateSecret :: MonadRandom m => m Integer
generateSecret = generateMax Sch.n

-- 2. Round 1 
--   P[i] publishes g^x[i], and a ZKP for x[i]

partFirstRound :: MonadRandom m => Integer -> Integer -> m (Integer, Sch.SchnorrProof)
partFirstRound _id secret = 
    do
      proof <- Sch.prove _id (gxi, secret)
      pure (gxi, proof)
  where
    gxi = expSafe Sch.g secret Sch.n
    



-- after the round finishes, each P[i] checks validity of proofs  and computes
-- g^y[i]

validateZKPs :: [(Integer, Sch.SchnorrProof)] -> Bool
validateZKPs  = all areValid
  where
    areValid (pk, proof) = Sch.verify pk proof

computeYi :: Integer -> Integer -> Integer -> [Integer] -> Integer
computeYi i vi xi ys = (expSafe gyi xi Sch.n) * (expSafe Sch.g vi Sch.n)
  where
    n = length ys
    as = foldl' (\r j ->  (r * (ys !! j)) `mod` Sch.n ) 1 [0..(fromIntegral i)-1]
    bs = foldl' (\r j ->  (r * (ys !! j)) `mod` Sch.n ) 1 [(fromIntegral i)+1..n-1]
    gyi = as * ( maybe (error "smth went wrong") id (inverse bs Sch.n))
    
-- Round II, publish (g^xi yi) (g vi), and zkp that vi is one of [1,0]


-------------------------

