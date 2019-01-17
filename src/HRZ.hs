module HRZ () where

import           Crypto.Random.Types (MonadRandom)
import  Crypto.ECC.Edwards25519


-- * From "Anonymous voting by two-round public discussion", 2010
-- * F. Hao P.Y.A. Ryan P.Zielinski
-- another impl: https://github.com/stonecoldpat/anonymousvoting
-- wiki page: https://en.wikipedia.org/wiki/Open_vote_network


data Proof = Proof 


-- 1. Each participant P[i] selects a random value as the secret x[i]

generateSecret :: MonadRandom m => m Scalar
generateSecret = scalarGenerate

-- 2. Round 1 
--   P[i] publishes g^x[i], and a ZKP for x[i]

partFirstRound :: MonadRandom m => Scalar -> m (Point, Proof)
partFirstRound secret = undefined
  where
    gxi = toPoint secret



-- after the round finishes, each P[i] checks validity of proofs  and computes
-- g^y[i]

-------------------------

