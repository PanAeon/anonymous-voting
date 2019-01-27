module HRZ () where

import           Crypto.Random.Types (MonadRandom)
import Crypto.Number.Generate(generateMax)
import qualified Schnorr as Sch
import Schnorr(g,n)
import qualified CDS as CDS
import CDS(proveYes, proveNo, CDSProof, verifyCDS)
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
    gxi = expSafe g secret n
    



-- after the round finishes, each P[i] checks validity of proofs  and computes
-- g^y[i]

validateZKPs :: [(Integer, Sch.SchnorrProof)] -> Bool
validateZKPs  = all areValid
  where
    areValid (pk, proof) = Sch.verify pk proof

computeYi :: Integer  -> [Integer] -> Integer
computeYi i  ys = as * ( maybe (error "smth went wrong") id (inverse bs Sch.n))
  where
    n = length ys
    as = foldl' (\r j ->  (r * (ys !! j)) `mod` Sch.n ) 1 [0..(fromIntegral i)-1]
    bs = foldl' (\r j ->  (r * (ys !! j)) `mod` Sch.n ) 1 [(fromIntegral i)+1..n-1]

    
-- Round II, publish (g^xi yi) (g vi), and zkp that vi is one of [1,0]

roundTwo :: (MonadRandom m) => Integer -> Integer -> Integer -> Integer -> m (Integer, CDSProof)
roundTwo i xi vote g_yi =
    do
      proof <- case vote of 
                 0 -> proveNo g_yi i xi
                 1 -> proveYes g_yi i xi
      pure (token, proof) 
  where
    token = ((expSafe g_yi xi n) * (expSafe g vote n)) `mod` n

-- ! FIXME: Either (Error "such and such is mudak") Integer
computeTally :: [(Integer, CDSProof)] -> Integer
computeTally xs = maybe (negate 1) id $ find (\i -> expSafe g i n == p) [0..1024]
  where
    p = product ( fst <$> xs) `mod` n
    
validateTally :: [Integer] -> [Integer] -> [(Integer, CDSProof)] -> Either String ()
validateTally hs xs proofs = if null ys
                             then Right ()
                             else Left $ format ys
  where
    ys = filter notValid $ zip [0..] (zip hs (zip xs proofs))
    format errors = join $ intersperse "," ( formatSingle <$> errors) 
    formatSingle (i, (h, (g_xj, (token, proof)))) = "Not valid vote: " ++ show i
    notValid (i, (h, (g_xj, (token, proof)))) = 
        token /= (CDS._y proof) || not (verifyCDS i h g_xj proof)  
-------------------------

-- NOW just put all together 
-- ... and tests of course

allShitTogether :: Integer -> Integer -> IO ()
allShitTogether v0 v1 = 
    do
      putStrLn "hi there"
      x0 <- generateSecret
      x1 <- generateSecret
      (gx0, p0) <- partFirstRound 0 x0
      (gx1, p1) <- partFirstRound 1 x1
      putStrLn $ "Round1, voter0 valid? " ++ show (validateZKPs [(gx0, p0)])
      putStrLn $ "Round1, voter1 valid? " ++ show (validateZKPs [(gx1, p1)])
      let 
        gy0 = computeYi 0 [gx0, gx1]
        gy1 = computeYi 1 [gx0, gx1]
      (token0, proof0) <- roundTwo 0 x0 v0 gy0
      (token1, proof1) <- roundTwo 1 x1 v1 gy1
      let
        tally = computeTally [(token0, proof0), (token1, proof1)]
        errorOrNohting = validateTally [gy0, gy1] [gx0, gx1] [(token0, proof0), (token1, proof1)]
      putStrLn $ "And the result is: " ++ show tally
      putStrLn $ "Errors? " ++ either id (const "Nope. All clear") errorOrNohting
      putStrLn "\nThat's all folks!" 

        
   
    


-- TODO: put your shit together
-- TODO: baby step - giant step, plz
-- TODO: unit tests
-- TODO: math on one-out-of-two-proof-of-knowledge
-- TODO: import schnorr-nizk, use Curve25519
-- TODO: extension for multiple candidates  
-- TODO: interactive one-out-of-two proof of knowledge?


{-

verifyCDS ::
     Integer
  -> Integer -- g^yi
  -> Integer -- g^xj
  -> CDSProof         
  -> Bool
verifyCDS  i h x  (CDSProof y a1 b1 a2 b2 d1 d2 c r1 r2) =
-}