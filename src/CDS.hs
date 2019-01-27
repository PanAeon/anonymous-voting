module CDS(proveYes, proveNo, CDSProof(..), verifyCDS) where

import           Crypto.Random.Types (MonadRandom)
import Crypto.Number.Generate(generateMax)
import qualified Schnorr as Sch
import Schnorr(g,n)
import Crypto.Number.ModArithmetic
import Utils

-- * Cramer, Damgard and Schoenmakers
-- "Proofs of partial knowledge and simplified design
--  of witness hiding protocols", 1994
-- "A secure and optimally efficient multi authority election scheme"

-- * Should demonstrate that vote in {0,1} without revealing it


data CDSProof = CDSProof {
             --  _w  :: Integer
                _y :: Integer -- ECC.Point
             , _a1 :: Integer -- ECC.Point
             , _b1 :: Integer -- ECC.Point
             , _a2 :: Integer -- ECC.Point
             , _b2 :: Integer -- ECC.Point
             , _d1 :: Integer
             , _d2 :: Integer
             , _c  :: Integer
             , _r1  :: Integer
             , _r2  :: Integer
             } deriving (Show, Eq)

-- * Useful: https://en.wikipedia.org/wiki/Fiat%E2%80%93Shamir_heuristic



-- h is g^yi, x = g^xj (published), y == h^xj * g

-- ! FIXME: ok, we publish ZKP, how does this relates to vi, or more precisely
-- ! g^xiyi * g^vi ??? ?????, need to check that y is that value!!
-- ! Also define variables !
proveYes :: (MonadRandom m) => Integer
    -> Integer
    -> Integer
    -> m CDSProof
proveYes  h  i xj = do -- * h is g^yi
    w <- generateMax n
    r1 <- generateMax n
    d1 <- generateMax n
    let x = expSafe g xj n --pointMul xj h -- v == 1
        y =  ((expSafe h xj n) * g) `mod` n
        a1 = ((expSafe g r1 n) * (expSafe x d1 n)) `mod` n
        b1 = ((expSafe h r1 n) * (expSafe y d1 n)) `mod` n
        a2 = expSafe g w n
        b2 = expSafe h w n
        c  = genChallenge' i x y a1 b1 a2 b2
        d2 = c - d1
        r2 = w - (xj * d2)
    pure $ CDSProof y a1 b1 a2 b2 d1 d2 c r1 r2

proveNo :: (MonadRandom m) => Integer
    -> Integer
    -> Integer
    -> m CDSProof
proveNo  h  i xj = do -- * h is g^yi
    w <- generateMax n
    r2 <- generateMax n
    d2 <- generateMax n
    let x = expSafe g xj n --pointMul xj h -- v == 1
        y =  expSafe h xj n
        a1 = expSafe g w n
        b1 = expSafe h w n
        a2 = ((expSafe g r2 n) * (expSafe x d2 n) ) `mod` n
        inv_g = maybe (error "smth went wrong") id (inverse g n)
        b2 = ((expSafe h r2 n) * (expSafe ((y * inv_g) `mod` n) d2 n)) `mod` n
        c  = genChallenge' i x y a1 b1 a2 b2
        d1 = c - d2
        r1 = (w - (xj * d1)) 
    pure $ CDSProof y a1 b1 a2 b2 d1 d2 c r1 r2


verifyCDS ::
     Integer
  -> Integer -- g^yi
  -> Integer -- g^xj
  -> CDSProof         
  -> Bool
verifyCDS  i h x  (CDSProof y a1 b1 a2 b2 d1 d2 c r1 r2) =
--    trace ("checkA1: " ++ show checkA1 ++
--           "\ncheckB1: " ++ show checkB1 ++
--           "\ncheckA2: " ++ show checkA2 ++
--           "\ncheckB2: " ++ show checkB2 ++
--           "\nc ok: " ++ show (c == (d1+d2) `mod` n) ++
--           "\nc == c'? " ++ show (c == c')) $
   (c == (d1 + d2) `mod` n) &&  (c == c') &&  validPoints && 
   checkA1 &&  checkA2 &&  checkB1 &&  checkB2 
  where
    c' = genChallenge' i x y a1 b1 a2 b2
    validPoints = validPoint x && 
                  validPoint y && 
                  validPoint a1 && 
                  validPoint b1 && 
                  validPoint a2 && 
                  validPoint b2
                 

    checkA1 = a1 == ((expSafe g r1 n) * (expSafe x d1 n)) `mod` n
    checkB1 = b1 == ( (expSafe h r1 n) * (expSafe y d1 n)) `mod` n
    checkA2 = a2 == ( (expSafe g r2 n) * (expSafe x d2 n)) `mod` n
    checkB2 = b2 == ( (expSafe h r2 n) * (expSafe (y * inv_g) d2 n)) `mod` n
    inv_g = maybe (error "smth went wrong") id (inverse g n)
    validPoint p = p > 0 && p < n

genChallenge' :: Integer
   -> Integer -- ECC.Point
   -> Integer -- ECC.Point
   -> Integer -- ECC.Point
   -> Integer -- ECC.Point
   -> Integer -- ECC.Point
   -> Integer -- ECC.Point
   -> Integer
genChallenge'  i p0 p1 p2 p3 p4 p5 =
    hash'  (show i <> show p0 <> show p1 <> show p2 <> show p3 <> show p4 <> show p5 <> "")

{-
yi <- generateMax n
g_yi = expSafe g yi n
xj <- generateMax n
x = expSafe g xj n
yesProof <- proveYes g_yi 13 xj 
noProof <- proveNo g_yi 13 xj 
verify' 13 g_yi x noProof
-}    




