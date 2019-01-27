module Main where

import HRZ
import qualified Data.Text.Read as T
import qualified Data.List as L
import Schnorr(SchnorrProof)
import qualified Data.Text.IO as T -- ! FIXME: expose via prelude
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified  Data.ByteString.Base64 as B64
import Control.Monad.Loops -- ! FIXME: add to prelude
import CDS(CDSProof)
import System.IO(hFlush, hSetBuffering, BufferMode(..))
-- import String.Read(read)

-- n participants
-- ids?
-- return public key
-- consume n - 1 keys, validate, validate typos with retries
-- ask for vote
-- publish vote' and proof
-- ask for n - 1 votes, validate typos
-- validate votes
-- publish tally

-- TODO: add validations
-- TODO: publish checksum with msg??

unsafeRight :: Either a b -> b 
unsafeRight (Right a) = a
unsafeRight (Left _) = error "expected right!"

unsafeLeft :: Either a b -> a 
unsafeLeft (Right _) = error "expected left!"
unsafeLeft (Left a) = a 

unsafeGet :: Maybe a -> a
unsafeGet (Just x) = x
unsafeGet Nothing  = (error "expected something" )

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    putStrLn "Enter # of participants: "
    n <- (fst . unsafeRight) <$> (T.decimal @ Int <$> getLine)
    putStrLn "Enter your id (email address)"
    self <- getLine
    ids' <- sequence $ readParticipant <$> [2..n]
    let 
      ids = sort $ self : ids'
      i   = unsafeGet $ L.findIndex (== self) ids
    secretKey <- generateSecret
    (t1, p1) <- partFirstRound  (fromIntegral i) secretKey
    putStrLn "Your first round response is here:"
    T.putStrLn $ publishFirstRound self t1 p1
    putStrLn $ "Enter " <> show (n-1) <> " first round replies of other participants"
    xs' <- replicateM (n-1) readFirstRoundReply
    let
      gxs = sortOn fst ((T.unpack self, (t1, p1)): xs')
      validation1 = validateZKPs' gxs
    if isLeft validation1
    then putStrLn $ "Errors in zkp proofs in round1: \n" ++ (unsafeLeft validation1) ++ "\n"
    else do
      let
        bazz =  ( (fst . snd) <$> gxs)
        gys = (\j -> computeYi j bazz) <$> [0..(fromIntegral n)-1]
        gyi = computeYi (fromIntegral i) ( (fst . snd) <$> gxs)
      putStrLn "enter your vote, either '0' or '1' :"
      vote <- (fst . unsafeRight) <$> (T.decimal @ Integer <$> getLine)
      (t2, p2) <- roundTwo (fromIntegral i) secretKey vote gyi
      putStrLn "Your second round response is below:"
      T.putStrLn $ publishSecondRound self (t2,p2)
      hFlush stdout
      putStrLn $ "Enter " <> show (n-1) <> " second round replies of other participants"
      hFlush stdout
      xxs' <- replicateM (n-1) readSecondRoundReply
      putStrLn $ "Proofs are read, computing tally ... "
      let 
        zs = sortOn fst ((T.unpack self, (t2, p2)): xxs')
        tally = computeTally (snd <$> zs)
        errorOrNohting = validateTally gys ((fst . snd) <$> gxs) (snd <$> zs)
      if isLeft errorOrNohting
          then putStrLn $ "Errors in second round: \n" ++ (unsafeLeft errorOrNohting) ++ "\n"
          else do 
                 putStrLn "***"
                 putStrLn "Vote is successful!"
                 putStrLn $ "Total tally is" ++ show tally
                 putStrLn "***"

-- TODAY IS NOT MY BEST PRogramming day
{-
 let
        errorOrNohting = validateTally [gy0, gy1] [gx0, gx1] [(token0, proof0), (token1, proof1)]
      putStrLn $ "And the result is: " ++ show tally
      putStrLn $ "Errors? " ++ either id (const "Nope. All clear") errorOrNohting
      putStrLn "\nThat's all folks!"
-}

readParticipant :: Int -> IO Text
readParticipant i = 
  do
    putStrLn $ "Enter name for participant #" ++ show i
    getLine

publishFirstRound :: Text -> Integer -> SchnorrProof -> Text
publishFirstRound id token proof = 
    "#####\n" <>
    "round 0\n" <>
    "id: " <> id <> "\n" <>
    toBase64 plainToken <> "\n###\n" <>
    toBase64 plainProof <> "\n" <>
    "#####\n"
  where
    plainToken = show token
    plainProof = show proof 

publishSecondRound :: Text -> (Integer, CDSProof)-> Text
publishSecondRound id_ foo = 
    "#####\n" <>
    "round 1\n" <>
    "id: " <> id_ <> "\n" <>
    toBase64 (show foo) <> "\n" <>
    "#####\n"

readSecondRoundReply :: IO (String, (Integer, CDSProof))
readSecondRoundReply =
  do
    getLine
    getLine
    id'' <- getLine
    let
      id' = T.strip $ T.drop 3 id''
    hFlush stdout
    token' <- unfoldM $ do
        l <- T.strip <$> getLine
        if l == "#####"
        then pure $ Nothing
        else pure $ Just l
    let 
      token = fromBase64 $ traceShowId $ foldr (<>) "" token'
      (v,p)  = fst $ L.head $ reads $ T.unpack token :: (Integer, CDSProof)
    pure (T.unpack id', (v, p))


readFirstRoundReply :: IO (String, (Integer, SchnorrProof))
readFirstRoundReply =
  do
    getLine
    getLine
    id'' <- getLine
    let
      id' = T.strip $ T.drop 3 id''
    token' <- unfoldM $ do
                l <- getLine
                if l == "###"
                then pure $ Nothing
                else pure $ Just l
    let 
      token = fromBase64 $ T.strip $ foldr (<>) "" token'
    proof' <- unfoldM $ do
        l <- getLine
        if l == "#####"
        then pure $ Nothing
        else pure $ Just l
    let
      proof'' = fromBase64 $ T.strip $ foldr (<>) "" proof'
      secret  = fst $ L.head $ reads $ T.unpack proof'' :: SchnorrProof -- Could have just serialized tuple... the dumb and the dumbest
    pure (T.unpack id', (fst $ unsafeRight $ T.decimal token, secret)) -- ! FUCK READS! use megaparsec

toBase64 :: Text -> Text
toBase64 xs = decodeUtf8 $ B64.encode $ encodeUtf8 xs

fromBase64 :: Text -> Text
fromBase64 xs = decodeUtf8 $ either (\x -> error ( T.pack x)) id $ B64.decode $ encodeUtf8 xs

