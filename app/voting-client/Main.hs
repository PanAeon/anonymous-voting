
-- see durable but cynical subscriber
module Main where

import HRZ

import System.ZMQ4.Monadic
import Data.ByteString.Char8 (unpack, pack)
import Control.Monad (unless)
import Data.Restricted
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import Control.Concurrent(forkIO, threadDelay)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Catch
import Text.Read(read)
import Control.Monad.Loops(whileM)

import Schnorr(SchnorrProof)
import CDS(CDSProof)
import qualified  Data.ByteString.Base64 as B64
import qualified Data.List as L
import qualified Data.Text as T
import qualified System.IO
import System.IO(hFlush, hSetBuffering, BufferMode(..))
-- OK:
-- register
-- start-poll
-- status
-- abort-poll
-- vote
-- now I need Tchan?

data MyEvent = StartPoll
           | FirstRoundReply Int Integer SchnorrProof
           | SecondRoundReply Int Integer CDSProof  deriving (Eq, Show, Read)

main :: IO ()
main = do
    -- hSetBuffering stdin NoBuffering
    -- hSetBuffering stdout NoBuffering
    inputChan <- atomically STM.newTChan :: IO (STM.TChan ByteString)
    outputChan <- atomically STM.newTChan :: IO (STM.TChan ByteString) -- unbounded !!!
    -- _idvar <- atomically $  STM.newTVar (negate 1)
    void $ runZMQ $ do
        async tryStartProxyServer
        async runIdServer

        clientSocket <- socket Req
        connect clientSocket "tcp://localhost:1477"

        _id <- receiveNewId clientSocket

        -- liftIO $ atomically $ writeTVar _idvar _id -- ugly-ugly

        subscriber <- socket Sub
        setIdentity  (restrict $ "Client: " <> (BS.pack (show _id))) subscriber
        subscribe subscriber ""

        connect subscriber "tcp://127.0.0.1:1499" -- TODO: ipc?

        -- async $ getUpdates subscriber inputChan

        publisher <- socket Pub

        -- setSendHighWM (restrict 1) publisher
        connect publisher  "tcp://127.0.0.1:1488"--"ipc:///tmp/xsub" -- bind or connect?

        async $ pushUpdates publisher outputChan
        async $ getUpdates subscriber inputChan
        liftIO $ if _id == 0
          then do 
                 putStrLn "Press ⏎  to start poll"
                 userPoll <- waitForUserStartPoll
                 Async.wait userPoll
                 atomically $ STM.writeTChan outputChan (show StartPoll)
          else do 
                 putStrLn "Waiting for the poll to start..."
                 inputMsg <- waitForChan inputChan
                 Async.wait inputMsg
                 pure ()

        -- liftIO $ do
        --     putStrLn "Press any key to start poll;"
        --     userPoll <- waitForUserStartPoll
        --     inputMsg <- waitForChan inputChan
        --     -- foo <- Async.waitEither userPoll inputMsg -- channel
        --     -- hFlush stdin
        --     case foo of
        --       Left _ -> do 
        --                    atomically $ STM.writeTChan outputChan (show StartPoll)
        --                    System.IO.hGetChar stdin
        --                    pure ()
        --       Right _ -> pure ()
            -- liftIO $ threadDelay 500
            
        liftIO $ putStrLn "Starting vote"
        numParticipants <- getNumParticipants clientSocket
        liftIO $ putStrLn $ "numParticipants: " <> (show numParticipants)
        -- let 
        --     allIds = (\i -> "Client: " ++ (show i)) <$> [0..numParticipants]
        --     myId   = "Client: " ++ (show _id)
        (secretKey, t1,p1) <- liftIO $ do
            secretKey <- generateSecret
            (t1, p1) <- partFirstRound  (fromIntegral _id) secretKey
            pure (secretKey, t1, p1)
        putStrLn "Sending first round response..."
        publishFirstRound publisher _id t1 p1
        -- await n responses
        liftIO $ threadDelay 500
        firstRoundReplies' <- liftIO $  awaitFirstRoundReplies inputChan (numParticipants) -- FIXME: check 1!
        let firstRoundReplies = sortOn frrId $ {-(FirstRoundReply _id t1 p1) : -}  firstRoundReplies'
        putStrLn "Validating first round responses?"
        let firstRoundReplies'' = foobar <$> firstRoundReplies
            firstRoundValidation = validateZKPs' firstRoundReplies''
        either (\err -> error (T.pack $ "First round validation invalid" ++ err)) (\z -> pure ()) firstRoundValidation
        liftIO $ putStrLn ">First Round Validation Succeeded<"
        let
          bazz =  ( (fst . snd) <$> firstRoundReplies'')
          gys = (\j -> computeYi j bazz) <$> [0..(fromIntegral numParticipants)-1]
          gyi = computeYi (fromIntegral _id) ( (fst . snd) <$> firstRoundReplies'')
        liftIO $ putStrLn "enter your vote, either '0' or '1' :"
        vote <- liftIO $ (read @ Integer . T.unpack) <$> getLine
        (t2, p2) <- liftIO $ roundTwo (fromIntegral _id) secretKey vote gyi
        publishSecondRound publisher _id t2 p2
        -- await second round replies 
        liftIO $ threadDelay 500
        secondRoundReplies' <- liftIO $ awaitSecondRoundReplies inputChan (numParticipants) -- FIXME: check 1!
        liftIO $ putStrLn $ "Proofs are read, computing tally ... "
        let 
          secondRoundReplies = sortOn srrId $ {-(SecondRoundReply _id t2 p2) :-} secondRoundReplies'
          zs = foobaz <$> secondRoundReplies
          tally = computeTally (snd <$> zs)
          errorOrNohting = validateTally gys ((fst . snd) <$> firstRoundReplies'') (snd <$> zs)
        putStrLn (show secondRoundReplies)
        either (\err -> error (T.pack $ "Second round validation invalid:\n" ++ err ++ "^^^")) (\z -> pure ()) errorOrNohting 
        liftIO $ do
                 putStrLn "***"
                 putStrLn "Vote is successful!"
                 putStrLn $ "Total tally is: " ++ show tally
                 putStrLn "***"
    
 
        
    

frrId (FirstRoundReply _id _ _) = _id
srrId (SecondRoundReply _id _ _) = _id

foobar (FirstRoundReply _id t p) = (show _id, (t,p))
foobaz (SecondRoundReply _id t p) = (show _id, (t,p))
-- [(String, (Integer, Sch.SchnorrProof))]

publishFirstRound :: Socket z Pub -> Int -> Integer -> SchnorrProof -> ZMQ z ()
publishFirstRound publisher _id token proof = send publisher [] (show $ FirstRoundReply _id token proof)

publishSecondRound :: Socket z Pub -> Int -> Integer -> CDSProof -> ZMQ z ()
publishSecondRound publisher _id token proof = send publisher [] (show $ SecondRoundReply _id token proof)

awaitFirstRoundReplies :: STM.TChan ByteString -> Int -> IO [MyEvent]
awaitFirstRoundReplies input 0 = pure []
awaitFirstRoundReplies input n = 
    do
      msg <- atomically $ STM.readTChan input
    --   putStrLn $ "got msg: " ++ (unpack msg)
      let msg' = read (unpack msg) :: MyEvent
      case msg' of
        rply@(FirstRoundReply _ _ _) -> do 
                                          ys <- awaitFirstRoundReplies input (n-1)
                                          pure $ rply:ys
        _ -> awaitFirstRoundReplies input n
    
awaitSecondRoundReplies :: STM.TChan ByteString -> Int -> IO [MyEvent]
awaitSecondRoundReplies input 0 = pure []
awaitSecondRoundReplies input n = 
    do
      msg <- atomically $ STM.readTChan input
    --   putStrLn $ "got msg: " ++ (unpack msg)
      let msg' = read (unpack msg) :: MyEvent
      case msg' of
        rply@(SecondRoundReply _ _ _) -> do 
                                          ys <- awaitSecondRoundReplies input (n-1)
                                          pure $ rply:ys
        _ -> awaitSecondRoundReplies input n
      


receiveNewId :: Socket z Req -> ZMQ z Int
receiveNewId clientSocket = do
    liftIO $ putStrLn "obtaining an id..."
    send clientSocket [] "GetID"
    _id <- receive clientSocket
    liftIO . putStrLn $ "Received ID: " ++ show _id
    pure (read (unpack _id) :: Int)

getNumParticipants :: Socket z Req -> ZMQ z Int
getNumParticipants clientSocket = do
    liftIO $ putStrLn "getting num participants..."
    send clientSocket [] "GetNumClients"
    n <- unpack <$> receive clientSocket
    liftIO . putStrLn $ "Received ID: " ++  n
    pure (read n :: Int)

runIdServer :: ZMQ z ()
runIdServer = handleErrors $ flip evalStateT 0 $ do
    responder <- lift $ socket Rep
    lift $ bind responder "tcp://*:1477"
    liftIO $ putStrLn "Started Id server."

    forever $ do
        buffer <- lift $ receive responder
        case buffer of 
            "GetID" -> sendNewId responder
            "GetNumClients" -> sendNumClients responder
  where
    handler z =  (putStrLn "Id server port busy. It must be started elsewhere.")
    handleErrors x = catchAll x handler

sendNewId :: Socket z Rep -> StateT Int (ZMQ z) ()
sendNewId responder = do
        i <- get
        put (i + 1)
        lift $ send responder [] (show i)

sendNumClients :: Socket z Rep -> StateT Int (ZMQ z) ()
sendNumClients responder = do
        i <- get
        lift $ send responder [] (show i)

waitForChan :: STM.TChan ByteString -> IO (Async.Async ByteString)
waitForChan tchan =   Async.async $ atomically $ STM.readTChan tchan


waitForUserStartPoll :: IO (Async.Async ())
waitForUserStartPoll = Async.async $ void $ getLine -- System.IO.getLine --

-- getupdates:: Receiver t => Socket z t  -> ZMQ z ()
-- getupdates sock = do
--      msg <- unpack <$> receive sock
--      putStrLn  $  "received" <> msg
--      getupdates sock


getUpdates :: Receiver t => Socket z t -> STM.TChan ByteString  -> ZMQ z ()
getUpdates sock chan = do
    msg <-  receive sock
    -- liftIO $ putStrLn ("get:" ++ unpack msg)
    liftIO $ atomically $ STM.writeTChan chan msg
    unless (msg == "END") (getUpdates sock chan)

-- pushUpdates :: Socket z Pub -> STM.TChan ByteString  -> ZMQ z ()
pushUpdates sock chan = do
    msg <-   liftIO $ atomically $ STM.readTChan chan
    -- liftIO $ putStrLn ("push:" ++ unpack msg)
    send sock [] msg -- flag, data
    unless (msg == "END") (pushUpdates sock chan) 


tryStartProxyServer :: ZMQ z ()
tryStartProxyServer = handleErrors  $ do
    xsub <- socket XSub
    bind xsub "tcp://127.0.0.1:1488"

    xpub <- socket XPub
    bind xpub "tcp://127.0.0.1:1499"

    liftIO $ putStrLn "Starting proxy server on 1488/1489"
    proxy xsub xpub Nothing
  where
    handler z =  (putStrLn "Port is busy. Trying to connect to proxy.")
    handleErrors x = catchAll x handler
-- withContext 1 $ \context -> do
    -- withSocket context Sub $ \subscriber -> do
    --     setOption subscriber (Identity "Hello")
    --     subscribe subscriber ""
    --     connect subscriber "tcp://localhost:5565"
    --     withSocket context Push $ \sync -> do
    --         connect sync "tcp://localhost:5564"
    --         send sync (pack "") []
    --         getUpdates subscriber

-- main :: IO ()
-- main = runZMQ $ do
--     liftIO $ putStrLn "Connecting to hello world server…"

--     requester <- socket Req
--     connect requester "tcp://localhost:5555"

--     forM_ [1..10] $ \i -> do
--         liftIO . putStrLn $ "Sending Hello " ++ show i ++ "…"
--         send requester [] "Hello"
--         _ <- receive requester
--         liftIO . putStrLn $ "Received World " ++ show i

-- pure ()

        -- forever $ do
        --   send publisher [] "foo foo" -- flag, data
        --   liftIO $ putStrLn "wtf?"
        --   liftIO $ threadDelay $ 1 * 1000 * 1000

    
    -- forkIO $ main' inputChan
    -- whileM ( (< 0) <$> (readTVarIO _idvar)) (threadDelay $ 25 * 1000)
    

   





    -- subscriber <- socket Sub
    -- setIdentity  (restrict "Hello") subscriber
    -- subscribe subscriber ""

    -- connect subscriber "ipc:///tmp/xsub" -- ! FIXME: random!

    -- publisher <- socket Pub
    -- bind publisher  "tcp://*:5565"--"ipc:///tmp/xsub" -- bind or connect?
    -- setSendHighWM (restrict 1000) publisher