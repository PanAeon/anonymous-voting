
-- see durable but cynical subscriber
module Main where

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
-- OK:
-- register
-- start-poll
-- status
-- abort-poll
-- vote
-- now I need Tchan?

data Event = StartPoll
           | StartPollReply Int deriving (Eq, Show, Read)

main :: IO ()
main = do
    inputChan <- atomically STM.newTChan :: IO (STM.TChan ())
    outputChan <- atomically STM.newTChan :: IO (STM.TChan ByteString)
    _idvar <- atomically $  STM.newTVar (negate 1)
    forkIO $ runZMQ $ do
        async tryStartProxyServer
        async runIdServer

        clientSocket <- socket Req
        connect clientSocket "tcp://localhost:1477"

        _id <- receiveNewId clientSocket

        liftIO $ atomically $ writeTVar _idvar _id -- ugly-ugly

        subscriber <- socket Sub
        setIdentity  (restrict "Hello") subscriber
        subscribe subscriber ""

        connect subscriber "tcp://127.0.0.1:1499" -- ! FIXME: random!

        -- async $ getUpdates subscriber inputChan

        publisher <- socket Pub

        -- setSendHighWM (restrict 1) publisher
        connect publisher  "tcp://127.0.0.1:1488"--"ipc:///tmp/xsub" -- bind or connect?

        async $ pushUpdates publisher outputChan
        getUpdates subscriber inputChan
        -- forever $ do
        --   send publisher [] "foo foo" -- flag, data
        --   liftIO $ putStrLn "wtf?"
        --   liftIO $ threadDelay $ 1 * 1000 * 1000

    
    -- forkIO $ main' inputChan
    whileM ( (< 0) <$> (readTVarIO _idvar)) (threadDelay $ 25 * 1000)
    putStrLn "Press any key to start poll;"
    userPoll <- waitForUserStartPoll

    inputMsg <- waitForChan inputChan
    foo <- Async.waitEither userPoll inputMsg -- channel
    case foo of
      Left _ -> atomically $ STM.writeTChan outputChan (show StartPoll)
      Right _ -> pure ()

    putStrLn "OK, something happening"





    -- subscriber <- socket Sub
    -- setIdentity  (restrict "Hello") subscriber
    -- subscribe subscriber ""

    -- connect subscriber "ipc:///tmp/xsub" -- ! FIXME: random!

    -- publisher <- socket Pub
    -- bind publisher  "tcp://*:5565"--"ipc:///tmp/xsub" -- bind or connect?
    -- setSendHighWM (restrict 1000) publisher

 



receiveNewId :: Socket z Req -> ZMQ z Int
receiveNewId clientSocket = do
    liftIO $ putStrLn "obtaining an id..."
    send clientSocket [] "GetID"
    _id <- receive clientSocket
    liftIO . putStrLn $ "Received ID: " ++ show _id
    pure (read (unpack _id) :: Int)

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

waitForChan :: STM.TChan () -> IO (Async.Async ())
waitForChan tchan =   Async.async $ atomically $ STM.readTChan tchan


waitForUserStartPoll :: IO (Async.Async ())
waitForUserStartPoll = Async.async $ void getLine

-- getupdates:: Receiver t => Socket z t  -> ZMQ z ()
-- getupdates sock = do
--      msg <- unpack <$> receive sock
--      putStrLn  $  "received" <> msg
--      getupdates sock

getUpdates :: Receiver t => Socket z t -> STM.TChan ()  -> ZMQ z ()
getUpdates sock chan = do
    msg <-  unpack <$> receive sock
    putStrLn msg
    liftIO $ atomically $ STM.writeTChan chan ()
    unless (msg == "END") (getUpdates sock chan)

pushUpdates :: Socket z Pub -> STM.TChan ByteString  -> ZMQ z ()
pushUpdates sock chan = do
    msg <-   liftIO $ atomically $ STM.readTChan chan
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