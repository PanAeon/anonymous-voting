
-- see durable but cynical subscriber
module Main where

import System.ZMQ4.Monadic
import Data.ByteString.Char8 (unpack, pack)
import Control.Monad (unless)
import Data.Restricted
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import Control.Concurrent(forkIO, threadDelay)
-- OK:
-- register
-- start-poll
-- status
-- abort-poll
-- vote
-- now I need Tchan?

main :: IO ()
main = do 
    runZMQ $ do
    


        -- subscriber <- socket Sub
        -- setIdentity  (restrict "Hello") subscriber
        -- subscribe subscriber ""
         
        -- connect subscriber "ipc:///tmp/xsub" -- ! FIXME: random!
    
        publisher <- socket Pub
        
        setSendHighWM (restrict 1) publisher
        connect publisher  "tcp://127.0.0.1:1488"--"ipc:///tmp/xsub" -- bind or connect?
        forever $ do 
          send publisher [] "foo foo" -- flag, data
          liftIO $ putStrLn "wtf?"
          liftIO $ threadDelay $ 1 * 1000 * 1000

        liftIO $ putStrLn "wtf?"
        send publisher [] "bar bar" -- flag, data
        liftIO $ putStrLn "wtf?"
        send publisher [] "foo foo" -- flag, data
        send publisher [] "bar bar" -- flag, data
        liftIO $ putStrLn "wtf?"
        send publisher [] "bar bar" -- flag, data
        liftIO $ putStrLn "wtf?"
        send publisher [] $ "foo bar"

    inputChan <- atomically STM.newTChan :: IO (STM.TChan ())
    forkIO $ main' inputChan
    putStrLn "Press any key to start poll;"
    userPoll <- waitForUserStartPoll
    
    inputMsg <- waitForChan inputChan
    Async.waitEither userPoll inputMsg -- channel

    putStrLn "OK, something happening"
    
main' :: STM.TChan () -> IO ()
main' inputChan = runZMQ $ do
    


    -- subscriber <- socket Sub
    -- setIdentity  (restrict "Hello") subscriber
    -- subscribe subscriber ""
     
    -- connect subscriber "ipc:///tmp/xsub" -- ! FIXME: random!

    -- publisher <- socket Pub
    -- bind publisher  "tcp://*:5565"--"ipc:///tmp/xsub" -- bind or connect?
    -- setSendHighWM (restrict 1000) publisher

    -- -- send publisher [SendMore] "foo foo" -- flag, data
    -- send publisher [] "bar bar" -- flag, data
    -- liftIO $ putStrLn "wtf?"
    -- send publisher [] "bar bar" -- flag, data
    -- liftIO $ putStrLn "wtf?"
    -- send publisher [] "foo foo" -- flag, data
    -- send publisher [] "bar bar" -- flag, data
    -- liftIO $ putStrLn "wtf?"
    -- send publisher [] "bar bar" -- flag, data
    -- liftIO $ putStrLn "wtf?"
    -- -- send publisher [SendMore] $ "foo bar"
    -- -- getUpdates subscriber inputChan
    pure ()
    
waitForChan :: STM.TChan () -> IO (Async.Async ())
waitForChan tchan =   Async.async $ atomically $ STM.readTChan tchan


waitForUserStartPoll :: IO (Async.Async ())
waitForUserStartPoll = Async.async $ void getLine


    
getUpdates :: Receiver t => Socket z t -> STM.TChan ()  -> ZMQ z ()     
getUpdates sock chan = do
    msg <-  unpack <$> receive sock
    putStrLn msg
    liftIO $ atomically $ STM.writeTChan chan ()
    unless (msg == "END") (getUpdates sock chan)
    
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