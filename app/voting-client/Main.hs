
-- see durable but cynical subscriber
module Main where

import System.ZMQ4.Monadic
import Data.ByteString.Char8 (unpack, pack)
import Control.Monad (unless)
import Data.Restricted
import qualified Control.Concurrent.Async as Async

-- OK:
-- register
-- start-poll
-- status
-- abort-poll
-- vote
-- now I need Tchan?

main :: IO ()
main = runZMQ $ do
    putStrLn "Press any key to start poll;"
    userPoll <- liftIO $ waitForUserStartPoll
    liftIO $ Async.waitEither userPoll undefined


    subscriber <- socket Sub
    setIdentity  (restrict "Hello") subscriber
    subscribe subscriber ""
     
    connect subscriber "tcp://localhost:9000" -- ! FIXME: random!

    sync <- socket Push
    connect sync  "tcp://localhost:9001"
    send sync [] "" -- flag, data
    getUpdates subscriber
    pure ()
    
waitForUserStartPoll :: IO (Async.Async ())
waitForUserStartPoll = Async.async $ void getLine

waitForServerPull :: IO (Async.Async ())
waitForServerPull = undefined
    
getUpdates :: Receiver t => Socket z t -> ZMQ z ()     
getUpdates sock = do
    msg <-  unpack <$> receive sock
    putStrLn msg
    unless (msg == "END") (getUpdates sock)
    
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