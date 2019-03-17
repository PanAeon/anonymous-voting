
-- See "Durable, but cynical publisher"
module Main where

import System.ZMQ4.Monadic
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.ByteString.Char8 (pack)
import Data.Restricted

main :: IO ()
main = runZMQ $ do
    sync <- socket Pull
    bind sync "tcp://*:9001"

    publisher <- socket Pub
    setReceiveHighWM (restrict 1) publisher
    bind publisher "tcp://*:9000"

    receive sync
    forM_ [1..10] $ \update_nbr ->
        do
            let string = "Update " ++ show update_nbr
            send publisher [] (pack string)
            liftIO $ threadDelay $ 1 * 1000 * 1000
    send publisher [] "END"

--             forM_ [1..10] $ \update_nbr -> do
--                 let string = "Update " ++ show update_nbr
--                 send publisher (pack string) []
--                 threadDelay $ 1 * 1000 * 1000
--             send sync (pack "END") []
    
    

--     withSocket context Pull $ \sync -> do
--         bind sync "tcp://*:5564"
--         withSocket context Pub $ \publisher -> do
--             setReceiveHighWM publisher (receiveHighWM 1)
--             -- setOption publisher (Swap 25000000) ???
--             bind publisher "tcp://*:5565"
--             receive sync []
--             forM_ [1..10] $ \update_nbr -> do
--                 let string = "Update " ++ show update_nbr
--                 send publisher (pack string) []
--                 threadDelay $ 1 * 1000 * 1000
--             send sync (pack "END") []

-- main :: IO ()
-- main = runZMQ $ do
    
--     subscriber <- socket Sub
--     setIdentity  (restrict "Hello") subscriber
--     subscribe subscriber ""
--      
--     connect subscriber "tcp://localhost:9000"

--     sync <- socket Push
--     connect sync  "tcp://localhost:9001"
--     send sync [] "" -- flag, data
--     getUpdates subscriber
--     pure ()