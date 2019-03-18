
-- See "Durable, but cynical publisher"
module Main where

import System.ZMQ4.Monadic
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS
import Data.Restricted
-- import qualified Data.ByteString as BS

-- provides common channel for publishers/subscribers .. using IPC?
main :: IO ()
main = runZMQ $ do
    xsub <- socket XSub
    bind xsub "tcp://127.0.0.1:1488"

    xpub <- socket XPub
    bind xpub "tcp://127.0.0.1:1499"

    proxy xsub xpub Nothing
--     setIdentity  (restrict "Hello") xsub
--     subscribe xsub ""
     --"ipc:///tmp/xsub"
    -- xpub <- socket XPub
    -- bind xpub "ipc:///tmp/xpub"

--     liftIO $ putStrLn ">>"
    -- normal people use proxy socketA, socketB
--     forever $ do 
--         msg <- receive xsub
--         liftIO $ putStrLn "<<"
--         liftIO $ putStrLn $ BS.unpack $ "xsub rcv msg: " <> msg
        -- send xpub [] msg
    -- forever $ do
    --     msg <- receive xpub
    --     liftIO $ putStrLn $ BS.unpack $ "xpub rcv msg: " <> msg
    --     send xsub [] msg
    

    -- forM_ [1..10] $ \update_nbr ->
    --     do
    --         let string = "Update " ++ show update_nbr
    --         send publisher [] (pack string)
    --         liftIO $ threadDelay $ 1 * 1000 * 1000
    -- send publisher [] "END"

{-
import zmq
from threading import Thread
from time import sleep
from random import randint

# Setup sockets for the proxy XSUB/XPUB and bind them to separate ports
ctx = zmq.Context()
xsub_sock = ctx.socket(zmq.XSUB)
xpub_sock = ctx.socket(zmq.XPUB)
xsub_sock.bind('tcp://127.0.0.1:1234')
xpub_sock.bind('tcp://*:1235')


def xsub_recv():
    # Proxy xsub where publishers connect
    while True:
        pub_msg = xsub_sock.recv()
        print('xsub_recv : ', pub_msg)
        xpub_sock.send(pub_msg)


def xpub_recv():
    # Proxy xpub where subscribers connect, and messages are sent by xsub
    while True:
        sub_msg = xpub_sock.recv()
        xsub_sock.send(sub_msg)
        print('xpub_recv : ', sub_msg)


def subscriber_loop(sub_sock, id):
    # The receive loop for the subscribers
    while True:
        msg = sub_sock.recv_string()
        print("SUB", id, ":", msg)


def subscriber_setup():
    # Creates 5 subscribers, each filtering for a specific topic
    for i in range(0, 5):
        print(i)
        sub_sock = ctx.socket(zmq.SUB)
        sub_sock.connect('tcp://127.0.0.1:1235')
        sub_sock.setsockopt_string(zmq.SUBSCRIBE, 'TOPIC_{i}'.format(i=i))
        Thread(target=subscriber_loop, args=(sub_sock, i)).start()


def publisher_loop():
    # This thread publishes a message to a random topic every second
    pub_sock = ctx.socket(zmq.PUB)
    pub_sock.connect('tcp://127.0.0.1:1234')
    while True:
        sleep(1)
        pub_sock.send_string("TOPIC_{id} Hello World!".format(id=randint(0, 5)))


# Spin up all the threads
Thread(target=xsub_recv).start()
Thread(target=xpub_recv).start()
subscriber_setup()
Thread(target=publisher_loop).start()

# Here we setup a subscriber that receives messages of any topic
sub_sock = ctx.socket(zmq.SUB)
sub_sock.connect('tcp://127.0.0.1:1235')
sub_sock.setsockopt_string(zmq.SUBSCRIBE, '')
Thread(target=subscriber_loop, args=(sub_sock, 'ALL')).start()

-}