

{-# LANGUAGE OverloadedStrings #-}

-- Hello World server

module Main where

import Control.Concurrent
import Control.Monad
import System.ZMQ4.Monadic
-- import Control.Monad.Trans.State.Lazy

respond :: Socket z Rep -> StateT Int (ZMQ z) ()
respond responder = do
        i <- get
        put (i + 1)
        lift $ send responder [] (show i)

main :: IO ()
main = runZMQ $ flip evalStateT 0 $ do
    -- Socket to talk to clients
    responder <- lift $ socket Rep
    lift $ bind responder "tcp://*:5555"

    forever $ do
        buffer <- lift $ receive responder
        respond responder
