{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forever)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import System.Environment (getArgs)

import Network.SSTP (simpleSSTP, emptyRequest, Request(..), fromRequest)

main :: IO ()
main = do
  port <- getPort
  WS.runServer "127.0.0.1" port $ myApp
  where
    getPort = do
      args <- getArgs
      if null args then
        return 55432
      else
        return . read . head $ args

type Clients = [WS.Connection]

myApp :: WS.ServerApp
myApp pending = do
  con <- WS.acceptRequest pending
  notify con

sendText :: WS.Connection -> Text -> IO ()
sendText con t = WS.sendTextData con t

notify :: WS.Connection -> IO ()
notify conn = forever $ do
  msg <- WS.receiveData conn
  let reqstr = buildRequest msg
  r <- simpleSSTP reqstr
  WS.sendTextData conn $ T.pack r

buildRequest :: Text -> String
buildRequest msg = let params = map T.unpack . T.splitOn "," $ msg in
                    fromRequest . req $ params
  where
    req params
      | paramLen == 0 = emptyRequest
      | paramLen == 1 = emptyRequest { _event=head params }
      | paramLen == 2 = emptyRequest { _event=params!!0
                                     , _script=params!!1 }
      | otherwise     = emptyRequest { _event=params !! 0
                                     , _script=params !! 1
                                     , _reference=drop 2 params
                                     }
      where
        paramLen = length params
