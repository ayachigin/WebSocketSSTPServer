{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forever)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Network.WebSockets as WS
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

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
  let path = decodeUtf8 . WS.requestPath . WS.pendingRequest$ pending
  print path
  sendSSTP path con

sendSSTP :: Text -> WS.Connection -> IO ()
sendSSTP path conn = forever $ do
  wsmsg <- fmap T.unpack . WS.receiveData $ conn
  let msg = case path of
             "/notify" -> notifyRequestString wsmsg
             _         -> wsmsg
  response <- simpleSSTP msg
  putStrLn msg
  putStrLn response
  hFlush stdout
  WS.sendTextData conn . T.pack $ response ++ "\r\n" ++ msg

notifyRequestString :: String -> String
notifyRequestString = buildNotifyRequest . T.pack

buildNotifyRequest :: Text -> String
buildNotifyRequest s = fromRequest request
  where
    args = map T.unpack . T.splitOn "," $ s
    event      = if argLen >= 1 then args!!0 else _event emptyRequest
    sender     = if argLen >= 2 then args!!1 else _sender emptyRequest
    script     = if argLen >= 3 then args!!2 else _script emptyRequest
    references = if argLen >= 4 then drop 3 args else _references emptyRequest
    request = emptyRequest { _event=event
                           , _sender=sender
                           , _script=script
                           , _references=references }
    argLen = length args
