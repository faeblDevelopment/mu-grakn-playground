{-# language DataKinds             #-}
{-# language NamedFieldPuns        #-}
{-# language OverloadedStrings     #-}
{-# language TypeApplications      #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Server where

import           Control.Concurrent.STM
import qualified Control.Monad.Trans.Except
import           Control.Monad.IO.Class (liftIO)
import           Data.Int
import           Data.List              (find)
import qualified Data.ByteString as BS

import           Mu.GRpc.Server
import           Mu.Server

import           Proto 
import           Prelude                hiding (id)

main :: IO ()
main = do
  putStrLn "running grakn mock Server"
  runGRpcApp msgProtoBuf 8080 (server)


    
server :: ServerIO info GraknService _ 
  --                       ('[ '[ ServerErrorIO BS.ByteString]]) 
                         -- inferred part 
server = singleService ( method @"session_open" $ session_open)

{-
( method @"database_contains"    $ undefined --open 
                       , method @"database_create"   $ undefined
                       , method @"database_all" $ undefined
                       , method @"database_delete" $ undefined
                       , method @"session_open" $ session_open
                       , method @"session_close" $ undefined
                       , method @"transaction" $ undefined)
-}
session_open :: (MonadServer m) => SessionOpenReq -> m SessionOpenRes
session_open _ = pure $ SessionOpenRes ""

    {-
close :: [Byte] -> ServerErrorIO ()
close = alwaysOk $ do
  putStrLn "close" 
-}
