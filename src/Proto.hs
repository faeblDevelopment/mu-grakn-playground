{-# language CPP                   #-}
{-# language DataKinds             #-}
{-# language DeriveAnyClass        #-}
{-# language DeriveGeneric         #-}
{-# language DerivingStrategies    #-}
{-# language DerivingVia           #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language TemplateHaskell       #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
module Proto where

import GHC.Generics
import GHC.Int
import Data.Void
import           Mu.Quasi.GRpc
import Mu.Quasi.ProtoBuf
import Mu.Schema
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Mu.Adapter.ProtoBuf

--grpc "GraknOptionsSchema" (++ "Service") "protobuf/options.proto"
grpc "GraknSessionSchema" (++ "Service") "protobuf/session.proto"
grpc "GraknSchema" (++ "Service") "protobuf/grakn.proto"

instance Generic Int32 where


data Options =
     Options { inferOpt :: InferOpt
             , explainOpt :: ExplainOpt
             , batchSizeOpt :: BatchSizeOpt }
     deriving ( Generic
              , ToSchema   GraknSessionSchema "Options"
              , FromSchema GraknSessionSchema "Options" )

data InferOpt = InferOpt Bool deriving (Generic)
data ExplainOpt = ExplainOpt Bool deriving (Generic)
data BatchSizeOpt = BatchSizeOpt Int32 deriving (Generic)

data GType = DATA | SCHEMA
     deriving ( Generic
              , ToSchema   GraknSessionSchema "Session.Type"
              , FromSchema GraknSessionSchema "Session.Type" )

    
type TypeFieldMapping = '[ "type_" ':-> "type" ]

data SessionOpenReq =
     SessionOpenReq { database  :: T.Text
                    , type_     :: GType 
                    , options   :: Options }
     deriving ( Generic )
     deriving ( ToSchema   GraknSessionSchema "Session.Open.Req" 
              , FromSchema GraknSessionSchema "Session.Open.Req" )
        via (CustomFieldMapping "Session.Open.Req" TypeFieldMapping SessionOpenReq)


data SessionOpenRes =
     SessionOpenRes { sessionID :: BS.ByteString }
     deriving ( Generic
              , ToSchema   GraknSessionSchema "Session.Open.Res"
              , FromSchema GraknSessionSchema "Session.Open.Res" )
