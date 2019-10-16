module Types where

-- import           ClassyPrelude.Yesod hiding (Proxy)
-- import qualified Data.Aeson as Aeson
-- import           Data.Bifunctor
-- import qualified Data.ByteString.Lazy as BSL
-- import           Data.Proxy
-- import qualified Data.Text as T
-- import           Database.Persist.Sql (PersistValue(..), PersistFieldSql(..), PersistField(..), SqlType(..))
-- import           JitCert.GSN.Types
-- 
-- instance PersistFieldSql GSNGraph where
--     sqlType Proxy = SqlString
-- 
-- instance PersistField GSNGraph where
--     toPersistValue g = PersistByteString $ BSL.toStrict $ Aeson.encode g
-- 
--     fromPersistValue (PersistByteString t) = first T.pack $ Aeson.eitherDecodeStrict t
--     fromPersistValue _ = Left "Invalid database encoding"
-- 
