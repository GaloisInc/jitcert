module FIPS.Property where

-- import qualified Data.Aeson as Aeson
-- import           Data.Aeson ((.:), (.=), ToJSON)
-- import           Data.Text.Lazy (Text)

import           JitCert.GSN.Types

data NoSideChannels = NoSideChannels

-- instance ToJSON NoSideChannels where
--     toJSON NoSideChannels = Aeson.object [
--           "type" .= ("NoSideChannels" :: Text)
--         ]

-- instance FromJSON NoSideChannels where
--     parseJSON (Aeson.Object o) = do
--         t <- o .: "type"
--         if t == ("NoSideChannels" :: Text) then
--             return NoSideChannels
--         else
--             fail "invalid type"
--     parseJSON _ = fail "invalid json"

instance Property NoSideChannels where
    renderProperty NoSideChannels = [SFText "is free from side channels"]

data NoIOExceptRNG = NoIOExceptRNG

-- instance ToJSON NoIOExceptRNG where
--     toJSON NoIOExceptRNG = Aeson.object [
--           "type" .= ("NoIOExceptRNG" :: Text)
--         ]

-- instance FromJSON NoIOExceptRNG where
--     parseJSON (Aeson.Object o) = do
--         t <- o .: "type"
--         if t == ("NoIOExceptRNG" :: Text) then
--             return NoIOExceptRNG
--         else
--             fail "invalid type"
--     parseJSON _ = fail "invalid json"

instance Property NoIOExceptRNG where
    renderProperty NoIOExceptRNG =
        [ SFText
              "performs no I/O operations except to seed random number generators"
        ]

data NoSharedMemoryOrState c = NoSharedMemoryOrState (Node c Context)

-- instance ToJSON (NoSharedMemoryOrState c) where
--     toJSON (NoSharedMemoryOrState n) = Aeson.object [
--           "type" .= ("NoSharedMemoryOrState" :: Text)
--         , "boundary" .= n
--         ]

-- instance FromJSON (NoSharedMemoryOrState c) where
--     parseJSON (Aeson.Object o) = do
--         t <- o .: "type"
--         n <- o .: "boundary"
--         if t == ("NoSharedMemoryOrState" :: Text) then
--             return $ NoSharedMemoryOrState n
--         else
--             fail "invalid type"
--     parseJSON _ = fail "invalid json"


instance RenderContext c => Property (NoSharedMemoryOrState c) where
    renderProperty (NoSharedMemoryOrState c) =
        [ SFText "does not share any memory or state with threads outside"
        , SFContext c
        ]


data RunOnBoundary c = RunOnBoundary (Node c Context)

-- instance ToJSON (RunOnBoundary c) where
--     toJSON (RunOnBoundary n) = Aeson.object [
--           "type" .= ("RunOnBoundary" :: Text)
--         , "boundary" .= n
--         ]

-- instance FromJSON (RunOnBoundary c) where
--     parseJSON (Aeson.Object o) = do
--         t <- o .: "type"
--         n <- o .: "boundary"
--         if t == ("RunOnBoundary" :: Text) then
--             return $ RunOnBoundary n
--         else
--             fail "invalid type"
--     parseJSON _ = fail "invalid json"


instance RenderContext c => Property (RunOnBoundary c) where
    renderProperty (RunOnBoundary c) = [SFText "is run on", SFContext c]

data NoMemoryLeaks = NoMemoryLeaks

instance Property NoMemoryLeaks where
    renderProperty NoMemoryLeaks = [SFText "does not have memory leaks"]

-- instance ToJSON NoMemoryLeaks where
--     toJSON NoMemoryLeaks = Aeson.object [
--           "type" .= ("NoMemoryLeaks" :: Text)
--         ]

-- instance FromJSON NoMemoryLeaks where
--     parseJSON (Aeson.Object o) = do
--         t <- o .: "type"
--         if t == ("NoMemoryLeaks" :: Text) then
--             return NoMemoryLeaks
--         else
--             fail "invalid type"
--     parseJSON _ = fail "invalid json"

