{-# LANGUAGE OverloadedStrings #-}

module FIPS.Context where

import           Data.Monoid                    ( (<>) )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text.Lazy                 ( Text )
import           JitCert.Context
import           JitCert.GSN

-- Data type for FIPS contexts.
-- data FContext = 
--       CryptographicAlgorithm
--     | List FContext
--     | Approved FContext 
--     -- Approved is an adjective. Should be able to use like: X is approved. OR ... approved X ...
--     | CryptographicBoundaryComponent
-- 
--     deriving (Show)

-- instance RenderContext FContext where
--     renderLabel CryptographicAlgorithm = "cryptographic algorithm"
--     renderLabel (List a) = "list of " <> renderLabel a
--     renderLabel (Approved a) = "FIPS approved " <> renderLabel a
--     renderLabel CryptographicBoundaryComponent = "component of the cryptographic module (hardware, software, or firmware that implements approved security functions)"

data CryptographicAlgorithm =
      CASymmetricEncAlgorithm SymmetricEncAlgorithm
    | CAAsymmetricEncAlgorithm AsymmetricEncAlgorithm
    | CASymmetricAuthAlgorithm SymmetricAuthAlgorithm
    | CAAsymmetricAuthAlgorithm AsymmetricAuthAlgorithm
    | CAHashAlgorithm HashAlgorithm
    deriving (Eq, Ord)
    -- RNG, ...

-- TODO: Implement these. 
data SymmetricEncAlgorithm = SymmetricEncAlgorithm
    deriving (Eq, Ord)
    -- AES
data AsymmetricEncAlgorithm = AsymmetricEncAlgorithm
    deriving (Eq, Ord)
data SymmetricAuthAlgorithm = SymmetricAuthAlgorithm
    deriving (Eq, Ord)
    -- HMAC
data AsymmetricAuthAlgorithm = AsymmetricAuthAlgorithm
    deriving (Eq, Ord)
    -- RSA
data HashAlgorithm = HashAlgorithm
    deriving (Eq, Ord)

instance RenderContext CryptographicAlgorithm where
    renderContextTypeReference Proxy = "cryptographic algorithm"

    renderContext _ = "cryptographic algorithm" -- TODO

data LogicalBoundary = LogicalBoundary
    deriving (Eq, Ord)

instance RenderContext LogicalBoundary where
    renderContextTypeReference Proxy = "logical boundary"

    renderContext LogicalBoundary = "logical boundary" -- TODO

isSoftwareFirmware :: Component -> Bool
isSoftwareFirmware Hardware         = False
isSoftwareFirmware (Software _ _ _) = True
isSoftwareFirmware Firmware         = True

-- data CryptographicAlgorithm -- = 
-- 
data Component =
      Hardware
    | Software {
        softwareName :: Text
      , softwareVersion :: Text
      , softwareFiles :: Files
      }
    | Firmware
    deriving (Eq, Ord)

instance RenderContext Component where

    renderContextTypeReference Proxy = "component"

    renderContext Hardware          = "hardware"
    renderContext Firmware          = "firmware"
    renderContext (Software ns v _) = ns <> " " <> v
        -- renderContextReference fs

-- 
-- instance RenderContext Component where
--     renderType Proxy = "component"
--     renderTypeShort = renderType
-- 
--     renderLabel Hardware = "hardware"
--     renderLabel Software = "software"
--     renderLabel Firmware = "firmware"

-- 
-- data FIPS f =
--       FIPSComponent f -- (Maybe Component)
--     | 
-- 
-- hmac :: BitSize -> MAC
-- 
-- mac :: Algorithm
-- 
