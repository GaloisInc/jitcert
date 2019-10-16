{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module JitCert.FIPS where

import qualified Data.Set                      as S
import qualified Data.Text.Lazy                as T


--------------------------------------------------------------------------------
-- Some experimental FIPS/argumentation stuff that relied on a different
-- abstraction of node labels and so doesn't work quite right

data Feature
  = SigAlg AllowedSignature
  | SymKeyEncrypt AllowedSymKeyEncrypt
  | HashAlg AllowedHash
  deriving (Eq, Ord, Show)

data AllowedSymKeyEncrypt = AES | TDEA
  deriving (Eq, Ord, Show)

data AllowedSignature = DSA | RSA | ECDSA
  deriving (Eq, Ord, Show)

data AllowedHash = SHS
  deriving (Eq, Ord, Show)

-- Hardware
-- Software
-- Firmware

data Component
  = Hardware
  | Software (S.Set FilePath)
  | Firmware
  deriving (Eq, Ord)

instance Show Component where
  show (Software s) = concatMap show (S.toList s)
  show Hardware     = "hardware"
  show Firmware     = "firmware"

data Reference = DocRef | GSNRef | CodeRef FilePath | ProofRef
  deriving (Eq, Ord, Show)

-- Or a combination thereof

type Components = S.Set Component

-- Implements

data Implements = Implements (S.Set (Component, Feature))

instance Show Implements where
  show (Implements s) = concatMap show (S.toList s)

instance Semigroup Implements where
  Implements s1 <> Implements s2 = Implements (S.union s1 s2)

modularImplementation :: Implements -> Implements -> Implements
modularImplementation = (<>)

-- defined cryptographic boundary
data Boundary = Boundary (FilePath -> Bool)

instance Show Boundary where
  show (Boundary _) = "boundary"

data FIPS
  = FIPSImpl Implements
  | FIPSText T.Text
  -- Make a "claim" a more general thing?  Or should this actully just be GSN?
  | FIPSBound Boundary FilePath

instance Show FIPS where
  show (FIPSImpl i) = show i
  show (FIPSText t) = T.unpack t
  show (FIPSBound (Boundary f) x) =
    "Boundary Check: " ++ if f x then "Pass" else "Fail"
