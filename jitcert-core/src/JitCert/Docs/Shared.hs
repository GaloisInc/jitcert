-- | Module for shared types, combinators, etc for documentation GSNs.

module JitCert.Docs.Shared where

import           Data.Proxy
import           Data.Text.Lazy                 ( Text )

import           JitCert.Context
import           JitCert.GSN.Types

data Function = Function {
    functionName :: Text
  , functionContent :: Text
  }
  deriving (Eq, Ord)

instance RenderContext Function where
    renderContextTypeReference Proxy = "function"
    renderContext (Function name _) = name

data SoftwareProgram = SoftwareProgram {
      softwareName :: Text
    , softwareBuildSettings :: Settings
    , softwareFiles :: Files
    }
    deriving Eq

instance RenderContext SoftwareProgram where
    renderContextTypeReference Proxy = "program"

    renderContext (SoftwareProgram name _ _) = name

data Settings = Settings
    deriving Eq

data RNG = RNG Text
    deriving Eq

instance RenderContext RNG where
    renderContextTypeReference Proxy = "rng"
    renderContext (RNG t) = t

data ProducesUniformDistribution = ProducesUniformDistribution
instance Property ProducesUniformDistribution where
    renderProperty ProducesUniformDistribution =
        [SFText "produces a uniform distribution"]

