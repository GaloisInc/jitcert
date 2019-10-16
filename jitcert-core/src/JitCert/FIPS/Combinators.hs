{-|

Description: Patterns of FIPS-based arguments

This module contains a handful of combinators representing patterns of
argumentation specific to the FIPS standard.  In a finished product, these
"packages" of combinators could be packaged for any desired standards or
specifications.

-}

module JitCert.FIPS.Combinators where

import qualified Data.Text.Lazy as T

import           JitCert.Context
import           JitCert.DocGenerator
import           JitCert.GSN.Builder
import           JitCert.GSN.Combinators
import           JitCert.GSN.Types
import           JitCert.Solution

-- | A FIPS structural requirement stating that any module nested inside the
-- physical boundary must have a caveat in the certificate.
physBoundCaveat
  :: Monad m
  => Ctx Documentation  -- ^ The context node for the documentation
  -> Ctx Certificate    -- ^ The context node for the certificate
  -> BoundCtx           -- ^ The context node for the physical boundary
  -> BuilderT m (Node c Goal)
physBoundCaveat doc cert physb = do
  let docT = "the documentation"
      certT = "the certificate"
      physbT = "the physical boundary"

  -- Top level goal, for any module nested in ...
  g <- goalAndWithOptions
         [ SFText "Any module nested in"
         , SFContext physb
         , SFText "have certificate caveats"
         ]
         $ mkNodeOptions (dgCaveatIn "module" "the physical boundary" certT) "tmp ref"

  -- Argument over ...
  s <- strategyWithOptions
         PolicyOr
         [ SFText "Argument over all nested modules" ]
         $ mkNodeOptions (dgArgOver ["Any module nested in " <> physbT]) "tmp ref"
  addEdge g s

  -- Branch for no nested modules
  gNoBranch <- goalAndWithOptions
    [SFProperty ContainsNoEmbeddedModules]
    $ mkNodeOptions (dgNoEmbedded "A module" physbT) "tmp ref"
  snNoBranch <- solutionWithOptions
    (ManualInspection doc $ States ContainsNoEmbeddedModules)
    $ mkNodeOptions (dgVerifyCheck docT) "tmp ref"
  _ <- goalSolution gNoBranch snNoBranch

  -- Branch for yes nested modules
  gYesEmb <- goalAndWithOptions
    [SFText "A module is embedded in", SFContext physb]
    $ mkNodeOptions (dgYesEmbedded "A module" physbT) "tmp ref"
  snYesEmb <- solutionWithOptions
    (ManualInspection doc $ States (ModuleEmbeddedIn physb))
    $ mkNodeOptions (dgVerifyCheck docT) "tmp ref"
  addEdge gYesEmb snYesEmb
  gYesCav <- goalAndWithOptions
    [SFText "Embedded module has a caveat in", SFContext cert]
    $ mkNodeOptions (dgContains certT "an appropriate caveat") "tmp ref"
  snYesCav <- solutionWithOptions
    (ManualInspection cert $ States (EmbeddedModuleCaveat physb))
    $ mkNodeOptions (dgVerifyCheck certT) "tmp ref"
  addEdge gYesCav snYesCav

  gYesBranch <- andBranches [gYesEmb, gYesCav]
  or <- orBranches [gYesBranch, gNoBranch]
  addEdge s or

  return g

-- | A FIPS argument around boundary containment.
boundaryContainment
  :: Monad m
  => Ctx Documentation   -- ^ The context node for the documentation
  -> T.Text              -- ^ The textual name for the documentation
  -> BoundCtx            -- ^ The context node for the inner boundary
  -> T.Text              -- ^ The textual name for the inner boundary
  -> BoundCtx            -- ^ The context node for the outer boundary
  -> T.Text              -- ^ The textual name for the outer boundary
  -> BuilderT m (Node c Goal)
boundaryContainment doc docname inner inname outer outname = do
  let ci = inner `ContainedIn` outer
  g <- goalAndWithOptions [SFProperty ci] $ mkNodeOptions (dgInBound inname outname) "tmp ref"
  sn <- solutionWithOptions (ManualInspection doc $ States ci) $ mkNodeOptions (dgVerifyCheck docname) "tmp ref"
  addEdge g sn
  return g

boundaryAllElements
  :: Monad m
  => Ctx Documentation
  -> T.Text
  -> BoundCtx
  -> T.Text
  -> Ctx SoftwareModule
  -> T.Text
  -> BuilderT m (Node c Goal)
boundaryAllElements _ _ bound boundt mod modt = do
  g <- goalAndWithOptions
         [SFContext bound, SFText "contains all components of", SFContext mod]
         $ mkNodeOptions (dgYesEmbeddedMany ["The components of " <> modt] boundt) "tmp ref"

  -- solution
  return g




--------------------------------------------------------------------------------
-- Types

type BoundCtx = Ctx Boundary

type Ctx c = Node c Context

data Boundary
  = CryptographicBoundary
  | CryptographicAlgorithmBoundary
  | PhysicalBoundary
  | LogicalBoundary
  | ModuleBoundary
  | KernelBoundary
  deriving (Eq, Show)

instance RenderContext Boundary where
  renderContextTypeReference _ = "boundary"
  renderContext CryptographicBoundary          = "cryptographic"
  renderContext CryptographicAlgorithmBoundary = "cryptographic algorithm"
  renderContext PhysicalBoundary               = "physical"
  renderContext LogicalBoundary                = "logical"
  renderContext ModuleBoundary                 = "module"
  renderContext KernelBoundary                 = "kernel"


data SoftwareModule = SoftwareModule [SoftwareComponent]
  deriving (Eq, Show)

instance RenderContext SoftwareModule where
  renderContextTypeReference _ = "software module"
  renderContext (SoftwareModule cs) = T.pack $ show cs

data SoftwareComponent
  = SWFile String
  | Interface String
  | Service String
  | Port String
  | Nest SoftwareModule
  deriving (Eq, Show)

data TestSuite

instance RenderContext TestSuite where
  renderContextTypeReference _ = "Test suite"
  renderContext _ = ""

data Certificate

instance RenderContext Certificate where
  renderContextTypeReference _ = "Certificate"
  renderContext _ = ""

-- Properties
data UsedToImplement lang code = UsedToImplement (Node lang Context) (Node code Context)
instance (RenderContext lang, RenderContext code) => Property (UsedToImplement lang code) where
  renderProperty (UsedToImplement lang code) =
    [SFContext lang, SFText "is used to implement", SFContext code]

data AuthoredBy mod auths = AuthoredBy (Node mod Context) (Node auths Context)
instance (RenderContext mod, RenderContext auths) => Property (AuthoredBy mod auths) where
  renderProperty (AuthoredBy mod auths) =
    [SFContext mod, SFText "is authored by", SFContext auths]

data IsVersion mod ver = IsVersion (Node mod Context) (Node ver Context)
instance (RenderContext mod, RenderContext ver) => Property (IsVersion mod ver) where
  renderProperty (IsVersion mod ver) =
    [SFContext mod, SFText "has version number", SFContext ver]

data ContainsNoEmbeddedModules = ContainsNoEmbeddedModules
instance Property ContainsNoEmbeddedModules where
  renderProperty ContainsNoEmbeddedModules =
    [SFText "contains no embedded modules"]

data ContainedIn b c = ContainedIn (Node b Context) (Node c Context)
instance (RenderContext b, RenderContext c) => Property (ContainedIn b c) where
  renderProperty (ContainedIn b c) =
    [SFContext b, SFText "contained in", SFContext c]

data ArePartOf b = ArePartOf (Node b Context)
instance (RenderContext b) => Property (ArePartOf b) where
  renderProperty (ArePartOf b) = [SFText "are part of", SFContext b]

data AreIn b = AreIn (Node b Context)
instance (RenderContext b) => Property (AreIn b) where
  renderProperty (AreIn b) = [SFText "are in", SFContext b]

data IsOutside b c = IsOutside (Node b Context) (Node c Context)
instance (RenderContext b, RenderContext c) => Property (IsOutside b c) where
  renderProperty (IsOutside b c) =
    [SFContext b, SFText "is outside", SFContext c]

data IsInside b c = IsInside (Node b Context) (Node c Context)
instance (RenderContext b, RenderContext c) => Property (IsInside b c) where
  renderProperty (IsInside b c) =
    [SFContext b, SFText "is inside", SFContext c]

data States p = States p
instance Property p => Property (States p) where
  renderProperty (States p) = [SFText "states", SFProperty p]

data IsNestedIn c = IsNestedIn (Node c Context)
instance RenderContext c => Property (IsNestedIn c) where
  renderProperty (IsNestedIn c) = [SFText "is nested in", SFContext c]

data TestPortsIn c = TestPortsIn (Node c Context)
instance RenderContext c => Property (TestPortsIn c) where
  renderProperty (TestPortsIn c) = [SFText "test the ports in", SFContext c]

data TestServicesIn c = TestServicesIn (Node c Context)
instance RenderContext c => Property (TestServicesIn c) where
  renderProperty (TestServicesIn c) =
    [SFText "test the services in", SFContext c]

data TestInterfacesIn c = TestInterfacesIn (Node c Context)
instance RenderContext c => Property (TestInterfacesIn c) where
  renderProperty (TestInterfacesIn c) =
    [SFText "test the interfaces in", SFContext c]

data ModuleEmbeddedIn c = ModuleEmbeddedIn (Node c Context)
instance RenderContext c => Property (ModuleEmbeddedIn c) where
  renderProperty (ModuleEmbeddedIn c) =
    [SFText "a module is embedded in", SFContext c]

data EmbeddedModuleCaveat c = EmbeddedModuleCaveat (Node c Context)
instance RenderContext c => Property (EmbeddedModuleCaveat c) where
  renderProperty (EmbeddedModuleCaveat c) =
    [SFText "Embedded module has a caveat in", SFContext c]

-- Should really include descriptions and dependencies in solution nodes.
data ContainsThis c = ContainsThis (Node c Context)
instance RenderContext c => Property (ContainsThis c) where
  renderProperty (ContainsThis c) = [SFText "contains this", SFContext c]

data DoesNotContain c = DoesNotContain (Node c Context)
instance RenderContext c => Property (DoesNotContain c) where
  renderProperty (DoesNotContain c) = [SFText "does not contain ", SFContext c]

data RefersTo p = RefersTo p
instance Property p => Property (RefersTo p) where
  renderProperty (RefersTo p) = [SFText "refers to", SFProperty p]

data DoesNotReferTo p = DoesNotReferTo p
instance Property p => Property (DoesNotReferTo p) where
  renderProperty (DoesNotReferTo p) = [SFText "does not refer to", SFProperty p]

data DifferFrom b c = DifferFrom [Node b Context] (Node c Context)
instance (RenderContext b, RenderContext c) => Property (DifferFrom b c) where
  renderProperty (DifferFrom b c) =
    [SFText "One or more of"] <> map SFContext b <> [SFText "differs from", SFContext c]

data DiffersFrom b c = DiffersFrom (Node b Context) (Node c Context)
instance (RenderContext b, RenderContext c) => Property (DiffersFrom b c) where
  renderProperty (DiffersFrom b c) =
    [SFContext b, SFText "differs from", SFContext c]
