{-|

Description: A small example of using the assurance case DSL

This module makes use of the JITCert assurance case DSL in order to create some
structured arguments around the OpenSSL library and the FIPS 140 standard for
cryptographic software.  Moreso than a proper assurance case, it is intended as
a proof of concept to test features of the DSL.

-}

{-# LANGUAGE RankNTypes #-}
module JitCert.Examples.OpenSSL where

import           Data.Functor.Identity
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as T
import           Data.Time.Calendar ( Day(..) )
import           Data.Time.Clock ( UTCTime(..) )

import           JitCert.Context
import           JitCert.DocGenerator
import           JitCert.DotBackend
import           JitCert.Evidence
import           JitCert.FIPS.Combinators
import           JitCert.GSN.Builder
import           JitCert.GSN.Builder.Internal ( runBuilderT )
import           JitCert.GSN.Combinators
import           JitCert.GSN.Types
import           JitCert.Properties
import           JitCert.Solution

(~>)
  :: (Monad m, ValidEdge t1 t2)
  => Node c t1
  -> Node d t2
  -> BuilderT m (Node c t1)
(~>) source sink = addEdge source sink >> return source

modifyEnv :: GSNEnv -> GSNEnv
modifyEnv (GSNEnv cs sns) = GSNEnv cs' sns
  where cs' = M.insert 20 (GSNEnvContext (SomeContext openSSLCanisterSM2)) cs

-- The OpenSSL based example
runOpenSSL :: IO ()
runOpenSSL = do
  (g, _) <- runBuilderT openSSL
  render "openssl.dot" g

runOpenSSLEnvs :: (GSNGraph, [GSNEnv])
runOpenSSLEnvs =
  let (g, env1) = runIdentity $ runBuilderT openSSL
  in (g, [env1, modifyEnv env1])

openSSL :: Monad m => BuilderT m ()
openSSL = do
  g <- goalAndWithOptions [SFText "OpenSSL FIPS Certification"] 
                            $ mkNodeOptions dgEmpty "tmp ref"
  s <- boundariesS
  addEdge g s

-- A standalone snippet for zooming in on the certificate caveat portion of the
-- larger example.
openSSLCertCaveat :: Monad m => BuilderT m ()
openSSLCertCaveat = do
  doc <- context @Documentation "d" Nothing
  cert <- certificateC
  physb <- physicalBoundaryC
  g <- physBoundCaveat doc cert physb
  addEdge g doc
  addEdge g cert
  addEdge g physb

-- Define some values for the environment
openSSLCanisterSM :: SoftwareModule
openSSLCanisterSM =
  SoftwareModule [SWFile "fips_rand.c", Interface "canister interface"]

openSSLCanisterSM2 :: SoftwareModule
openSSLCanisterSM2 =
  SoftwareModule [SWFile "fips_rand_v2.c", Interface "canister interface"]

openSSLWrapSM :: SoftwareModule
openSSLWrapSM = SoftwareModule [Nest openSSLCanisterSM]

openSSLWrapSM2 :: SoftwareModule
openSSLWrapSM2 = SoftwareModule [Nest openSSLCanisterSM2]

-- Everything related to boundary definitions
boundariesS :: (Monad m) => BuilderT m (Node Boundary Goal)
boundariesS = do
  doc                        <- context @Documentation "d" Nothing
  bounds@[c, _, p, l, m, k] <- boundariesC
  g <- setOfDefinitions bounds

  addEdges g bounds
  addEdge g doc

  g1 <- boundaryContainment doc theDocsT l lbt p pbt
  addEdge g g1

  app <- appC
  addEdge g app
  g2 <- appInPhysG app p
  addEdge g g2

  wrap <- wrapperAppC
  addEdge g wrap
  g3 <- logIsCanisterTop app l
  addEdge g g3

  g4 <- boundaryContainment doc theDocsT l lbt k kbt
  addEdge g g4

  g5 <- canInWrap app wrap
  addEdge g g5

  g6 <- moduleInLog doc app l
  addEdge g g6

  g7 <- appOutKernel doc app k
  addEdge g g7

  g8 <- kernelInPhys doc k p
  addEdge g g8

  g9 <- notSamePhysG doc bounds p
  addEdge g g9

  tests <- testSuiteC
  addEdge g tests
  g10 <- testingModBoundG m app tests
  addEdge g g10

  g11 <- cryptoBoundaryDefG doc c m app tests
  addEdge g g11

  cert <- certificateC
  addEdge g cert

  caveat <- physBoundCaveat doc cert p
  addEdge g caveat

  return g

cryptoBoundaryDefG
  :: Monad m
  => Node Documentation Context
  -> BoundCtx
  -> BoundCtx
  -> Node SoftwareModule Context
  -> Node TestSuite Context
  -> BuilderT m (Node c Goal)
cryptoBoundaryDefG doc cb mb mod ts = do
  g <- goalAndWithOptions [SFContext cb, SFText "is well defined"]
                            $ mkNodeOptions dgEmpty "tmp ref"

  cont <- cryptoContinuousG doc cb mod
  addEdge g cont

  name <- modNameConsistentG doc cb mb mod ts
  addEdge g name

  exp <- explicitDefG doc cb
  addEdge g exp

  all <- containsAllG doc cb mod
  addEdge g all

  return g

containsAllG
  :: Monad m
  => Node Documentation Context
  -> BoundCtx
  -> Node SoftwareModule Context
  -> BuilderT m (Node c Goal)
containsAllG doc cb app = do
  g <- goalAndWithOptions
    [SFContext cb, SFText "contains all components of", SFContext app]
    $ mkNodeOptions dgEmpty "tmp ref"
  sn <- solutionWithOptions
          (ManualInspection doc $ IsInside app cb)
          $ setNodeOptionsGenerator def dgEmpty
  g ~> sn

explicitDefG
  :: Monad m
  => Node Documentation Context
  -> BoundCtx
  -> BuilderT m (Node c Goal)
explicitDefG doc cb = do
  g <- goalAndWithOptions
         [SFContext cb, SFText "is explicitly defined"]
         $ mkNodeOptions dgEmpty "tmp ref"
  sn <- solution
          (ManualInspection doc $ ContainsThis cb)
  g ~> sn

-- Also needs the certificate context, etc.
modNameConsistentG
  :: Monad m
  => Node Documentation Context
  -> BoundCtx
  -> BoundCtx
  -> Node SoftwareModule Context
  -> Node TestSuite Context
  -> BuilderT m (Node c Goal)
modNameConsistentG doc cb mb sw ts = do
  g <- goalAndWithOptions [SFContext sw, SFText "naming consistent"] $ mkNodeOptions dgEmpty "tmp ref"
  s <- strategyWithOptions
    PolicyOr
    [SFContext sw, SFText "name must match report on", SFContext ts]
    $ setNodeOptionsGenerator def dgEmpty
  addEdge g s

  gNotOut <- goalAndWithOptions
    [ SFContext sw
    , SFText "name doesn't refer to components outside"
    , SFContext mb
    ]
    $ mkNodeOptions dgEmpty "tmp ref"
  addEdge s gNotOut
  gYesOut <- goalAndWithOptions
    [SFContext cb, SFText "consistent with name scope"]
    $ mkNodeOptions dgEmpty "tmp ref"
  addEdge s gYesOut

  snNo <- solution (ManualInspection doc $ DoesNotReferTo (IsOutside sw mb))
  addEdge gNotOut snNo
  snYes <- solution (ManualInspection doc $ RefersTo (IsOutside sw mb))
  addEdge gYesOut snYes

  return g


cryptoContinuousG
  :: Monad m
  => Node Documentation Context
  -> BoundCtx
  -> Node SoftwareModule Context
  -> BuilderT m (Node c Goal)
cryptoContinuousG doc cb mod = do
  g <- goalAndWithOptions
    [SFContext cb, SFText "has a continuous perimeter"]
    $ mkNodeOptions dgEmpty "tmp ref"
  s <- strategyWithOptions
    PolicyOr
    [ SFText "Every component in"
    , SFContext mod
    , SFText "is either inside or outside"
    , SFContext cb
    ]
    $ setNodeOptionsGenerator def dgEmpty
  addEdge g s
  gin  <- goalAndWithOptions [SFText "Component is inside", SFContext cb]
    $ setNodeOptionsReference def "tmp ref"
  gout <- goalAndWithOptions [SFText "Component is outnside", SFContext cb]
    $ setNodeOptionsReference def "tmp ref"
  addEdge s gin
  addEdge s gout
  snin  <- solution (ManualInspection doc $ IsInside mod cb)
  snout <- solution (ManualInspection doc $ IsOutside mod cb)
  addEdge gin  snin
  addEdge gout snout
  return g

-- Type for test already exists?
testingModBoundG
  :: Monad m
  => BoundCtx
  -> Node SoftwareModule Context
  -> Node TestSuite Context
  -> BuilderT m (Node c Goal)
testingModBoundG bound mod tests = do
  g <- goalAndWithOptions
    [SFContext tests, SFText "test the module boundary", SFContext bound]
    $ setNodeOptionsReference def "tmp ref"
  s  <- testingModBoundS mod tests
  g' <- boundaryDefinedByG bound mod
  addEdge g s
  addEdge g g'
  return g


boundaryDefinedByG
  :: Monad m
  => BoundCtx
  -> Node SoftwareModule Context
  -> BuilderT m (Node c Goal)
boundaryDefinedByG _bound _cs = goalAndWithOptions
  [ SFText
      "Somehow extract components and show that all interfaces on boundary are accounted for"
  ]
  $ setNodeOptionsReference def "tmp ref"


testingModBoundS
  :: Monad m
  => Node SoftwareModule Context
  -> Node TestSuite Context
  -> BuilderT m (Node c Strategy)
testingModBoundS mod tests = do
  s <- strategyAnd
    [ SFContext tests
    , SFText "include tests for all ports, interfaces, and services in"
    , SFContext mod
    ]
  gint <- goalAndWithOptions
    [SFContext tests, SFText "test the interfaces in", SFContext mod]
    $ setNodeOptionsReference def "tmp ref"
  snint <- solution (ManualInspection tests $ TestInterfacesIn mod)
  addEdge gint snint
  addEdge s    gint
  gserv <- goalAndWithOptions
    [SFContext tests, SFText "test the services in", SFContext mod]
    $ setNodeOptionsReference def "tmp ref"
  snserv <- solution (ManualInspection tests $ TestServicesIn mod)
  addEdge gserv snserv
  addEdge s     gserv
  gport  <- goalAndWithOptions [SFContext tests, SFProperty (TestPortsIn mod)]
        $ setNodeOptionsReference def "tmp ref"
  snport <- solution (ManualInspection tests $ TestPortsIn mod)
  addEdge gport snport
  addEdge s     gport
  return s


notSamePhysG
  :: Monad m
  => Node Documentation Context
  -> [BoundCtx]
  -> BoundCtx
  -> BuilderT m (Node c Goal)
notSamePhysG doc bounds phys = do
  g <- goalAndWithOptions [SFText "Not all boundaries are the same as", SFContext phys]
        $ setNodeOptionsReference def "tmp ref"
  s <- notSamePhysS doc bounds phys
  g ~> s

notSamePhysS
  :: Monad m
  => Node Documentation Context
  -> [BoundCtx]
  -> BoundCtx
  -> BuilderT m (Node c Strategy)
notSamePhysS doc bounds phys = do
  -- quantification?
  s <- strategyAnd [SFText "Compare against each other boundary"]

  -- Should actually be a bunch of separate nodes, but I don't want to make them all
  g <- goalWithOptions
    PolicyOr
    (  SFText "One of"
    :  map SFContext bounds
    ++ [SFText "differs from", SFContext phys]
    )
    $ setNodeOptionsReference def "tmp ref"
  sn <- solution (ManualInspection doc $ States (DifferFrom bounds phys))
  addEdge s g
  addEdge g sn
  return s

kernelInPhys
  :: Monad m
  => Node Documentation Context
  -> BoundCtx
  -> BoundCtx
  -> BuilderT m (Node c Goal)
kernelInPhys doc kern phys = do
  g  <- goalAndWithOptions [SFContext kern, SFText "is inside", SFContext phys]
        $ setNodeOptionsReference def "tmp ref"
  sn <- solution (ManualInspection doc $ IsInside kern phys)
  g ~> sn

-- If we had this systematized (i.e., knowing that the module is the same as
-- the logical boundary, we could derive some interesting relationships and see
-- if we could find conflicts.
appOutKernel
  :: Monad m
  => Node Documentation Context
  -> Node SoftwareModule Context
  -> BoundCtx
  -> BuilderT m (Node c Goal)
appOutKernel doc mod kern = do
  g  <- goalAndWithOptions [SFContext mod, SFText "is outside", SFContext kern]
        $ setNodeOptionsReference def "tmp ref"
  sn <- solution (ManualInspection doc $ IsOutside mod kern)
  g ~> sn

moduleInLog
  :: Monad m
  => Node Documentation Context
  -> Node SoftwareModule Context
  -> BoundCtx
  -> BuilderT m (Node c Goal)
moduleInLog doc mod log = do
  g  <- goalAndWithOptions [SFContext mod, SFText "is inside", SFContext log]
        $ setNodeOptionsReference def "tmp ref"
  sn <- solution (ManualInspection doc $ IsInside mod log)
  g ~> sn

canInWrap
  :: Monad m
  => Node SoftwareModule Context
  -> Node SoftwareModule Context
  -> BuilderT m (Node c Goal)
canInWrap can wrap = do
  t <- context @Test "t" Nothing
  g <- goalAndWithOptions [SFContext can, SFText "is nested in", SFContext wrap]
        $ setNodeOptionsReference def "tmp ref"
  addEdge g t
  sn <- solution (Tests t can $ IsNestedIn wrap)
  g ~> sn

logInKernelG
  :: Monad m
  => Node Documentation Context
  -> BoundCtx
  -> BoundCtx
  -> BuilderT m (Node c Goal)
logInKernelG doc log kern = do
  let isIn = log `IsInside` kern
  g  <- goalAndWithOptions [SFProperty isIn]
        $ setNodeOptionsReference def "tmp ref"
  sn <- solution (ManualInspection doc $ States isIn)
  g ~> sn

logIsCanisterTop
  :: (Monad m)
  => Node SoftwareModule Context
  -- Want this to be (forall t. RenderContext t) I think
  -- -> [Ctx t]
  -> BoundCtx
  -> BuilderT m (Node c Goal)
logIsCanisterTop app log = do
  g <- goalAndWithOptions [SFContext app, SFText "is the same as", SFContext log]
        $ setNodeOptionsReference def "tmp ref"
  s <- logIsCanisterS app log
  g ~> s

logIsCanisterS
  :: (Monad m)
  => Node SoftwareModule Context
  -- Want this to be (forall t. RenderContext t) I think
  -- -> [Ctx t]
  -> BoundCtx
  -> BuilderT m (Node c Strategy)
logIsCanisterS app log = do
  s <- strategyAnd
    [ SFText "Contents of"
    , SFContext app
    , SFText "are exactly those inside"
    , SFContext log
    ]
  gin  <- logIsCanisterGIn app log
  gout <- logIsCanisterGOut app log
  addEdge s gin
  addEdge s gout
  return s

logIsCanisterGIn
  :: (Monad m)
  => Node SoftwareModule Context
  -- Want this to be (forall t. RenderContext t) I think
  -- -> [Ctx t]
  -> BoundCtx
  -> BuilderT m (Node c Goal)
logIsCanisterGIn app log = do
  g <- goalAndWithOptions
    [SFText "Components of", SFContext app, SFText "are in", SFContext log]
    $ setNodeOptionsReference def "tmp ref"
  sn <- solution (ManualInspection app (AreIn log))
  g ~> sn

logIsCanisterGOut
  :: (Monad m)
  => Node SoftwareModule Context
  -- Want this to be (forall t. RenderContext t) I think
  -- -> [Ctx t]
  -> BoundCtx
  -> BuilderT m (Node c Goal)
logIsCanisterGOut app log = do
  g <- goalAndWithOptions
    [SFText "Elements of", SFContext log, SFText "are part of", SFContext app]
    $ setNodeOptionsReference def "tmp ref"
  sn <- solution (ManualInspection log (ArePartOf app))
  g ~> sn

appInPhysG
  :: Monad m
  => Node SoftwareModule Context
  -> BoundCtx
  -> BuilderT m (Node c Goal)
appInPhysG app phys = do
  g  <- goalAndWithOptions [SFContext app, SFText "runs in", SFContext phys] $ setNodeOptionsReference def "tmp ref"
  sn <- solution (ManualInspection app IsDocumented)
  let t = UTCTime (ModifiedJulianDay 10) 0
  setSolutionEvidence sn $ Inspected "Joe" t True -- (Documented t "doc_file")
  g ~> sn

data Application = Application

--------------------------------------------------------------------------------
-- Context Nodes
cryptoBoundaryC :: Monad m => BuilderT m (Node Boundary Context)
cryptoBoundaryC = do
  c <- context
         @Boundary
         "cryptographic boundary"
         Nothing
  setContextValue c CryptographicBoundary
  return c

cryptoAlgBoundaryC :: Monad m => BuilderT m (Node Boundary Context)
cryptoAlgBoundaryC = do
  c <- context
         @Boundary
         "cryptographic algorithm boundary"
         Nothing
  setContextValue c CryptographicAlgorithmBoundary
  return c

physicalBoundaryC :: Monad m => BuilderT m (Node Boundary Context)
physicalBoundaryC = do
  c <- context
         @Boundary
         "Physical Boundary"
         Nothing
  setContextValue c PhysicalBoundary
  return c

logicalBoundaryC :: Monad m => BuilderT m (Node Boundary Context)
logicalBoundaryC = do
  c <- context
         @Boundary
         "Logical Boundary"
         Nothing
  setContextValue c LogicalBoundary
  return c

moduleBoundaryC :: Monad m => BuilderT m (Node Boundary Context)
moduleBoundaryC = do
  c <- context
         @Boundary
         "Module Boundary"
         Nothing
  setContextValue c ModuleBoundary
  return c

kernelBoundaryC :: Monad m => BuilderT m (Node Boundary Context)
kernelBoundaryC = do
  c <- context
         @Boundary
         "Kernel Boundary"
         Nothing
  setContextValue c KernelBoundary
  return c

boundariesC :: Monad m => BuilderT m [Node Boundary Context]
boundariesC = sequence
  [ cryptoBoundaryC
  , cryptoAlgBoundaryC
  , physicalBoundaryC
  , logicalBoundaryC
  , moduleBoundaryC
  , kernelBoundaryC
  ]

appC :: Monad m => BuilderT m (Node SoftwareModule Context)
appC = do
  c <- context @SoftwareModule "Canister Module" Nothing
  setContextValue c openSSLCanisterSM
  return c

wrapperAppC :: Monad m => BuilderT m (Node SoftwareModule Context)
wrapperAppC = do
  c <- context @SoftwareModule "Wrapper Module" Nothing
  setContextValue c openSSLWrapSM
  return c

wrapperAppC2 :: Monad m => BuilderT m (Node SoftwareModule Context)
wrapperAppC2 = do
  c <- context @SoftwareModule "Wrapper Module" Nothing
  setContextValue c openSSLWrapSM2
  return c

testSuiteC :: Monad m => BuilderT m (Node TestSuite Context)
testSuiteC = context @TestSuite "Operational Tests" Nothing

certificateC :: Monad m => BuilderT m (Node Certificate Context)
certificateC = context @Certificate "Certificate" Nothing


-------------------------------------------------------------------------------
-- DocGen strings
cbt, cabt, pbt, lbt, mbt, kbt :: T.Text
cbt = "The Cryptographic Boundary"
cabt = "The Cryptographic Algorithm Boundary"
pbt = "The Physical Boundary"
lbt = "The Logical Boundary"
mbt = "The Module Boundary"
kbt = "The Kernel Boundary"

theDocsT :: T.Text
theDocsT = "the documentation"

theCertT :: T.Text
theCertT = "the certificate"

swapp :: T.Text
swapp = "the software application"

swmod :: T.Text
swmod = "the software module"

opensslmodt :: T.Text
opensslmodt = "the OpenSSL module"

fipscertt :: T.Text
fipscertt = "FIPS certification"

boundaryNames :: [T.Text]
boundaryNames = [cbt, cabt, pbt, lbt, mbt, kbt]
