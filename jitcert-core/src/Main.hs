module Main where

import qualified Data.Text.Lazy.IO as T
import           Data.Text.Prettyprint.Doc
import           Type.Reflection

import           JitCert.Context
import           JitCert.Docs
import           JitCert.Examples.OpenSSL
import           JitCert.GSN ( docGenText )
import           JitCert.GSN.Builder
import           JitCert.GSN.Combinators
import           JitCert.GSN.Types
import           JitCert.Logic
import           JitCert.Properties
import           JitCert.Solution


main :: IO ()
main = do
  buildDocImages

  runOpenSSL

  (gsn, _) <- runBuilderT $ figure7 >> return ()
  print $ pretty $ gsnToLogic gsn

opensslDocs :: IO ()
opensslDocs = do
  (g, env) <- runBuilderT openSSL
  T.putStrLn "\n"
  T.putStrLn $ docGenText g env
  T.putStrLn "\n"

fipsrandc :: File
fipsrandc = File { fileName      = "fips_rand.c"
                 , fileDirectory = "fips-1.0/rand"
                 , fileContent   = error "TODO"
                 }

bounds :: Monad m => BuilderT m [Node Boundary Context]
bounds = sequence [cryptoBound, cryptoAlgBound, physBound, logBound]

data Boundary
  = Cryptographic
    { cryptoIn  :: [Component]
    , cryptoOut :: [Component]
    }
  | CryptographicAlgorithm
  | Physical
  | Logical
  | Module
  | Kernel
  deriving (Eq)

data Component
  = SWFile File
  | HW String
  deriving (Eq)

instance RenderContext Boundary where
  renderContextTypeReference _ = "boundary"
  renderContext (Cryptographic _ _)    = "cryptographic"
  renderContext CryptographicAlgorithm = "cryptographic algorithm"
  renderContext Physical               = "physical"
  renderContext Logical                = "logical"
  renderContext Module                 = "module"
  renderContext Kernel                 = "kernel"

cryptoBound :: Monad m => BuilderT m (Node Boundary Context)
cryptoBound = do
  c <- context @Boundary "Cryptographic Boundary" Nothing
  setContextValue c cryptoBoundData
  return c

cryptoBoundData :: Boundary
cryptoBoundData = Cryptographic inComp outComp
 where
  inComp  = [SWFile fipsrandc]
  outComp = []

cryptoAlgBound :: Monad m => BuilderT m (Node Boundary Context)
cryptoAlgBound = do
  c <- context @Boundary "Cryptographic Algorithm Boundary" Nothing
  setContextValue c CryptographicAlgorithm
  return c

physBound :: Monad m => BuilderT m (Node Boundary Context)
physBound = do
  c <- context @Boundary "Physical Boundary" Nothing
  setContextValue c Physical
  return c

logBound :: Monad m => BuilderT m (Node Boundary Context)
logBound = do
  c <- context @Boundary "Logical Boundary" Nothing
  setContextValue c Logical
  return c

data TestTarget
  = Interfaces
  | Services
  | Ports

instance RenderContext TestTarget where
  renderContextTypeReference _ = "target"
  renderContext _ = ""
  -- renderContext Interfaces = "module interfaces"
  -- renderContext Services = "module services"
  -- renderContext Ports = "module ports"

testISP :: Monad m => BuilderT m (Node c Strategy)
testISP = do
  tests <- context @Test "Operational" Nothing
  ints  <- context @TestTarget "Interfaces" Nothing
  servs <- context @TestTarget "Services" Nothing
  ports <- context @TestTarget "Ports" Nothing
  overTests tests [ints, servs, ports]

-- notSamePhys :: Monad m => BuilderT m (Node c Strategy)
-- notSamePhys = do
--   bounds <- sequence [cryptoBound, cryptoAlgBound, logBound]
--   phys <- physBound
--   let sSent  = [SFContext phys, SFText "differs from"] ++ map SFContext bounds
--       gSent  = [SFText "At least one boundary differs from", SFContext phys]
--       -- snSent = [SFText "Documentation check"]
--       snSent = ManualInspection (error "TODO") IsDocumented -- One of many?
--   s <- terminalStrategy sSent gSent snSent
--   addEdge s phys
--   mapM_ (addEdge s) bounds
--   return s

-----------------------------------------------------------------------------------------
-- Pattern

-- FIPS pattern for a test with a target
tested
  :: (RenderContext t, Typeable t, Monad m)
  => Node Test Context
  -> Node t Context
  -> BuilderT m (Node c Goal)
tested testCtx testTarget = do
  let gSent  = [SFContext testCtx, SFText "tests", SFContext testTarget]
      -- snSent = [SFText "Check", SFContext testCtx, SFContext testTarget]
      snSent = Tests testCtx testTarget Passes -- [SFText "Check", SFContext testCtx, SFContext testTarget]
  g <- terminalGoal gSent snSent
  addEdge g testCtx
  addEdge g testTarget
  return g

-- Set of test arguments
overTests
  :: (RenderContext t, Typeable t, Monad m)
  => Node Test Context
  -> [Node t Context]
  -> BuilderT m (Node c Strategy)
overTests test targets = do
  gs <- mapM (tested test) targets
  let sSent = [SFContext test] ++ map SFContext targets
  s <- strategy PolicyAnd sSent
  mapM_ (addEdge s) gs
  return s
