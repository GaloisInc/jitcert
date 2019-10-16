{-|

Description: Exploration of integrating NLP with the DSL.

This module (which is a proof of concept and not a finished product) revolves
around an exploration of how one could integrate natural language processing
with the DSL.  The high-level goal would be to give, as input, a specification
document such as the FIPS 140 standard, and automatically generate a template
assurance case.

-}

module JitCert.Nlp2Dsl where

import qualified Data.ByteString.Char8 as B8
import           Data.Proxy
import qualified Data.Text.Lazy as T
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString (send, recv)
import qualified System.Process as S
import           Type.Reflection

import           JitCert.DotBackend
import           JitCert.GSN.Builder
import           JitCert.GSN.Builder.Internal ( runBuilderT )
import           JitCert.GSN.Combinators
import           JitCert.GSN.Types
import           JitCert.Solution


s2goal :: Monad m => T.Text -> BuilderT m (Node c 'Goal)
s2goal s = goal PolicyAnd [SFText s]

s2strategy :: Monad m => T.Text -> BuilderT m (Node c Strategy)
s2strategy s = strategyAnd [SFText s]


data CTX = CTX T.Text
    deriving Eq

instance RenderContext CTX where
    renderContextTypeReference Proxy = "context-description"
    renderContext (CTX t) = t

data Prop = Prop
instance Property Prop where
    renderProperty Prop =
        [SFText "Property-is-true"]


data GenericEvidence c p = GenericEvidence {
      info :: T.Text
    , evidenceValidates :: Bool
    }
    deriving (Eq)

data GenericSoln c p = GenericSoln (Node c Context) p
instance (Typeable c, Typeable p, RenderContext c, Property p) => SolutionType (GenericSoln c p) where
    type SolutionEvidence (GenericSoln c p) = GenericEvidence c p

    validEvidence GenericEvidence {..} = evidenceValidates

    renderEvidence GenericEvidence{..} = "GenericEvidence " <> info <> if evidenceValidates then " passed" else " failed"

    renderSolution (GenericSoln c p) = [SFText "GenericSoln", SFContext c, SFProperty p]

genericSoln
    :: (RenderContext c, Typeable c, Typeable p, Property p, Monad m)
    => Node c Context
    -> p
    -> BuilderT m (Node (GenericSoln c p) Solution)
genericSoln c p =
    solution (GenericSoln c p)



s2solution2
    :: (Typeable p, Property p, RenderContext c, Typeable c, Monad m)
    => T.Text
    -> (Node c Context)
    -> p
    -> BuilderT m (Node (ManualInspection c p) Solution)
s2solution2 _ c p = manualInspection c p

s2solution
     :: (RenderContext c, Typeable c, Monad m)
     => T.Text
     -> (Node c Context)
     -> BuilderT m (Node (ManualInspection c Prop) Solution)
s2solution _ c = manualInspection c Prop

--TODO: Why does the below throw an error?
--s2context :: (RenderContext c, Typeable c, Monad m) => T.Text -> Variable -> BuilderT m (Node c Context)
--s2context s c = context c (Just s)
s2context :: (Monad m) => T.Text -> BuilderT m (Node CTX Context)
s2context s = context @CTX "ctx_var" (Just s)

gsn
    :: Monad m
    => T.Text
    -> T.Text
    -> T.Text
    -> T.Text
    -> BuilderT m ()
gsn goalS contextS _ solutionS = do
    g <- s2goal goalS

    --TODO: Why does the below throw an error?
    --ctx <- s2context contextS @CTX var
    ctx <- s2context contextS

    sn <- s2solution2 solutionS ctx Prop

    addEdge g ctx
    addEdge g sn

    --addEdge g c
    --addEdge g sn

nl2Dsl :: Monad m => String -> BuilderT m ()
nl2Dsl _ =
    -- write the input to a file
    -- execute nlp2Dsl.py
    -- read the output of nlp2Dsl.py
    gsn
    (T.pack "This is the goal")
    (T.pack "The context")
    (T.pack "The strategy")
    (T.pack "The solution")

simpleExample1 :: Monad m => BuilderT m ()
simpleExample1 = do
    let s = unlines ["Goal: The cryptographic module m is contained within the cryptographic boundary." , "Context: m." , "Solution: Manual inspection." ]
    nl2Dsl s

simpleExample2 :: IO String
simpleExample2 = do
    let s = unlines ["Goal: The cryptographic module m is contained within the cryptographic boundary." , "Context: m." , "Solution: Manual inspection." ]
    writeFile "_nlp.tmp" s
    S.readProcess "nlp/nlp2Dsl.py" [] ""

simpleExample3 :: IO String
simpleExample3 = do
    let s = unlines ["The cryptographic module m is contained within the cryptographic boundary.", "This can be inspected manually." ]
    writeFile "_nlp.tmp" s
    S.readProcess "nlp/nlp2Dsl.py" [] ""


runNLPEx :: IO ()
runNLPEx = do
  (g, _) <- runBuilderT simpleExample1
  out <- simpleExample2
  putStrLn out
  render "nlp2dsl.dot" g

nl2DslString :: String -> IO String
nl2DslString s = do
    -- write the input to a file
    -- execute nlp2Dsl.py
    -- read the output of nlp2Dsl.py
    writeFile "_nlp.tmp" s
    S.readProcess "nlp/nlp2Dsl.py" [] ""


-- TODO: The client code needs to be tested and probably re-worked using a
-- higher level interface (sockets?).
client' :: Int -> IO ()
client' = client "localhost"

client :: String -> Int -> IO ()
client host port = withSocketsDo $ do
                addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
                let serverAddr = head addrInfo
                sock <- socket (addrFamily serverAddr) Stream defaultProtocol
                connect sock (addrAddress serverAddr)
                msgSender sock
                close sock

msgSender :: Socket -> IO ()
msgSender sock = do
  msg <- B8.getLine
  _ <- send sock msg
  rMsg <- recv sock 10
  B8.putStrLn rMsg
  if msg == B8.pack "q" then putStrLn "Disconnected!" else msgSender sock
