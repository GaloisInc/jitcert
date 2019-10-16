module JitCert.Docs.ProgramIsSafe where

import JitCert.Evidence
import JitCert.DocGenerator
import JitCert.Docs.FunctionsCorrect
import JitCert.Docs.Shared
import JitCert.GSN.Builder
import JitCert.GSN.Combinators
import JitCert.GSN.Types

rngOptions :: NodeOptions
rngOptions = setNodeOptionsGenerator def $ dgText "Random number generator is sufficiently random."

figure7 :: Monad m => BuilderT m (Node [Function] Context, Node RNG Context, Node a Goal)
figure7 = do
    p  <- context @SoftwareProgram "p" Nothing

    g1 <- goalWithOptions PolicyAnd [SFContext p, SFText "is safe"] $ setNodeOptionsGenerator def dgAnd
    addEdge g1 p

    rng <- context @RNG "g" Nothing

    g2  <- goalWithOptions PolicyAnd [SFContext rng, SFText "is is sufficiently random"] rngOptions
    addEdge g1 g2
    addEdge g2 rng

    s <- manualInspection rng ProducesUniformDistribution
    addEdge g2 s

    setSolutionEvidence s $ Inspected "Alice" (read "1999-01-01 00:00:00.000000 UTC") True

    (fs, _, g3, _, _) <- figure5'
    addEdge g1 g3

    return (fs, rng, g1)

