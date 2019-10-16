module JitCert.Docs.UnboundError where

import           JitCert.Docs.Shared
import           JitCert.GSN.Builder
import           JitCert.GSN.Types

figureUnboundError :: Monad m => BuilderT m ()
figureUnboundError = do
    g1 <- goal PolicyAnd [SFText "Top level goal"]

    g2 <- goal PolicyAnd [SFText "A goal"]
    addEdge g1 g2

    p <- context @SoftwareProgram "p" Nothing
    addEdge g2 p

    g3 <- goal PolicyAnd
               [SFText "Cousin goal that", SFContext p, SFText "is safe"]

    addEdge g1 g3

