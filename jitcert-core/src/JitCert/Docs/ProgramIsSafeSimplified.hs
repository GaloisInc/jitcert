module JitCert.Docs.ProgramIsSafeSimplified where

import JitCert.Docs.Shared
import JitCert.GSN.Builder
import JitCert.GSN.Combinators
import JitCert.GSN.Types
import JitCert.Properties

figure8 :: Monad m => BuilderT m ()
figure8 = do
    p          <- context @SoftwareProgram "p" Nothing

    (ms, _, _) <- manualInspectionOrStaticAnalysis
        [SFContext p, SFProperty IsSafe]
        p
        IsSafe
    addEdge ms p
