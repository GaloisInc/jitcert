module JitCert.Docs (module JitCert.Docs) where

import           Control.Monad
import           Control.Monad.Trans
import           Data.Digest.Pure.SHA
import           Data.FileEmbed
import qualified Data.GraphViz                 as Dot
import qualified Data.Map                      as Map
import           Data.Monoid                    ( (<>) )
import qualified Data.Set                      as Set
import qualified Data.Text                     as ST
import           Data.Time.Clock
import qualified System.Process                as Process

import           JitCert.Context
import           JitCert.Docs.FunctionsCorrect as JitCert.Docs
import           JitCert.Docs.ProgramIsSafe    as JitCert.Docs
import           JitCert.Docs.ProgramIsSafeSimplified as JitCert.Docs
import           JitCert.Docs.UnboundError     as JitCert.Docs
import           JitCert.Docs.Shared
import           JitCert.DotBackend
import           JitCert.Evidence
import           JitCert.GSN                    ( normalizeGSN )
import           JitCert.GSN.Builder
import           JitCert.GSN.Types
import           JitCert.Properties
import           JitCert.Query
import           JitCert.Solution

buildDocImages :: IO ()
buildDocImages = do
    Process.callCommand $ "mkdir -p " <> "./doc/img"
    -- Generate images for documentation.
    saveGSN "./doc/img/figure1.png" figure1
    saveGSN "./doc/img/figure2.png" figure2
    saveGSN "./doc/img/figure3.png" figure3
    saveGSN "./doc/img/figure4.png" figure4
    saveGSN "./doc/img/figure5.png" $ void figure5
    saveGSN "./doc/img/figure6.png" figureUnboundError
    saveGSN "./doc/img/figure7.png" $ void figure7
    saveGSN "./doc/img/figure8.png"     figure8
    saveGSN "./doc/img/figure9.png"     figure9
    saveGSN "./doc/img/figureLogic.png" figureLogic
    do
        (g, e) <- runBuilderT figureLogic
        saveGSN' "./doc/img/figureNormalize.png" (normalizeGSN g) e
    ns <- runEnvDiff figure9 figure10
    print $ map _sNodeId $ Set.toList ns
    saveGSNWithHighlight "./doc/img/figure10.png" ns figure10

runEnvDiff :: Monad m => BuilderT m () -> BuilderT m () -> m (Set.Set SomeNode)
runEnvDiff g1 g2 = do
    (gsn, env1) <- runBuilderT g1
    (_  , env2) <- runBuilderT g2

    return $ envDiff gsn env1 env2


data SolutionNode = SolutionNode

instance SolutionType SolutionNode where
    type SolutionEvidence SolutionNode = ()

    validEvidence () = True

    renderEvidence () = "No evidence"

    renderSolution _s = [SFText "Solution node"]

figure1 :: Monad m => BuilderT m ()
figure1 = do
    g <- goalWithOptions PolicyAnd [SFText "Top level goal"] $ setNodeOptionsReference def "Reference"

    s <- solution SolutionNode
    addEdge g s

    return ()

-- instance ToJSON ProducesUniformDistribution where
--     toJSON ProducesUniformDistribution = Aeson.object [
--           "type" .= ("ProducesUniformDistribution" :: Text)
--         ]

-- instance FromJSON ProducesUniformDistribution where
--     parseJSON (Aeson.Object o) = do
--         t <- o .: "type"
--         if t == ("ProducesUniformDistribution" :: Text) then
--             return ProducesUniformDistribution
--         else
--             fail "invalid type"
--     parseJSON _ = fail "invalid json"


-- data PassesTest = PassesTest (Node Test Context)
--
-- instance Property PassesTest where
--     renderProperty (PassesTest t) = [SFText "passes", SFContext t]

-- instance ToJSON PassesTest where
--     toJSON (PassesTest n) = Aeson.object [
--           "type" .= ("PassesTest" :: Text)
--         , "property" .= n
--         ]

-- instance FromJSON PassesTest where
--     parseJSON (Aeson.Object o) = do
--         t <- o .: "type"
--         p <- o .: "property"
--         if t == ("PassesTest" :: Text) then
--             return $ PassesTest p
--         else
--             fail "invalid type"
--     parseJSON _ = fail "invalid json"

-- instance ToJSON IsSafe where
--     toJSON IsSafe = Aeson.object [
--           "type" .= ("IsSafe" :: Text)
--         ]

-- instance FromJSON IsSafe where
--     parseJSON (Aeson.Object o) = do
--         t <- o .: "type"
--         if t == ("IsSafe" :: Text) then
--             return IsSafe
--         else
--             fail "invalid type"
--     parseJSON _ = fail "invalid json"

-- instance SolutionType (PassesTest t c p) where
--     -- type SolutionReferences (PassesTest t c p) = (t, c)
--     -- type SolutionEvidence (PassesTest t c p) = TestResult
--
--     -- validEvidence
--
--     renderSolution (PassesTest c t p) = [SFContext t, SFText "that", SFContext c, SFProperty p]



programTargetsLinux :: SoftwareProgram -> Bool
programTargetsLinux = const True

figure2 :: Monad m => BuilderT m ()
figure2 = do
    p <- context @SoftwareProgram "p" Nothing

    g <- goal PolicyAnd [SFContext p, SFProperty IsSafe]
    addEdge g p

    t <- context @Test "t" Nothing
    addEdge g t

    -- s <- solution @() [SFText "Test that", SFContext p, SFText "is safe"]
    s <- solution (Tests t p IsSafe)
    addEdge g s

    return ()

figure3 :: Monad m => BuilderT m ()
figure3 = do
    p <- context @SoftwareProgram "p" Nothing
    setContextValue p (SoftwareProgram "myProgram"
                      Settings
                      (Files mempty))

    g <- goal PolicyAnd [SFContext p, SFText "is compatible"]
    addEdge g p

    c  <- contextVerb "targets Linux" programTargetsLinux

    g1 <- goal (PolicyWhen p c PolicyAnd)
               [SFContext p, SFProperty UsesLinuxLibraries]
    addEdge g  g1
    addEdge g1 c

    -- s <- solution @() [SFText "Manual inspection that", SFContext p, SFText "uses Linux libraries"]
    s <- solution (ManualInspection p UsesLinuxLibraries)
    addEdge g1 s

    setSolutionEvidence s $ Inspected "Alice" (read "2000-01-01 00:00:00.000000 UTC") True

data UsesLinuxLibraries = UsesLinuxLibraries

instance Property UsesLinuxLibraries where
    renderProperty UsesLinuxLibraries = [SFText "uses Linux libraries"]

figure4 :: MonadIO m => BuilderT m ()
figure4 = build
  where
    build = do
        f <- context @Function "f" Nothing

        t <- context @Test "t" Nothing

        g <- goal PolicyAnd [SFContext f, SFText "is correct"]
        addEdge g f
        addEdge g t

        -- s <- solution @(TestResult Function PassesTest) [SFContext f, SFProperty (PassesTest t)]
        s <- solution (Tests t f IsCorrect)
        addEdge g s

        setContextValue f (Function "fibonacci" "c9a0ad2c")
        setContextValue t (Test "test_fibonacci" (sha256 "..."))

        now <- liftIO getCurrentTime
        setSolutionEvidence s $ TestResult now True


figure9 :: Monad m => BuilderT m ()
figure9 = do
    (_, rng, _) <- figure7

    setContextValue rng $ RNG "Dual_EC_DRBG"

figure10 :: Monad m => BuilderT m ()
figure10 = do
    (_, rng, _) <- figure7

    setContextValue rng $ RNG "HMAC_DRBG"

figureLogic :: Monad m => BuilderT m ()
figureLogic = do
    s  <- strategy PolicyAnd [SFText "Strategy 1"]

    n1 <- andNode
    addEdge s n1

    n2 <- notNode PolicyOr
    addEdge n1 n2

    g1 <- goal PolicyAnd [SFText "Goal 1"]
    addEdge n1 g1

    g2 <- goal PolicyAnd [SFText "Goal 2"]
    addEdge n2 g2

    g3 <- goal PolicyAnd [SFText "Goal 3"]
    addEdge n2 g3

    return ()



figureNot :: Monad m => BuilderT m ()
figureNot = do
    f <- context @Function "f" Nothing

    t <- context @Test "t" Nothing

    g <- goal (PolicyNot $ PolicyNot PolicyAnd)
              [SFContext f, SFText "is correct"]

    addEdge g f
    addEdge g t

    -- s <- solution @(TestResult Function PassesTest) [SFContext f, SFProperty (PassesTest t)]
    s <- solution (Tests t f IsCorrect)
    addEdge g s


saveGSN :: FilePath -> BuilderT IO () -> IO ()
saveGSN file = saveGSNWithHighlight file mempty

saveGSN' :: FilePath -> GSNGraph -> GSNEnv -> IO ()
saveGSN' file = saveGSNWithHighlight' file mempty

saveGSNWithHighlight :: FilePath -> Set.Set SomeNode -> BuilderT IO () -> IO ()
saveGSNWithHighlight file ns g = do
    (gsn, env) <- runBuilderT g
    saveGSNWithHighlight' file ns gsn env

saveGSNWithHighlight'
    :: FilePath -> Set.Set SomeNode -> GSNGraph -> GSNEnv -> IO ()
saveGSNWithHighlight' file ns gsn env = do

    saveWithEnvHighlight tmpFile gsn env h

    Process.callCommand $ "dot -Tpng " <> tmpFile <> " > " <> file

  where
    tmpFile = "/tmp/jitcertdot"

    h = GSNHighlight $ Map.fromSet (const Dot.Tomato) $ Set.map _sNodeId ns



testFunction
    :: (Monad m)
    => Node g Goal
    -> BuilderT
           m
           ( Node Function Context
           , Node Test Context
           , Node (Tests Function IsCorrect) Solution
           )
testFunction parent = do
    f <- context @Function "f" Nothing
    t <- context @Test "t" Nothing

    g <- goal PolicyAnd [SFContext f, SFText "is correct"]
    addEdge g f
    addEdge g t

    s <- solution (Tests t f IsCorrect)
    addEdge g      s

    addEdge parent g
    return (f, t, s)

functionsAreCorrectRaw :: ST.Text
functionsAreCorrectRaw = $(embedStringFile "src/JitCert/Docs/FunctionsCorrect.hs")

unboundErrorRaw :: ST.Text
unboundErrorRaw =  $(embedStringFile "src/JitCert/Docs/UnboundError.hs")

programIsSafeSimplifiedRaw :: ST.Text
programIsSafeSimplifiedRaw = $(embedStringFile "src/JitCert/Docs/ProgramIsSafeSimplified.hs")

programIsSafeRaw :: ST.Text
programIsSafeRaw = pre <> _dropLines 3 functionsAreCorrectRaw <> _dropLines 9 (dropLinesEnd 6 ps) <> manual
    where
        pre = "module WebGSN where\n\nimport           JitCert.GSN.Combinators\nimport           Data.Digest.Pure.SHA\n"
        ps = $(embedStringFile "src/JitCert/Docs/ProgramIsSafe.hs")

        manual = "\n\
          \    (_, fs, _, g3, _, _) <- figure5\n\
          \    addEdge g1 g3\n\
          \    \n\
          \    return (fs, rng, g1)\n\
          \\n\
          \ "


        dropLinesEnd n t = ST.unlines $ dropEnd n $ ST.lines t

        dropEnd n = reverse . drop n . reverse

opensslRaw :: ST.Text
opensslRaw = _dropLines 10 $ $(embedStringFile "src/JitCert/Examples/OpenSSL.hs")

utmRaw :: ST.Text
utmRaw = _dropLines 14 $ $(embedStringFile "src/JitCert/Examples/UTM.hs")

_dropLines :: Int -> ST.Text -> ST.Text
_dropLines n t = ST.unlines $ drop n $ ST.lines t



