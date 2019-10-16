module JitCert.GSN.Combinators where

import Type.Reflection

import JitCert.Context
import JitCert.DocGenerator
import JitCert.GSN.Builder
import JitCert.GSN.Types
import JitCert.Properties
import JitCert.Solution

-- | A GSN argument in which a set of subgoals is connected to an OR node
orBranches :: Monad m => [Node a Goal] -> BuilderT m (Node b Goal)
orBranches gs = do
  or <- orNode
  mapM_ (addEdge or) gs
  return or

-- | A GSN argument in which a set of subgoals is connected to an AND node
andBranches :: Monad m => [Node a Goal] -> BuilderT m (Node b Goal)
andBranches gs = do
  and <- andNode
  mapM_ (addEdge and) gs
  return and

-- | A trivial GSN argument in which a solution follows a goal node
goalSolution :: Monad m => Node a Goal -> Node b Solution -> BuilderT m (Node a Goal)
goalSolution g sn = addEdge g sn >> return g

-- | A GSN argument that enumerates a set of context nodes which are to be well defined.
setOfDefinitions
  :: (Monad m, RenderContext t)
  => [Node t Context] -> BuilderT m (Node t Goal)
setOfDefinitions cs = do
  goalAndWithOptions
    (map SFContext cs ++ [SFText "defined"])
    (mkNodeOptions (dgListHeader "All of the following are well defined.") "tmp ref")

-- | GSN pattern for strat/goal/solution
terminalStrategy
  :: (Typeable s, SolutionType s, Monad m)
  => Sentence
  -> Sentence
  -> s
  -> BuilderT m (Node c Strategy)
terminalStrategy sSent gSent snSent = do
  s <- strategy PolicyAnd sSent
  g <- terminalGoal gSent snSent
  addEdge s g
  return s

-- | GSN pattern for goal/solution
terminalGoal
  :: (Typeable s, SolutionType s, Monad m)
  => Sentence
  -> s
  -> BuilderT m (Node c Goal)
terminalGoal gSent snSent = do
  g  <- goal PolicyAnd gSent
  sn <- solution snSent
  addEdge g sn
  return g

goalContextSolution
    :: Monad m
    => Node c Goal
    -> Node d Context
    -> Node g Solution
    -> BuilderT m ()
goalContextSolution g c sn = do
    addEdge g c
    addEdge g sn

strategySubgoal
    :: Monad m
    => Node c Goal
    -> Node d Context
    -> Node e Strategy
    -> Node f Goal
    -> BuilderT m ()
strategySubgoal g1 c s g2 = do
    addEdge g1 c
    addEdge g1 s
    addEdge s  g2
    -- addEdge g2 sn

strategySubgoals
    :: Monad m
    => Node c Goal
    -> Node d Context
    -> Node e Strategy
    -> [SomeGoal]
    -> BuilderT m ()
strategySubgoals g1 c s gs = do
    addEdge g1 c
    addEdge g1 s
    mapM_ (\(SomeGoal g) -> addEdge s g) gs

strategySubgoalChain
    :: Monad m => Node c Goal -> [(SomeStrategy, SomeGoal)] -> BuilderT m ()
strategySubgoalChain _g1 [] = return ()
strategySubgoalChain g1 ((SomeStrategy h, SomeGoal g) : t) = do
    addEdge g1 h
    addEdge h  g
    strategySubgoalChain g t


-- * Combinators for constructing commonly used nodes.

-- | Solution node stating that the given context is documented.
documented
    :: (RenderContext c, Typeable c, Monad m)
    => Node c Context
    -> BuilderT m (Node (ManualInspection c IsDocumented) Solution)
documented c =
    manualInspection c IsDocumented
    -- solution [SFContext c, SFText "is documented"]

tests
    :: forall c p d m
     . (RenderContext c, Typeable c, Monad m, Property p, Typeable p)
    => Node Test Context
    -> Node c Context
    -> p
    -> BuilderT m (Node d Goal)
tests t c p = do
    g <- goal PolicyAnd [SFContext c, SFText "tests", SFProperty p]

    e <- solution (Tests t c p)
    addEdge g e


    return g

-- | Create a goal for policy p supported by either manual inspection or static program analysis.
manualInspectionOrStaticAnalysis
    :: (RenderContext c, Typeable c, Typeable p, Property p, Monad m)
    => Sentence
    -> Node c Context
    -> p
    -> BuilderT
           m
           ( Node d Goal
           , Node (ManualInspection c p) Solution
           , Node (StaticProgramAnalysis c p) Solution
           )
manualInspectionOrStaticAnalysis s c p = do
    g    <- goal PolicyOr s

    insp <- manualInspection c p
    addEdge g insp

    sa <- staticProgramAnalysis c p
    addEdge g sa

    return (g, insp, sa)


-- | Solution node that context satisfies property by manual inspeciton.
manualInspection
    :: (RenderContext c, Typeable c, Typeable p, Property p, Monad m)
    => Node c Context
    -> p
    -> BuilderT m (Node (ManualInspection c p) Solution)
manualInspection c p =
    solution (ManualInspection c p)

-- | Solution node that context satisfies property by static program analaysis.
staticProgramAnalysis
    :: (RenderContext c, Typeable c, Typeable p, Property p, Monad m)
    => Node c Context
    -> p
    -> BuilderT m (Node (StaticProgramAnalysis c p) Solution)
staticProgramAnalysis c p = solution (StaticProgramAnalysis c p)

-- | Solution node that context satisfies property by static program analaysis.
dynamicProgramAnalysis
    :: (RenderContext c, Typeable c, Typeable p, Property p, Monad m)
    => Node c Context
    -> p
    -> BuilderT m (Node (DynamicProgramAnalysis c p) Solution)
dynamicProgramAnalysis c p = solution (DynamicProgramAnalysis c p)
