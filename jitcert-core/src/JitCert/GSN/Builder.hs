module JitCert.GSN.Builder
    ( module Export

    -- * Create nodes.
    , context
    , contextWithOptions
    , contextVerb
    , contextVerbWithOptions
    , goal
    , goalWithOptions
    , goalAnd
    , goalAndWithOptions
    , goalOr
    , goalOrWithOptions
    , solution
    , solutionWithOptions
    , strategy
    , strategyWithOptions
    , strategyAnd
    , strategyAndWithOptions
    , strategyOr
    , strategyOrWithOptions
    , assumption
    , assumptionWithOptions
    , justification
    , justificationWithOptions
    -- ** Policy nodes.
    , forall
    , andNode
    , orNode
    , notNode
    -- * Create edges.
    , addContext
    , addEdge
    , addEdges
    )
where

import           Data.Default
import qualified Data.Text.Lazy                as TL
import           Type.Reflection

import           JitCert.DocGenerator
import           JitCert.GSN.Types              ( ValidEdge
                                                , Variable
                                                , Node
                                                , NodeTy(..)
                                                , Sentence
                                                , NodePolicy(..)
                                                , RenderContext
                                                )
import qualified JitCert.GSN.Internal           as GSN
import qualified JitCert.GSN.Types              as GSN
import           JitCert.GSN.Builder.Internal
import           JitCert.GSN.Builder.Internal  as Export
                                                ( BuilderT
                                                , runBuilderT
                                                , runBuilderT'
                                                , Builder
                                                , runBuilder
                                                , setContextValue
                                                , setContextValuesWith
                                                , setSolutionEvidence
                                                , NodeOptions(..)
                                                , mkNodeOptions
                                                , setNodeOptionsReference
                                                , setNodeOptionsGenerator
                                                )


-- | Create a (noun) context node, representing what is being argued about.
context
    :: (RenderContext c, Monad m)
    => Variable                    -- ^ Variable name for context node.
    -> Maybe TL.Text               -- ^ Optional custom label for context node.
    -> BuilderT m (Node c Context)
context v s = insertNode GSN.ContextRepr (GSN.ContextNoun v s) def

-- | Create a (noun) context node with options.
contextWithOptions
    :: (RenderContext c, Monad m)
    => Variable                    -- ^ Variable name for context node.
    -> Maybe TL.Text               -- ^ Optional custom label for context node.
    -> NodeOptions                 -- ^ Node options.
    -> BuilderT m (Node c Context)
contextWithOptions v s = insertNode GSN.ContextRepr (GSN.ContextNoun v s)

-- | Create a (verb) context node, representing a situation. 
contextVerb
    :: (Typeable c, RenderContext c, Monad m)
    => TL.Text
    -> (c -> Bool)
    -> BuilderT m (Node (c -> Bool) Context)
contextVerb s v = insertNode GSN.ContextRepr (GSN.ContextVerb s v) def

-- | Create a (verb) context node with options.
contextVerbWithOptions
    :: (Typeable c, RenderContext c, Monad m)
    => TL.Text                     -- ^ Description of situation.
    -> (c -> Bool)                 -- ^ Predicate that indicates whether situation applies.
    -> NodeOptions                 -- ^ Node options.
    -> BuilderT m (Node (c -> Bool) Context)
contextVerbWithOptions s v = insertNode GSN.ContextRepr (GSN.ContextVerb s v)

-- | Create a goal node.
goal :: Monad m 
     => NodePolicy c               -- ^ Node's logical policy.
     -> Sentence                   -- ^ Sentence description of node.
     -> BuilderT m (Node c Goal)
goal n s = insertNode GSN.GoalRepr (GSN.NodePolicyContent n s) def

-- | Create a goal node with options.
goalWithOptions
    :: Monad m
    => NodePolicy c               -- ^ Node's logical policy.
    -> Sentence                   -- ^ Sentence description of node.
    -> NodeOptions                -- ^ Node options.
    -> BuilderT m (Node c Goal)
goalWithOptions n s = insertNode GSN.GoalRepr (GSN.NodePolicyContent n s)

-- | Create a goal node with an AND policy.
goalAnd :: Monad m => Sentence -> BuilderT m (Node c Goal)
goalAnd s = insertNode GSN.GoalRepr (GSN.NodePolicyContent PolicyAnd s) def

-- | Create a goal node with an AND policy given options.
goalAndWithOptions
    :: Monad m => Sentence -> NodeOptions -> BuilderT m (Node c Goal)
goalAndWithOptions s =
    insertNode GSN.GoalRepr (GSN.NodePolicyContent PolicyAnd s)

-- | Create a goal node with an OR policy.
goalOr :: Monad m => Sentence -> BuilderT m (Node c Goal)
goalOr s = insertNode GSN.GoalRepr (GSN.NodePolicyContent PolicyOr s) def

-- | Create a goal node with an OR policy given options.
goalOrWithOptions
    :: Monad m => Sentence -> NodeOptions -> BuilderT m (Node c Goal)
goalOrWithOptions s =
    insertNode GSN.GoalRepr (GSN.NodePolicyContent PolicyOr s)

-- | Create a special goal node with an AND policy and no description.
andNode :: Monad m => BuilderT m (Node c Goal)
andNode = insertNode GSN.GoalRepr GSN.NodePolicyAnd $ setNodeOptionsGenerator def dgAnd

-- | Create a special goal node with an OR policy and no description.
orNode :: Monad m => BuilderT m (Node c Goal)
orNode = insertNode GSN.GoalRepr GSN.NodePolicyOr $ setNodeOptionsGenerator def dgOr

-- | Create a special goal node with a NOT policy and no description.
notNode :: Monad m => NodePolicy c -> BuilderT m (Node c Goal)
notNode p = insertNode GSN.GoalRepr (GSN.NodePolicyNot p) $ setNodeOptionsGenerator def dgNot

-- | Create a solution node. It expects a `SolutionType` as an argument. For example, the following indicates that test @t@ tests that function @f@ is correct:
--
-- @
-- s <- solution (Tests t f IsCorrect)
-- @
--
solution
    :: (Typeable c, GSN.SolutionType c, Monad m)
    => c                                            -- ^ The type of the solution node.
    -> BuilderT m (Node c Solution)
solution c = insertNode GSN.SolutionRepr (GSN.SolutionContent c) def

-- | Create a solution node given options.
solutionWithOptions
    :: (Typeable c, GSN.SolutionType c, Monad m)
    => c
    -> NodeOptions
    -> BuilderT m (Node c Solution)
solutionWithOptions c = insertNode GSN.SolutionRepr (GSN.SolutionContent c)

-- | Create a strategy node with an AND policy.
strategyAnd :: Monad m => Sentence -> BuilderT m (Node c Strategy)
strategyAnd s =
    insertNode GSN.StrategyRepr (GSN.NodePolicyContent PolicyAnd s) def

-- | Create a strategy node with an OR policy.
strategyOr :: Monad m => Sentence -> BuilderT m (Node c Strategy)
strategyOr s =
    insertNode GSN.StrategyRepr (GSN.NodePolicyContent PolicyOr s) def

-- | Create a strategy node with an OR policy given options.
strategyOrWithOptions :: Monad m => Sentence -> NodeOptions -> BuilderT m (Node c Strategy)
strategyOrWithOptions s =
    insertNode GSN.StrategyRepr (GSN.NodePolicyContent PolicyOr s)

-- | Create a strategy node with an AND policy given options.
strategyAndWithOptions
    :: Monad m => Sentence -> NodeOptions -> BuilderT m (Node c Strategy)
strategyAndWithOptions s =
    insertNode GSN.StrategyRepr (GSN.NodePolicyContent PolicyAnd s)


-- | Create a strategy node.
strategy :: Monad m 
         => NodePolicy c               -- ^ Node's logical policy.
         -> Sentence                   -- ^ Sentence description of node. 
         -> BuilderT m (Node c Strategy)
strategy n s = insertNode GSN.StrategyRepr (GSN.NodePolicyContent n s) def

-- | Create a strategy node with options.
strategyWithOptions
    :: Monad m
    => NodePolicy c
    -> Sentence
    -> NodeOptions
    -> BuilderT m (Node c Strategy)
strategyWithOptions n s =
    insertNode GSN.StrategyRepr (GSN.NodePolicyContent n s)

-- | Create an assumption node.
assumption :: Monad m => Sentence -> BuilderT m (Node c Assumption)
assumption s = insertNode GSN.AssumptionRepr s def

-- | Create an assumption node with options.
assumptionWithOptions
    :: Monad m => Sentence -> NodeOptions -> BuilderT m (Node c Assumption)
assumptionWithOptions = insertNode GSN.AssumptionRepr

-- | Create a justification node.
justification :: Monad m => Sentence -> BuilderT m (Node c Justification)
justification s = insertNode GSN.JustificationRepr s def

-- | Create a justification node with options.
justificationWithOptions
    :: Monad m => Sentence -> NodeOptions -> BuilderT m (Node c Justification)
justificationWithOptions = insertNode GSN.JustificationRepr

-- | Add an edge to a context node.
addContext
    :: (ValidEdge n Context, Monad m)
    => Node c n
    -> Node d Context
    -> BuilderT m ()
addContext = insertEdge

-- | Add a valid edge between two nodes.
addEdge :: (Monad m, ValidEdge n1 n2) => Node c n1 -> Node d n2 -> BuilderT m ()
addEdge = insertEdge

-- | Add multiple edges to a node.
addEdges
    :: (Foldable f, Monad m, ValidEdge n1 n2)
    => Node c n1                                -- ^ Parent node.
    -> f (Node d n2)                            -- ^ Children nodes.
    -> BuilderT m ()
addEdges n1 = mapM_ (addEdge n1)


-- | Creates a hidden context node representing elements of the given list and its associated FORALL policy.
-- The following example creates a FORALL policy for all functions @f@ in the functions context node @fs@.
--
-- @
-- (f, p) <- forall "f" fs
-- gs     <- goal p [SFContext f, SFText "is correct"]
-- @
forall
    :: (RenderContext c, Monad m)
    => Variable                     -- ^ Variable name for hidden node introduced by policy. 
    -> Node [c] Context             -- ^ Context node representing list of objects.
    -> BuilderT m (Node c Context, NodePolicy c)
forall v cs = do
    i <- nextIdentifier

    let cNode = GSN.context i v Nothing -- d
    insertHiddenNode $ GSN.SomeNode cNode

    addBoundNode cNode cs

    return (cNode, PolicyForall cNode cs PolicyAnd)
