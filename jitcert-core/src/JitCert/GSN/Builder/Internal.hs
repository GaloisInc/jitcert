module JitCert.GSN.Builder.Internal where

import qualified Control.Monad.Fail            as Fail
import           Control.Monad.State
import           Data.Default
import           Data.Functor.Identity
import qualified Data.Graph.Inductive          as FGL
import qualified Data.Map.Strict               as Map
import           Data.Monoid                    ( (<>) )
import           Type.Reflection

import           JitCert.DocGenerator           ( dgEmpty )
import           JitCert.DocGenerator.Types
import           JitCert.GSN.Internal           ( mkEdge
                                                , edgeSrc
                                                , edgeDst
                                                )
import           JitCert.GSN.Types

-- * @BuilderT@ monad transformer.

-- | Internal state of `BuilderT`.
data BuilderState = BuilderState {
      builderStateNextIdentifier :: Int      -- ^ Next available identifier for nodes/edges.
    , builderStateNodes :: [SomeNode]        -- ^ List of nodes inserted.
    , builderStateHiddenNodes :: [SomeNode]  -- ^ List of hidden nodes (nodes not displayed in the graph, but ).
    , builderBoundNodes :: Map.Map Int Int   -- ^ Map to nodes that are bound to the key node.
    , builderStateEdges :: [SomeEdge]        -- ^ List of inserted edges.
    , builderEnvironment :: GSNEnv           -- ^ Current environment.
    }

-- JP: Maybe RWST makes more sense?
-- | Monad transformer used to construct GSN arguments and their environments.
newtype BuilderT m a = BuilderT (StateT BuilderState m a)
    deriving (Functor, Applicative, Monad, MonadIO)

instance Monad m => MonadState BuilderState (BuilderT m) where
    get = BuilderT get
    put s = BuilderT $ put s
    state f = BuilderT $ state f

instance MonadTrans BuilderT where
    lift = BuilderT . lift

-- This is a hack and should be implemented
instance Monad m => Fail.MonadFail (BuilderT m) where
    fail = undefined

-- | Run a `BuilderT` and return the constructed GSN argument and environment.
runBuilderT :: Monad m => BuilderT m () -> m (GSNGraph, GSNEnv)
runBuilderT m = do
    (_, g, e) <- runBuilderT' m
    return (g, e)

-- | Similar to `runBuilderT`, except this function also returns the value returned by the monadic computation.
runBuilderT' :: Monad m => BuilderT m a -> m (a, GSNGraph, GSNEnv)
runBuilderT' (BuilderT m) = do
    (r, BuilderState {..}) <- runStateT m s
    let g = mkGraph builderStateNodes
                    builderStateHiddenNodes
                    builderStateEdges
                    builderBoundNodes
    return (r, g, builderEnvironment)
    where s = BuilderState 0 [] [] mempty [] $ GSNEnv Map.empty Map.empty

-- | Type alias for `BuilderT` without an underlying monad.
type Builder a = BuilderT Identity a

-- | Run a `Builder` and return the constructed GSN argument and environment.
runBuilder :: Builder () -> (GSNGraph, GSNEnv)
runBuilder = runIdentity . runBuilderT


-- * Set values in the environment.

-- | Assigns a value to the context node.
setContextValue
    :: (Monad m, RenderContext c, Eq c, Typeable c)
    => Node c Context  -- ^ Context node.
    -> c               -- ^ Value inserted.
    -> BuilderT m ()
setContextValue (Node _ nodeIdentifier _ _ _) c =
    setContextValue' (unNodeId nodeIdentifier) $ SomeContext c

-- | Assigns a value to a foldable (list) context node. Also takes an argument that can assign values that correspond to each element of the list. For example:
--
-- @
-- let fs = [ (Function "fibonacci", Test "test_fibonacci")
--          , (Function "foo", Test "test_foo")
--          ]
-- setContextValuesWith fsNode fs $ \(function, functionTest) -> do
--     setContextValue tNode functionTest
--     return function
-- @
--
setContextValuesWith
    :: forall f c d m
     . ( Monad m
       , Ord c
       , Typeable c
       , RenderContext (f c)
       , RenderContext c
       , Eq (f c)
       , Typeable f
       , Monoid (f c)
       , Applicative f
       , Foldable f
       )
    => Node (f c) Context     -- ^ List context node.
    -> f d                    -- ^ The value list.
    -> (d -> BuilderT m c)    -- ^ Monadic function that assigns values corresponding to each element of the list.
    -> BuilderT m ()
setContextValuesWith node ds f = do
    s     <- get

    cNode <- case Map.lookup (unNodeId $ nodeId node) $ builderBoundNodes s of
        Nothing ->
            fail
                "setContextValuesWith: Node does not have another node bound to it. Maybe call `forall` first."
        Just cn -> return cn

    (cs, nodeEnv) <- foldM
        (\(accV, acc) d -> do
        -- Empty builder environment.
            put $ s { builderEnvironment = GSNEnv mempty mempty }

            -- Run f.
            c <- f d

            -- Set the node's context value.
            setContextValue' cNode $ SomeContext c

            -- Update accumulator environment.
            s' <- get
            let env' = builderEnvironment s'
            return (accV <> pure c, Map.insert (ContextKey c) env' acc)
        )
        (mempty @(f c), mempty)
        ds

    let (GSNEnv sCEnv sSEnv) = builderEnvironment s
    let cEnv' = Map.insert (unNodeId $ nodeId node)
                           (GSNEnvContextEnv (SomeContext cs) nodeEnv)
                           sCEnv
    put $ s { builderEnvironment = GSNEnv cEnv' sSEnv }

-- | Sets the evidence for a solution node in the environment.
setSolutionEvidence
    :: ( Monad m
       , SolutionType e
       , Eq (SolutionEvidence e)
       , Typeable (SolutionEvidence e)
       )
    => Node e Solution         -- ^ Solution node.
    -> SolutionEvidence e      -- ^ Evidence.
    -> BuilderT m ()
setSolutionEvidence (Node _ nodeIdentifier (SolutionContent _) _ _) e = do
    s <- get
    let (GSNEnv sCEnv sSEnv) = builderEnvironment s

    let sEnv' = Map.insert (unNodeId nodeIdentifier) (SomeEvidence e) sSEnv
    put $ s { builderEnvironment = GSNEnv sCEnv sEnv' }


-- * Node options.

-- | Configurable options for a node.
data NodeOptions = NodeOptions {
      nodeOptionsGenerator :: Maybe DocGenerator  -- ^ Optional document generator.
    , nodeOptionsReference :: Maybe Reference     -- ^ Optional citation references.
    }

instance Default NodeOptions where
    def = NodeOptions Nothing Nothing

-- | Construct a `NodeOptions`.
mkNodeOptions :: DocGenerator -> Reference -> NodeOptions
mkNodeOptions g r = NodeOptions (Just g) $ Just r

-- | Set the document generator of a `NodeOptions`.
setNodeOptionsGenerator :: NodeOptions -> DocGenerator -> NodeOptions
setNodeOptionsGenerator o dg = o {nodeOptionsGenerator = Just dg}

-- | Set the citation reference of a `NodeOptions`.
setNodeOptionsReference :: NodeOptions -> Reference -> NodeOptions
setNodeOptionsReference o dg = o {nodeOptionsReference = Just dg}


-- * Internal functionality.

setContextValue' :: (Monad m) => Int -> SomeContext -> BuilderT m ()
setContextValue' nodeIdentifier c = do
    s <- get
    let (GSNEnv sCEnv sSEnv) = builderEnvironment s

    let cEnv'                = Map.insert nodeIdentifier (GSNEnvContext c) sCEnv
    put $ s { builderEnvironment = GSNEnv cEnv' sSEnv }
-- Must be a noun context? Must be created manually, not from policy (like forall)?

nextIdentifier :: Monad m => BuilderT m Int
nextIdentifier = do
    s <- get
    put $ s { builderStateNextIdentifier = builderStateNextIdentifier s + 1 }

    return $ builderStateNextIdentifier s

insertNode
    :: Monad m
    => NodeTyRepr n
    -> NodeContent c n
    -> NodeOptions
    -> BuilderT m (Node c n)
insertNode repr x NodeOptions{..} = do
    i <- nextIdentifier
    let n = Node repr (NodeId i) x (maybe dgEmpty id nodeOptionsGenerator) nodeOptionsReference 
    insertNode' n
    return n

insertNode' :: Monad m => Node c n -> BuilderT m ()
insertNode' n = do
    s <- get
    put $ s { builderStateNodes = SomeNode n : builderStateNodes s }

insertHiddenNode :: Monad m => SomeNode -> BuilderT m ()
insertHiddenNode n =
    modify $ \s -> s { builderStateHiddenNodes = n : builderStateHiddenNodes s }

addBoundNode :: Monad m => Node c Context -> Node (f c) Context -> BuilderT m ()
addBoundNode c cs = modify $ \s@BuilderState {..} -> s
    { builderBoundNodes = Map.insert (unNodeId $ nodeId cs)
                                     (unNodeId $ nodeId c)
                                     builderBoundNodes
    }

insertEdge
    :: (ValidEdge n1 n2, Monad m) => Node c n1 -> Node d n2 -> BuilderT m ()
insertEdge n1 n2 = do
    i <- nextIdentifier

    insertEdge' $ mkEdge i n1 n2

insertEdge' :: Monad m => Edge c d e -> BuilderT m ()
insertEdge' e = do
    s <- get
    put $ s { builderStateEdges = SomeEdge e : builderStateEdges s }

mkGraph :: [SomeNode] -> [SomeNode] -> [SomeEdge] -> Map.Map Int Int -> GSNGraph
mkGraph nodes hiddenNodes edges = GSNGraph g rn hn

  where
    hn = Map.fromList $ map (\n -> (_sNodeId n, n)) hiddenNodes
    g  = FGL.mkGraph (mkNodes nodes) (mkEdges edges)

    rn = map _sNodeId $ filter
        (\(SomeNode (Node _ uid _ _ _)) -> FGL.indeg g (unNodeId uid) == 0)
        nodes

    mkNodes = map (\n@(SomeNode (Node _ uid _ _ _)) -> (unNodeId uid, n))

    mkEdges :: [SomeEdge] -> [FGL.LEdge ELabel]
    mkEdges = map toLEdge
      where
        toLEdge :: SomeEdge -> FGL.LEdge ELabel
        toLEdge (SomeEdge (Edge repr srcdst _uid lab)) = case repr of
            InContextOfRepr ->
                (edgeSrc srcdst, edgeDst srcdst, (InContextOf, lab))
            SupportedByRepr ->
                (edgeSrc srcdst, edgeDst srcdst, (SupportedBy, lab))

