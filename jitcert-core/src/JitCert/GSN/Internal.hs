module JitCert.GSN.Internal where

import qualified Data.Graph.Inductive          as FGL
import           Data.Maybe                     ( fromMaybe )
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.Text.Lazy                as T
import           Type.Reflection

import           JitCert.DocGenerator
import           JitCert.GSN.Types

--------------------------------------------------------------------------------
-- Utilities

mkEdge
    :: forall c d t1 t2
     . ValidEdge t1 t2
    => Int
    -> Node c t1
    -> Node d t2
    -> Edge c d (NodesToEdgeType t1 t2)
mkEdge i n1 n2 = Edge (edgeTypeRepr (Proxy @t1) (Proxy @t2))
                      (edgeConst (nodeId n1) (nodeId n2))
                      i
                      ""

edgeSrc :: SrcDst c d ty -> Int
edgeSrc (ContGC  (NodeId s) _) = s
edgeSrc (ContGA  (NodeId s) _) = s
edgeSrc (ContGJ  (NodeId s) _) = s
edgeSrc (ContSC  (NodeId s) _) = s
edgeSrc (ContSA  (NodeId s) _) = s
edgeSrc (ContSJ  (NodeId s) _) = s
edgeSrc (SuppGG  (NodeId s) _) = s
edgeSrc (SuppGS  (NodeId s) _) = s
edgeSrc (SuppGSn (NodeId s) _) = s
edgeSrc (SuppSG  (NodeId s) _) = s

edgeDst :: SrcDst c d ty -> Int
edgeDst (ContGC  _ (NodeId d)) = d
edgeDst (ContGA  _ (NodeId d)) = d
edgeDst (ContGJ  _ (NodeId d)) = d
edgeDst (ContSC  _ (NodeId d)) = d
edgeDst (ContSA  _ (NodeId d)) = d
edgeDst (ContSJ  _ (NodeId d)) = d
edgeDst (SuppGG  _ (NodeId d)) = d
edgeDst (SuppGS  _ (NodeId d)) = d
edgeDst (SuppGSn _ (NodeId d)) = d
edgeDst (SuppSG  _ (NodeId d)) = d


goal :: Int -> NodePolicy c -> Sentence -> Node c Goal
goal i n s = Node GoalRepr (NodeId i) (NodePolicyContent n s) dgEmpty Nothing

sgoal :: Int -> NodePolicy c -> Sentence -> SomeNode
sgoal i n s = SomeNode $ goal i n s

solution :: (Typeable c, SolutionType c) => Int -> c -> Node c Solution
solution uid c = Node SolutionRepr (NodeId uid) (SolutionContent c) dgEmpty Nothing

-- ssolution :: Int -> Sentence -> SomeNode
-- ssolution uid s = SomeNode ( solution uid s :: Node c Solution)

strategy :: Int -> NodePolicy c -> Sentence -> Node c Strategy
strategy i n s = Node StrategyRepr (NodeId i) (NodePolicyContent n s) dgEmpty Nothing

sstrategy :: Int -> NodePolicy c -> Sentence -> SomeNode
sstrategy i n s = SomeNode $ strategy i n s

-- TODO: Ensure variable is unique?
-- context :: Int -> Variable -> c -> Node c Context
context :: RenderContext c => Int -> Variable -> Maybe T.Text -> Node c Context
context uid v d = Node ContextRepr (NodeId uid) (ContextNoun v d) dgEmpty Nothing

scontext
    :: forall c
     . RenderContext c
    => Int
    -> Variable
    -> Maybe T.Text
    -> Proxy c
    -> SomeNode
scontext uid v d _ = SomeNode $ context @c uid v d

contextVerb
    :: (Typeable c, RenderContext c)
    => Int
    -> T.Text
    -> (c -> Bool)
    -> Node (c -> Bool) Context
contextVerb uid s v = Node ContextRepr (NodeId uid) (ContextVerb s v) dgEmpty Nothing

scontextVerb :: (Typeable c, RenderContext c) => Int -> T.Text -> (c -> Bool) -> SomeNode
scontextVerb uid s v = SomeNode $ contextVerb uid s v

assumption :: Int -> Sentence -> Node c Assumption
assumption i s = Node AssumptionRepr (NodeId i) s dgEmpty Nothing

sassumption :: Int -> Sentence -> SomeNode
sassumption = (SomeNode .) . assumption

justification :: Int -> Sentence -> Node c Justification
justification i s = Node JustificationRepr (NodeId i) s dgEmpty Nothing

sjustification :: Int -> Sentence -> SomeNode
sjustification = (SomeNode .) . justification


contextVariable :: Node c Context -> Variable
contextVariable (Node ContextRepr _ (ContextNoun v _) _ _) = v
contextVariable (Node ContextRepr _ (ContextVerb _ _) _ _) =
    error "contextVariable: unreachable"

type ProxyId a b = b

-- Get parent nodes or throw exception.
getParentNodes :: FGL.Gr a b -> Int -> [a]
getParentNodes gsnGraph nid = map fst $ getLabeledParentNodes gsnGraph nid

getLabeledParentNodes :: FGL.Gr a b -> Int -> [(a, b)]
getLabeledParentNodes gsnGraph nid = justOrMalformed
    $ mapM (\(a, b) -> (, b) <$> FGL.lab gsnGraph a) (FGL.lpre gsnGraph nid)

justOrMalformed :: Maybe a -> a
justOrMalformed = fromMaybe (error "Malformed GSN")

lookupNode :: FGL.Gr a b -> Int -> a
lookupNode = (justOrMalformed .) . FGL.lab

-- Get child nodes or throw exception.
getChildNodes :: FGL.Gr a b -> Int -> [a]
getChildNodes gsnGraph nid = map fst $ getLabeledChildNodes gsnGraph nid

-- Get labeled child nodes or throw exception.
getLabeledChildNodes :: FGL.Gr a b -> Int -> [(a, b)]
getLabeledChildNodes gsnGraph nid = justOrMalformed
    $ mapM (\(a, b) -> (, b) <$> FGL.lab gsnGraph a) (FGL.lsuc gsnGraph nid)

