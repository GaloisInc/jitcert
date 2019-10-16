module JitCert.GSNAnalysis where

import qualified Data.Graph.Inductive          as FGL
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
--import           Debug.Trace (traceShow)
import           Type.Reflection

import           JitCert.GSN
import           JitCert.GSN.Internal
import           JitCert.Internal

type ErrorMsg = String

runChecks :: GSNGraph -> SomeNode -> Either ErrorMsg ()
runChecks gsn node = foldl1
    concatError
    [checkAllowedLeafNodes gsn node, checkStrategyIsDeveloped gsn node]

concatError :: Either ErrorMsg () -> Either ErrorMsg () -> Either ErrorMsg ()
concatError (   Left  e1) (   Left  e2) = Left (e1 <> e2)
concatError e1@(Left  _ ) (   Right ()) = e1
concatError (   Right ()) e2@(Left  _ ) = e2
concatError (   Right ()) (   Right ()) = Right ()

getNodeInfo :: SomeNode -> String
getNodeInfo = show . someNodeIdToNodeDescriptionVerbose

prettyPrintEvaluation :: GSNGraph -> GSNEnv -> String
prettyPrintEvaluation g@GSNGraph{..} gsnEnv =
    let s = Map.toList (evaluateArgument g gsnEnv) in
        List.intercalate "\n" (map (\x -> show (getNodeInfo (fst x), snd x)) s)

type EvaluationMap = Map.Map SomeNode Bool

--TODO: ADI, collect errors
evaluateArgument :: GSNGraph -> GSNEnv -> (EvaluationMap)--, Set GSNError)
evaluateArgument GSNGraph{..} gsnEnv =
    visit [gsnEnv] Map.empty (map (lookupNode gsnGraph) gsnRootNodes)
    where

        foldOnChildren :: EvaluationMap -> (Bool -> Bool -> Bool) -> SomeNode -> Bool
        foldOnChildren nodeValMap op parentNode =
            if (List.null $ children parentNode)
               then False
               else (foldr1 op (catMaybes $ map (\x -> Map.lookup x nodeValMap) (children parentNode)))

        -- TODO: Cleanup all the undefine's
        getPred :: [GSNEnv] -> Node c Context -> Node (c -> Bool) Context -> Bool
        getPred envStack c f =
            let childEnvs = envStack in
                case f of
                      (Node ContextRepr _ (ContextVerb _ fPred) _ _) ->
                            case (gSNEnvContext childEnvs) of
                              Just (GSNEnvContext (SomeContext cc)) ->
                                  let fpt = typeOf fPred in

                                  case eqTypeRep (typeRepArg fpt) (typeOf cc) of
                                      Just HRefl -> fPred cc
                                      Nothing -> undefined
                              Just (GSNEnvContextEnv (SomeContext _) _) -> undefined
                              --TODO: This should return a Left GSNError
                              Nothing -> False

                      _ -> undefined

                where

                typeRepArg :: forall a b . Typeable a => TypeRep (a -> b) -> TypeRep a
                typeRepArg _ = typeRep

                gSNEnvContext :: [GSNEnv] -> Maybe GSNEnvContext
                gSNEnvContext chEnv =
                    --TODO: Remove the union and the envStack. Use a single env
                    --let env = foldr Map.union Map.empty (map gsnContextEnv chEnv) in
                    let env = foldr Map.union Map.empty (map gsnContextEnv chEnv) in
                        let contextNodeId = unNodeId $ nodeId c in
                            Map.lookup contextNodeId env
--                     case (Map.lookup contextNodeId env) of
--                       Just gsc -> gsc
--                       --TODO: This should return a Left GSNError
--                       Nothing -> trace("key: " ++ show contextNodeId ++ " not found in: " ++ show chEnv) Nothing

        getNodeMergedEnv :: SomeNode -> [GSNEnv]
        getNodeMergedEnv node = lookupNodeEnvironments (_sNodeId node) gsnGraph gsnEnv

        gsnEnvSolution :: forall st. SolutionType st => GSNEnv -> Node st Solution -> Bool
        gsnEnvSolution GSNEnv{..} s =
            let contextNodeId = unNodeId $ nodeId s in
            case (Map.lookup contextNodeId gsnSolutionEnv) of
              Just (SomeEvidence e) ->
                  let te = typeOf e in
                      let tse = typeRep :: TypeRep (SolutionEvidence st) in
                      case eqTypeRep tse te of
                          Just HRefl -> validEvidence e
                          Nothing -> False
              Nothing -> False


        visit :: [GSNEnv] -> EvaluationMap -> [SomeNode] -> EvaluationMap
        visit _ acc [] = acc
        visit envStack acc nodes = foldr (visitNode envStack) acc nodes

    -- TODO: ADI, if a variable is not defined in the environment, do not throw
    -- undefined. Instead throw an error saying value not found.
        visitNode :: [GSNEnv] -> SomeNode -> EvaluationMap -> EvaluationMap
        visitNode envStack node acc =
            --TODO:FIX
            --let acc' = visit (getNodeMergedEnv node) acc (children node)
            let acc' = visit envStack acc (children node)
                foldChildrenOR = foldOnChildren acc' (||) node
                foldChildrenAnd = foldOnChildren acc' (&&) node in
                case node of
                (SomeNode (Node GoalRepr _ (NodePolicyContent PolicyOr _) _ _)) ->
                    Map.insert node foldChildrenOR acc'
                (SomeNode (Node GoalRepr _ (NodePolicyOr) _ _)) ->
                    Map.insert node foldChildrenOR acc'

                (SomeNode (Node GoalRepr _ (NodePolicyContent PolicyAnd _) _ _)) ->
                    Map.insert node foldChildrenAnd acc'
                (SomeNode (Node GoalRepr _ NodePolicyAnd _ _)) ->
                    Map.insert node foldChildrenAnd acc'

                (SomeNode (Node GoalRepr _ (NodePolicyContent (PolicyForall _  _  _) _) _ _)) ->
                    let accs' =
                    --trace("Forall NODEID: " ++ show(_sNodeId node) ++ ", ENVS: " ++ show ((envStack)))
                            --TODO:FIX
                            --(map (\env->visit [env] acc (children node)) envStack) in
                            (map (\env->visit [env] acc (children node)) (getNodeMergedEnv node)) in
                        let acc' = foldr (Map.unionWith (&&)) mempty accs' in
                    Map.insert node (foldOnChildren acc' (&&) node) acc'

                (SomeNode (Node GoalRepr _ (NodePolicyContent (PolicyWhen c f PolicyAnd) _) _ _)) ->
                    Map.insert node nodeVal acc'
                    where
                        --TODO: figure out if we need the below
                        --predVal = getPred ((getNodeMergedEnv node) ++ envStack) c f
                        predVal = getPred (envStack) c f
                        nodeVal = if (predVal) then foldOnChildren acc' (&&) node else True

            -- strategy
                (SomeNode (Node StrategyRepr _ (NodePolicyContent PolicyOr _) _ _)) ->
                    Map.insert node foldChildrenOR acc'
                (SomeNode (Node StrategyRepr _ NodePolicyOr _ _)) ->
                    Map.insert node foldChildrenOR acc'

                (SomeNode (Node StrategyRepr _ (NodePolicyContent PolicyAnd _) _ _)) ->
                    Map.insert node foldChildrenAnd acc'
                (SomeNode (Node StrategyRepr _ NodePolicyAnd _ _)) ->
                    Map.insert node foldChildrenAnd acc'

                (SomeNode (Node StrategyRepr _ (NodePolicyContent (PolicyForall _ _ _) _) _ _)) ->
                    Map.insert node foldChildrenAnd acc'

                (SomeNode (Node StrategyRepr _ (NodePolicyContent (PolicyWhen _ _ _) _) _ _)) ->
                    Map.insert node nodeVal acc'
                    where
                        --acc' = visit acc (children node)
                        nodeVal = foldOnChildren acc' (&&) node

            -- Solution nodes
                (SomeNode sn@(Node SolutionRepr _ (SolutionContent _) _ _)) ->
                    Map.insert node evaluateSnNode acc
                        where evaluateSnNode = and (map (flip gsnEnvSolution sn) (envStack))

                -- do not recurse on other nodes
                node ->
                    Map.insert node True acc



        nid :: SomeNode -> Int
        nid node = _sNodeId node

        nodeIdsToSomeNodes = catMaybes . map (FGL.lab gsnGraph)

        children :: SomeNode -> [SomeNode]
        children node = nodeIdsToSomeNodes $ FGL.suc gsnGraph (nid node)

evaluationMapToErrors :: EvaluationMap -> Set GSNError
evaluationMapToErrors = Set.map WarningEvaluationFalse . Map.keysSet . Map.filter not

visitNodes :: GSNGraph -> Either ErrorMsg ()
visitNodes g@GSNGraph {..} = checkNodes gsnRootNodes' (Right ())

  where
    gsnRootNodes' = map (lookupNode gsnGraph) gsnRootNodes

    checkNodes :: [SomeNode] -> Either ErrorMsg () -> Either ErrorMsg ()
    checkNodes nodes acc = foldr checkNode acc nodes

    checkNode :: SomeNode -> Either ErrorMsg () -> Either ErrorMsg ()
    checkNode node acc =
        let nid = _sNodeId node
        in
        -- Get children.
            let children = nodeIdsToSomeNodes $ FGL.suc gsnGraph nid
            in  let acc' = concatError (runChecks g node) acc
                in
                -- Check children.
                    checkNodes children acc'

    nodeIdsToSomeNodes = mapMaybe (FGL.lab gsnGraph)

--leafNodes :: GSNGraph -> (GSNGraph -> SomeNode -> Bool) -> [SomeNode]
--leafNodes g@GSNGraph{..} filter = checkNodes gsnRootNodes mempty

filterNodes :: GSNGraph -> (GSNGraph -> SomeNode -> Bool) -> [SomeNode]
filterNodes g@GSNGraph {..} filter = checkNodes gsnRootNodes' mempty

  where
    gsnRootNodes' = map (lookupNode gsnGraph) gsnRootNodes

    checkNodes nodes acc = foldr checkNode acc nodes

    checkNode node acc =
        let nid = _sNodeId node
        in
        -- Get children.
            let children = nodeIdsToSomeNodes $ FGL.suc gsnGraph nid
            in
                        -- Run filter, add to acc if match.
                let acc' = if filter g node then node : acc else acc
                in

                                                           -- Check children.
                    checkNodes children acc'

    nodeIdsToSomeNodes = mapMaybe (FGL.lab gsnGraph)


-- predicates
isLeafNode :: GSNGraph -> SomeNode -> Bool
isLeafNode GSNGraph {..} node =
    case (nodeIdsToSomeNodes $ FGL.suc gsnGraph (_sNodeId node)) of
        [] -> True
        _  -> False
    where nodeIdsToSomeNodes = mapMaybe (FGL.lab gsnGraph)

--nodeIdsToSomeNodes :: [Int] -> [SomeNode]

isStratNode :: SomeNode -> Bool
isStratNode (SomeNode (Node StrategyRepr _ _ _ _)) = True
-- isStratNode (SomeNode (Node StrategyRepr _ (NodePolicyContent PolicyOr _, _) _)) =
--     True
-- isStratNode (SomeNode (Node StrategyRepr _ (NodePolicyOr, _) _)) = True -- JP: Do we only want OR nodes?
isStratNode (SomeNode _) = False


isGoalNode :: SomeNode -> Bool
isGoalNode (SomeNode (Node GoalRepr _ _ _ _)) = True
-- isGoalNode (SomeNode (Node GoalRepr _ (NodePolicyContent PolicyOr _, _) _)) = True
-- isGoalNode (SomeNode (Node GoalRepr _ (NodePolicyOr, _) _)) = True -- JP: Doe we only want OR nodes?
isGoalNode (SomeNode _) = False

getChildren :: GSNGraph -> SomeNode -> [SomeNode]
getChildren GSNGraph{..} node =  let childNodeIds = FGL.suc gsnGraph (_sNodeId node) in
                                            catMaybes $ map (FGL.lab gsnGraph) childNodeIds

-- predicates every node must satisfy

-- If node is a leafNode, it must satisfy
-- Allowed leaf nodes = {Solution, Context, Assumption, Justification}


----allChildrenAreOfType node childType = all (isNodeType childType) $ getChildren node

-- P => Q
data Rule = RulePQ {rname :: String, p :: SomeNode -> Bool, q :: SomeNode -> Bool}

--nodeTypeToText

checkPred :: SomeNode -> Rule -> Either ErrorMsg ()
checkPred node RulePQ {..} = if valid then (Right ()) else (Left errorMsg)

    where
        errorMsg = "error: " ++ rname ++ " failed on node: "++ (getNodeInfo node) ++ "\n"
        valid = if p node then q node else True

checkAllowedLeafNodes :: GSNGraph -> SomeNode -> Either ErrorMsg ()
checkAllowedLeafNodes g@GSNGraph {..} node = checkPred
    node
    (RulePQ "checkAllowedLeafNodes" (isLeafNode g) leafNodeOfAllowedType)
  where
        --q (SomeNode (Node x _ (_, _))) = x `elem` [SolutionRepr, ContextRepr, AssumptionRepr, JustificationRepr]
    leafNodeOfAllowedType (SomeNode (Node SolutionRepr _ _ _ _)) = True
    leafNodeOfAllowedType (SomeNode (Node ContextRepr _ _ _ _)) = True
    leafNodeOfAllowedType (SomeNode (Node AssumptionRepr _ _ _ _)) = True
    leafNodeOfAllowedType (SomeNode (Node JustificationRepr _ _ _ _)) = True
    leafNodeOfAllowedType _ = False

-- If a node is a strategy node, it must have a goal node as its child.
checkStrategyIsDeveloped :: GSNGraph -> SomeNode -> Either ErrorMsg ()
checkStrategyIsDeveloped g@GSNGraph {..} node = checkPred
    node
    (RulePQ "checkStrategyIsDeveloped" isStratNode q)
    where q node = any isGoalNode $ getChildren g node

-- Enforce uniqueness of nodes: hash for every node?

-- Recursion/cycles?

-- A goal can either have a solution as its child or a strategy, not both.

----checkGaol :: GSNGraph -> SomeNode -> Either [Text] ()
----checkGaol GSNGraph{..} node = checkPred (RulePQ (isGoalNode) (allChildrenAreOfType node Strat || (allChildrenAreOfType node Soln)))

-- A goal can have only one solution as its child.

-- check for undeveloped nodes/holes
