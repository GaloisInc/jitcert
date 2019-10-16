module JitCert.Logic where

import qualified Data.Char                     as Char
import qualified Data.List                     as List
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.Text.Lazy                as T
import           Data.Text.Prettyprint.Doc
import qualified Text.Casing                   as Casing

import           JitCert.GSN
import           JitCert.GSN.Internal
import           JitCert.Internal

newtype Name = Name T.Text

data GSNLogic = GSNLogic [Dec]

data Dec =
      FuncD Name [Pat] Expr [Dec]
    | TypeD Name Type

data Type =
      VarT Name
    | ConT Name
    | AppT Type Type
    | ArrowT Type Type -- Make a constructor?

data Pat =
      VarP Name

data Lit =
      BoolL Bool
    | IntL Int

data Expr =
      VarE Name
    | LitE Lit
    | AppE Expr Expr
    | LamE [Pat] Expr
    | IfE Expr Expr Expr
    | IntrosE [Pat] Expr

instance Pretty Name where
    pretty (Name s) = pretty s

instance Pretty GSNLogic where
    pretty (GSNLogic ds) = vsep $ map pretty ds

instance Pretty Pat where
    pretty (VarP n) = pretty n
    prettyList ps = hsep $ map pretty ps

instance Pretty Expr where
    pretty (VarE n    ) = pretty n
    pretty (LitE l    ) = pretty l
    pretty (AppE e1 e2) = "(" <> pretty e1 <+> pretty e2 <> ")"
    pretty (LamE ps e ) = "(λ" <> pretty ps <+> "->" <+> pretty e <> ")"
    pretty (IfE e1 e2 e3) =
        "if" <+> pretty e1 <+> "then" <+> pretty e2 <+> "else" <+> pretty e3
    pretty (IntrosE ps e) = "(intros" <+> pretty ps <+> "." <+> pretty e <> ")"

instance Pretty Lit where
    pretty (BoolL b) = pretty b
    pretty (IntL  i) = pretty i

instance Pretty Dec where
    pretty (TypeD n t) = pretty n <+> "::" <+> pretty t
    pretty (FuncD n ps e wheres) =
        let fd = pretty n <+> pretty ps <+> "=" <+> pretty e <+> "\n"
        in
            case wheres of
                [] -> fd
                _ ->
                    nest 2 $ vsep
                        [fd, nest 2 (vsep $ "where\n" : map pretty wheres)]

instance Pretty Type where
    pretty (VarT n      ) = pretty n
    pretty (ConT n      ) = pretty n
    pretty (AppT   t1 t2) = "(" <> pretty t1 <+> pretty t2 <> ")"
    pretty (ArrowT t1 t2) = "(" <> pretty t1 <+> "->" <+> pretty t2 <> ")"

gsnToLogic :: GSNGraph -> GSNLogic
gsnToLogic GSNGraph {..} = GSNLogic $ concatMap someNodeToDec gsnRootNodes'

  where
    gsnRootNodes' = map (lookupNode gsnGraph) gsnRootNodes

    someNodeToDec :: SomeNode -> [Dec]
    someNodeToDec (SomeNode n) = nodeToDec n

    nodeToDec :: Node a t -> [Dec]
    nodeToDec n@(Node GoalRepr (NodeId nid) c _ _) =
        goalOrStrategyToDec (SomeNode n) nid $ nodePolicyContentToNodePolicy c

    nodeToDec (Node StrategyRepr      _ _ _ _) = [] -- TODO
    nodeToDec (Node ContextRepr       _ _ _ _) = []
    nodeToDec (Node AssumptionRepr    _ _ _ _) = []
    nodeToDec (Node SolutionRepr      _ _ _ _) = []
    nodeToDec (Node JustificationRepr _ _ _ _) = []

    goalOrStrategyToDec :: SomeNode -> Int -> NodePolicy c -> [Dec]
    goalOrStrategyToDec sn nid p =
        -- Get child nodes.
        let cns' = getChildNodes gsnGraph nid
        in

        -- Split context nodes.
            let (cs, ns) = List.partition isContextNode cns'
            in
                let (cns, _cvs) = List.partition isNounContextNode cs
                in

                -- Introduce child noun context nodes.
                    let pats = map contextNodeToPat cns
                    in

                    -- Add child verb context nodes.
                    -- Recursively convert child nodes to exprs.
                        let cnes = map someNodeToExpr ns
                        in

                            let wheres = concatMap someNodeToDec ns
                            in

                                let nName = nodeToName sn
                                in
                                    let
                                        bDec = FuncD nName
                                                     pats
                                                     (policyToExpr p cnes)
                                                     wheres
                                    in

                                        let
                                            typ = List.foldr
                                                (\n t -> ArrowT
                                                    (contextNodeToType n)
                                                    t
                                                )
                                                (VarT $ Name "Bool")
                                                cns
                                        in  let tDec = TypeD nName typ
                                            in  [tDec, bDec]



    contextNodeToType :: SomeNode -> Type
    contextNodeToType (SomeNode c@(Node ContextRepr _ (ContextNoun _ _) _ _)) =
        -- VarT $ Name $ T.pack $ show $ typeOf c
        VarT
            $ Name
            $ headToUpper
            $ toCamel
            $ renderContextTypeReference
            $ nodeToProxy c
        -- TODO: Improve this. Use Haskell type?
    contextNodeToType _ = error "contextNodeToType: unreachable?"

    nodeToProxy :: Node c Context -> Proxy c
    nodeToProxy _ = Proxy


    contextNodeToPat :: SomeNode -> Pat
    contextNodeToPat s@(SomeNode (Node ContextRepr _ _ _ _)) =
        VarP $ nodeToName s
    contextNodeToPat _ = error "contextNodeToPat: unreachable?"


    infixExprs _  []       = LitE $ BoolL True
    infixExprs _  [e     ] = e
    infixExprs op (e : es) = AppE (AppE op e) $ infixExprs op es

    policyToExpr PolicyAnd es = infixExprs (VarE (Name "&&")) es
    policyToExpr PolicyOr  es = infixExprs (VarE (Name "||")) es
    policyToExpr (PolicyNot p) es =
        AppE (VarE (Name "not")) $ policyToExpr p es
    policyToExpr (PolicyForall c cs p) es = AppE
        (AppE (VarE (Name "forall"))
              (LamE [contextNodeToPat (SomeNode c)] (policyToExpr p es))
        )
        (VarE $ nodeToName $ SomeNode cs)
    policyToExpr (PolicyWhen c v p) es = IfE
        (AppE (VarE $ nodeToName $ SomeNode v) (VarE $ nodeToName $ SomeNode c))
        (policyToExpr p es)
        (LitE $ BoolL True)


    someNodeToExpr :: SomeNode -> Expr
    someNodeToExpr (SomeNode n) = nodeToExpr n


    nodeToExpr :: Node a t -> Expr
    -- nodeToExpr n@(Node SolutionRepr _ _) = VarE $ nodeToName n -- JP: Should these take sentence references as arguments?
    nodeToExpr n@(Node SolutionRepr _ _ _ _) =
        solutionOrJustificationOrAssumptionToExpr $ SomeNode n
    nodeToExpr n@(Node JustificationRepr _ _ _ _) =
        solutionOrJustificationOrAssumptionToExpr $ SomeNode n
    nodeToExpr n@(Node AssumptionRepr _ _ _ _) =
        solutionOrJustificationOrAssumptionToExpr $ SomeNode n

    nodeToExpr n@(Node GoalRepr (NodeId nid) _ _ _) =
        goalOrStrategyToExpr (SomeNode n) nid
    nodeToExpr n@(Node StrategyRepr (NodeId nid) _ _ _) =
        goalOrStrategyToExpr (SomeNode n) nid

    nodeToExpr (Node ContextRepr _ _ _ _) = error "nodeToExpr: unreachable?"

    solutionOrJustificationOrAssumptionToExpr sn =
        let refs = nodesReferencedBy sn
        in 

        -- JP: Should we order the referenced nodes/arguments?
            let nName = VarE $ nodeToName sn
            in  List.foldl' (\acc r -> AppE acc (VarE $ nodeToName r))
                            nName
                            refs

    goalOrStrategyToExpr sn nid =
        -- Get children.
        let cns' = getChildNodes gsnGraph nid
        in

        -- Get context nodes.
            let cns = List.filter isNounContextNode cns'
            in

                let nName = VarE $ nodeToName sn
                in

                    let
                        bodyE = List.foldl'
                            (\acc n -> AppE acc (VarE $ nodeToName n))
                            nName
                            cns
                    in 

                    -- Introduce context noun nodes.
                        case cns of
                            [] -> bodyE
                            _  -> IntrosE (map (VarP . nodeToName) cns) bodyE





    -- TODO: Improve this.
    nodeToName (SomeNode (Node ContextRepr _ (ContextNoun (Variable v) _) _ _)) =
        Name v
    nodeToName s =
        Name
            $ toCamel
            $ T.filter (\c -> Char.isAlphaNum c || Char.isSpace c)
            $ someNodeIdToNodeDescription s

    headToUpper =
        T.pack
            . (\s -> case s of
                  []    -> []
                  h : t -> Char.toUpper h : t
              )
            . T.unpack

    toCamel = T.pack . Casing.camel . T.unpack

