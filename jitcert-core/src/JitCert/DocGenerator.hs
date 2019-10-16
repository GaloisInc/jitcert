module JitCert.DocGenerator where

import qualified Data.Text.Lazy                as T

import           JitCert.DocGenerator.Types

-- | For when one boundary is contained in another.
dgInBound :: T.Text -> T.Text -> DocGenerator
dgInBound inner outer =
  dgParagraph $ inner <> " is wholly contained inside " <> outer <> "."

-- | For caveats pertaining to nested software modules
dgCaveatIn
  :: T.Text   -- ^ The name of the nested thing, e.g. software module
  -> T.Text   -- ^ The boundary it is nested in
  -> T.Text   -- ^ The location of the documented caveat
  -> DocGenerator
dgCaveatIn mod physb doc = dgParagraph
  $  "Any " <> mod
  <> " which is nested inside " <> physb
  <> " must have a caveat in " <> doc <> "."

-- | For generating a list of things pertaining to an argument
dgArgOver :: [T.Text] -> DocGenerator
dgArgOver ts = DocGenerator $ \docs ->
  (Txt "The following argument must hold for each of:", Concat) :
  (List (map Txt ts), Concat) :
  concat docs

-- | When a container does not include a particular element.
dgNoEmbedded
  :: T.Text   -- ^ The thing not inside the container
  -> T.Text   -- ^ The container
  -> DocGenerator
dgNoEmbedded thing container =
  dgParagraph' $ thing <> " is not nested inside " <> container <> "."

-- | When a container does contain a particular element.
dgYesEmbedded
  :: T.Text   -- ^ The thing nested in the container
  -> T.Text   -- ^ The container itself
  -> DocGenerator
dgYesEmbedded thing container =
  dgParagraph' $ thing <> " is nested inside " <> container <> "."

dgYesEmbeddedMany
  :: [T.Text]   -- ^ The elements nested in the container
  -> T.Text     -- ^ The container
  -> DocGenerator
dgYesEmbeddedMany elems container = DocGenerator $ \docs ->
    (Txt $ "All of the following elements are nested in " <> container, Concat)
  : (List (map Txt elems), Concat)
  : concat docs

-- | Documentation where (flattenable) child nodes are list elements.
dgListHeader :: T.Text -> DocGenerator
dgListHeader t = DocGenerator $ \docs ->
  let (fs,cs) = splitByTypeMany docs
  in (Paragraph [Txt t], Concat) : (List (dropTypes fs), Concat) : cs

dgListHeader' :: T.Text -> DocGenerator
dgListHeader' t = DocGenerator $ \docs ->
  let (fs,cs) = splitByTypeMany docs
  in (Paragraph [Txt t], Flatten) : (List (dropTypes fs), Flatten) : cs

-- | Documentation for AND nodes
dgAnd :: DocGenerator
dgAnd = dgListHeader "All of the following must hold."


-- | Documentation for OR nodes
dgOr :: DocGenerator
dgOr = dgListHeader "One of the following must hold."


-- | Documentation for negation
dgNot :: DocGenerator
dgNot = dgListHeader "The following must not hold."

-- | Inject some custom text
dgText :: T.Text -> DocGenerator
dgText t = DocGenerator $ \docs -> (Txt t, Flatten) : concat docs

-- | Some custom text, and do not process child nodes
dgConst :: T.Text -> DocGenerator
dgConst t = DocGenerator $ const [(Txt t, Concat)]

-- | Generate no documentation for this node, but process children.
dgEmpty :: DocGenerator
dgEmpty = DocGenerator concat

-- | Wrap this text, and (flattenable) child nodes, into a paragraph
dgParagraph :: T.Text -> DocGenerator
dgParagraph t = DocGenerator $ \docs ->
  let (fs,cs) = splitByTypeMany docs
  in (Paragraph (Txt t : dropTypes fs), Concat) : cs

dgParagraph' :: T.Text -> DocGenerator
dgParagraph' t = DocGenerator $ \docs ->
  let (fs,cs) = splitByTypeMany docs
  in (Paragraph (Txt t : dropTypes fs), Flatten) : cs


-- | Documentation for manual inspection of a thing.
dgVerifyCheck :: T.Text -> DocGenerator
dgVerifyCheck doc = dgText $ "This should be verified by inspecting " <> doc <> "."


dgContains :: T.Text -> T.Text -> DocGenerator
dgContains outer inner = dgParagraph' $ outer <> " contains " <> inner <> "."

--------------------------------------------------------------------------------
-- Utilities and hardcoded strings

appendAccordingArg :: T.Text -> T.Text
appendAccordingArg = flip T.append accArg

accArg :: T.Text
accArg = " according to the following argument.\n"

accArgOr :: T.Text
accArgOr = accArg <> "One of the following holds.\n"

wellDef :: T.Text
wellDef = " is well-defined according to the requirements.\n"

it :: T.Text
it = "it"

dropTypes :: [TDoc] -> [Doc]
dropTypes = map fst

splitByType :: [TDoc] -> ([TDoc], [TDoc])
splitByType = foldr assignBucket ([],[])
  where
    assignBucket x@(_, Flatten) (fs,cs) = (x:fs, cs)
    assignBucket x@(_, Concat)  (fs,cs) = (fs, x:cs)

splitByTypeMany :: [[TDoc]] -> ([TDoc], [TDoc])
splitByTypeMany = mergePairwise . map splitByType
  where
    mergePairwise :: [([a], [b])] -> ([a], [b])
    mergePairwise = foldr (\(a,b) (as,bs) -> (a ++ as, b ++ bs)) ([],[])
