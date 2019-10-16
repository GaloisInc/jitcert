{-|

Description: Experimental parsing assurance cases from source code

This module was an experiment in parsing assurance cases, in the form of YAML
annotations, from source code.  It is unfinished and currently not interesting.
The idea was to explore integrating assurance cases directly with the code in
order to do some machine checking during CI/CD.

-}

{-# LANGUAGE DeriveGeneric #-}

module JitCert.SpecificationLang where

import qualified Control.Monad                 as CM
import qualified Control.Monad.IO.Class        as MIO
import qualified Data.ByteString               as BS
import qualified Data.ByteString.UTF8          as BS
import qualified Data.List                     as List
import qualified Data.Maybe                    as DM
import qualified Data.Text.Lazy                as T
import qualified Data.Yaml                     as Yaml
import qualified GHC.Generics                  as G

import           JitCert.Context
import           JitCert.GSN.Builder
import           JitCert.GSN.Types
import qualified JitCert.FIPS.Combinators
                                               as O
import           JitCert.Properties
import           JitCert.Solution

-- Currently this just abuses variable names to shove some of the values into
-- the resulting graph.  Obviously we should generate actual fresh variable
-- names and set the evidence, etc. appropriately.

-- This is a hack to get the generic FromJSON instance to let me use multiple
-- types because I don't want to write my own.
data FipsAnnotation = FipsAnnotation
  { fnAnnotation :: Maybe FunctionAnnotation
  , modAnnotation  :: Maybe ModuleAnnotation
  , lineAnnotation  :: Maybe LineAnnotation
  , swAnnotation :: Maybe SoftwareAnnotation
  , testAnnotation :: Maybe TestSuiteAnnotation
  } deriving (Eq, G.Generic, Show)

instance Yaml.FromJSON FipsAnnotation

type Name = String
type Type = String

data SoftwareAnnotation = SoftwareAnnotation
  { swName :: Name
  , swOS :: Name
  , swModules :: [ModuleAnnotation]
  , swFiles :: [Name]
  , swTestSuites :: [TestSuiteAnnotation]
  }
  deriving (Eq, G.Generic, Show)

instance Yaml.FromJSON SoftwareAnnotation

data TestSuiteAnnotation = TestSuiteAnnotation
  { tsFeatures :: [Name]
  , tsEnvironment :: Maybe String
  }
  deriving (Eq, G.Generic, Show)

instance Yaml.FromJSON TestSuiteAnnotation

data LineAnnotation = LineAnnotation
  { lnFeature :: Name
  , lnNumber :: Int
  }
  deriving (Eq, G.Generic, Show)

instance Yaml.FromJSON LineAnnotation

type Boundary = String

data ModuleAnnotation = ModuleAnnotation
  { modName :: Name
  , modLanguage :: Name
  , modLicense :: Maybe String
  , modAuthors :: [Name]
  , modBoundary :: [Boundary]
  , modVersion :: Int
  }
  deriving (Eq, G.Generic, Show)

instance Yaml.FromJSON ModuleAnnotation

data FunctionAnnotation = FunctionAnnotation
  { fnName :: String
  , fnTestSuite :: Maybe TestSuiteAnnotation
  , fnFeature :: Maybe Name
  , fnParameters :: [(Type, Name)]
  , fnReturn :: Maybe Type
  , fnCalls :: [Name]
  } deriving (Eq, G.Generic, Show)

instance Yaml.FromJSON FunctionAnnotation

parseSourceFile :: FilePath -> IO (Maybe (BuilderT IO (Node c Goal)))
parseSourceFile fp = do
  contents <- readFile fp
  let chunks =
        BS.concat $ map (BS.fromString . unlines) . groupIntoChunks $ lines
          contents
  let fs :: Either Yaml.ParseException [FipsAnnotation]
      fs = Yaml.decodeEither' chunks
  case fs of
    Left  err -> print err >> return Nothing
    Right fs  -> do
      print fs
      return $ Just $ mergeAll (map annotationToGSN fs)

trim :: String -> String
trim = dropWhile (== ' ')

isSpecPrefix :: String -> Bool
isSpecPrefix = List.isPrefixOf "//-|" . trim

groupIntoChunks :: [String] -> [[String]]
groupIntoChunks [] = []
groupIntoChunks xs =
  let chunk = takeWhile isSpecPrefix xs
      rest  = dropWhile (not . isSpecPrefix) $ dropWhile isSpecPrefix xs
  in  if not (null chunk)
        then map (drop 5 . trim) chunk : groupIntoChunks rest
        else groupIntoChunks rest

annotationToGSN :: MIO.MonadIO m => FipsAnnotation -> BuilderT m (Node c Goal)
annotationToGSN (FipsAnnotation (Just fn) _ _ _ _) = fnAnnotationToGSN fn
annotationToGSN (FipsAnnotation _ (Just mod) _ _ _) = modAnnotationToGSN mod
annotationToGSN (FipsAnnotation _ _ (Just ln) _ _) = lnAnnotationToGSN ln
annotationToGSN (FipsAnnotation _ _ _ (Just sw) _) = swAnnotationToGSN sw
annotationToGSN (FipsAnnotation _ _ _ _ (Just tst)) = tstAnnotationToGSN tst
annotationToGSN _ = error "bad assumptions in annotation scraper"

mergeAll
  :: MIO.MonadIO m => [BuilderT m (Node c Goal)] -> BuilderT m (Node c Goal)
mergeAll gs = do
  mergeGoal <- goalAnd [SFText "annotations are correct"]
  mapM_
    (\bg -> do
      g <- bg
      addEdge mergeGoal g
    )
    gs
  return mergeGoal

fnAnnotationToGSN
  :: MIO.MonadIO m => FunctionAnnotation -> BuilderT m (Node c Goal)
fnAnnotationToGSN (FunctionAnnotation name mtest mfeat pars mret calls) = do
  cfun    <- context @Function (stringToVar name) Nothing
  cpars   <- context @ParameterList (stringToVar $ show pars) Nothing
  ccalls  <- context @[Function] (stringToVar $ show calls) Nothing

  topGoal <- goalAnd [SFContext cfun, SFText "is correctly annotated"]

  gpar    <- goalAnd [SFContext cfun, SFText "has parameters", SFContext cpars]
  snpar   <- solution (ManualInspection cpars (O.ArePartOf cfun))
  addEdge gpar    snpar
  addEdge gpar    cpars
  addEdge topGoal gpar

  CM.when (DM.isJust mret) $ do
    let ret = DM.fromJust mret
    cret  <- context @ParameterList (stringToVar $ show ret) Nothing
    gret  <- goalAnd [SFContext cfun, SFText "returns", SFContext cret]
    snret <- solution (ManualInspection cret (O.ArePartOf cfun))
    addEdge topGoal gret
    addEdge gret    cfun
    addEdge gret    cret
    addEdge gret    snret
    return ()

  gcalls  <- goalAnd [SFContext cfun, SFText "calls", SFContext ccalls]
  sncalls <- solution (ManualInspection ccalls (O.ArePartOf cfun))
  addEdge gcalls  cfun
  addEdge gcalls  ccalls
  addEdge gcalls  sncalls
  addEdge topGoal gcalls

  CM.when (DM.isJust mfeat) $ do
    let feat = DM.fromJust mfeat
    cfeat  <- context @Feature (stringToVar feat) Nothing
    gfeat  <- goalAnd [SFContext cfun, SFText "implements", SFContext cfeat]
    snfeat <- solution (ManualInspection cfeat (O.ArePartOf cfun))
    addEdge topGoal gfeat
    addEdge gfeat   cfun
    addEdge gfeat   cfeat
    addEdge gfeat   snfeat
    return ()

  CM.when (DM.isJust mtest) $ do
    let test = DM.fromJust mtest
    ctest  <- context @Test (stringToVar (show test)) Nothing
    gtest  <- goalAnd [SFContext cfun, SFText "is tested by", SFContext ctest]
    sntest <- solution (Tests ctest cfun IsCorrect)
    addEdge topGoal gtest
    addEdge gtest   ctest
    addEdge gtest   sntest
    return ()

  return topGoal

data Function
instance RenderContext Function where
  renderContextTypeReference _ = "function"
  renderContext _ = ""

data Feature
instance RenderContext Feature where
  renderContextTypeReference _ = "feature"
  renderContext _ = ""

data ParameterList
instance RenderContext ParameterList where
  renderContextTypeReference _ = "parameter list"
  renderContext _ = ""

modAnnotationToGSN
  :: MIO.MonadIO m => ModuleAnnotation -> BuilderT m (Node c Goal)
modAnnotationToGSN (ModuleAnnotation mod lang _ auths bounds ver) = do
  cmod    <- context @Module (stringToVar mod) Nothing
  clang   <- context @Language (stringToVar lang) Nothing
  cauths  <- context @Authors (stringToVar (show auths)) Nothing
  cbounds <- context @Boundaries (stringToVar (show bounds)) Nothing
  cver    <- context @Version (stringToVar (show ver)) Nothing

  topGoal <- goalAnd [SFContext cmod, SFText "is annotated"]
  addEdge topGoal cmod

  glang <- goalAnd
    [SFContext cmod, SFText "is implemented using", SFContext clang]
  snlang <- solution (ManualInspection cmod (clang `O.UsedToImplement` cmod))
  addEdge topGoal clang
  addEdge topGoal glang
  addEdge glang   snlang

  gauths  <- goalAnd [SFContext cauths, SFText "authored", SFContext cmod]
  snauths <- solution (ManualInspection cmod (cmod `O.AuthoredBy` cauths))
  addEdge topGoal cauths
  addEdge topGoal gauths
  addEdge gauths  snauths

  gbounds  <- goalAnd [SFContext cmod, SFText "contained in", SFContext cbounds]
  snbounds <- solution (ManualInspection cmod (O.AreIn cbounds))
  addEdge topGoal gbounds
  addEdge topGoal cbounds
  addEdge gbounds snbounds

  gver  <- goalAnd [SFContext cmod, SFText "is version", SFContext cver]
  snver <- solution (ManualInspection cmod (cmod `O.IsVersion` cver))
  addEdge topGoal cver
  addEdge topGoal gver
  addEdge gver    snver

  return topGoal

data Module
instance RenderContext Module where
  renderContextTypeReference _ = "module"
  renderContext _ = ""

data Language
instance RenderContext Language where
  renderContextTypeReference _ = "programming language"
  renderContext _ = ""

data Boundaries
instance RenderContext Boundaries where
  renderContextTypeReference _ = "boundaries"
  renderContext _ = ""

data Authors
instance RenderContext Authors where
  renderContextTypeReference _ = "authors"
  renderContext _ = ""

data Version = Version Int
instance RenderContext Version where
  renderContextTypeReference _ = "version"
  renderContext (Version x) = T.pack (show x)

lnAnnotationToGSN :: MIO.MonadIO m => LineAnnotation -> BuilderT m (Node c Goal)
lnAnnotationToGSN (LineAnnotation feat num) = do
  cfeat   <- context @Feature (stringToVar (show feat)) Nothing
  cnum    <- context @LineNumber (stringToVar (show num)) Nothing

  topGoal <- goalAnd [SFContext cnum, SFText "is annotated"]

  gfeat   <- goalAnd [SFContext cnum, SFText "implements", SFContext cfeat]
  snfeat  <- solution (ManualInspection cnum (cnum `O.UsedToImplement` cfeat))

  addEdge topGoal gfeat
  addEdge topGoal cfeat
  addEdge topGoal cnum
  addEdge gfeat   snfeat
  return topGoal

data LineNumber
instance RenderContext LineNumber where
  renderContextTypeReference _ = "line number"
  renderContext _ = ""

swAnnotationToGSN
  :: MIO.MonadIO m => SoftwareAnnotation -> BuilderT m (Node c Goal)
swAnnotationToGSN (SoftwareAnnotation _ _ _ _ _) =
  goalAnd []

tstAnnotationToGSN
  :: MIO.MonadIO m => TestSuiteAnnotation -> BuilderT m (Node c Goal)
tstAnnotationToGSN (TestSuiteAnnotation _ _) =
  goalAnd []

stringToVar :: String -> Variable
stringToVar = Variable . T.pack
