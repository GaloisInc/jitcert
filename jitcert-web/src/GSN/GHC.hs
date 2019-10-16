module GSN.GHC where

import Data.Dynamic
import Data.IORef
import DynFlags
import ErrUtils
import GHC
import GHC.Paths (libdir)
import HscTypes
import Language.Haskell.TH.LanguageExtensions
import Outputable (showSDoc)
import StringBuffer

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock
import JitCert.GSN
import Prelude

compileGSN :: MonadIO m => String -> m (Either [(SrcSpan, Text)] (GSNGraph, [(Int, Text, GSNEnv)]))
compileGSN source = liftIO $ do
    now <- getCurrentTime

    errorM <- newIORef Nothing

    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
        runGhcT (Just libdir) $ do
            -- TODO: Enable SafeHaskell to prevent unsafePerformIO!!!
            -- - Add timeout, resource limits
            -- TODO: Disable file level language extensions.
            
            dfs' <- getSessionDynFlags 
            let dfs = foldl xopt_set dfs' [OverloadedStrings, TypeFamilies, TypeApplications, DataKinds]
            _ <- setSessionDynFlags dfs {
              ghcLink = LinkInMemory
            , hscTarget = HscInterpreted
            , log_action = logAction errorM
            , safeHaskell = Sf_Safe
            -- , packageFlags = [ExposePackage "jitcert-core" (PackageArg "jitcert-core") (ModRenaming True [])]
            }

            let target = Target (TargetModule modName) True $ Just (stringToStringBuffer source, now)
            addTarget target
            -- setTargets [target]

            res <- load LoadAllTargets
            case res of
                Failed -> liftIO (readIORef errorM) >>= \case
                    Nothing -> return $ Left []
                    Just e -> return $ Left e
                Succeeded -> do
                    -- wgModule <- findModule modName Nothing

                    -- setContext [IIModule modName]
                    setContext [IIDecl $ simpleImportDecl modName]

                    handleSourceError (\errs -> 
                        return $ Left $ foldr (\e acc -> (errMsgSpan e, Text.pack (show e)):acc) [] (srcErrorMessages errs)
                      ) $ do
                        result <- dynCompileExpr "WebGSN.webGSN"

                        case fromDynamic result :: Maybe (GSNGraph, GSNEnv) of
                            Just (g,e) ->
                                
                                return $ Right (g, [(1, "Environment 1", e)])
                                
                            Nothing -> case fromDynamic result :: Maybe (GSNGraph, [GSNEnv]) of
                                Just (g,e) ->
                                    return $ Right (g, zip3 envIds envNames e)
                                Nothing -> case fromDynamic result :: Maybe (GSNGraph, [(Text, GSNEnv)]) of
                                    Just (g,e) ->
                                        return $ Right (g, map (\(a,(b,c)) -> (a,b,c)) $ zip envIds e)
                                    Nothing ->
                                        return $ Left [(noSrcSpan, "Invalid type for webGSN.")]
                            


    where
        modName = mkModuleName "WebGSN"

        envIds = [1..]
        envNames = ["Environment " <> Text.pack (show x) | x <- envIds]

        logAction errorRef flags _ _severity src _style msg = do
            let err = Text.pack $ showSDoc flags msg

            modifyIORef errorRef $ \case
                Just errs -> Just $ (src,err):errs
                Nothing -> Just [(src,err)]


