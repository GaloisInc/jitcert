module JitCert.Context where

import           JitCert.GSN.Types

import qualified Data.ByteString.Lazy          as BSL
import           Data.Digest.Pure.SHA
import           Data.Monoid                    ( (<>) )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Data.Text.Lazy                as T
import           System.FilePath                ( FilePath )
import qualified System.FilePath               as FP

newtype Files = Files (Set File)
    deriving (Eq, Ord)

type Hash = Digest SHA256State

data File = File {
      fileName :: FilePath       -- Name, Filepath?
    , fileDirectory :: FilePath
    , fileContent :: Hash
    }
    deriving (Eq, Ord)

instance RenderContext File where
    renderContextTypeReference Proxy = "file"

    renderContext File {..} = T.pack fileName

instance RenderContext Files where
    renderContextTypeReference Proxy = "files"

    renderContext (Files fs) = case ts of
        [] -> "[]"
        _  -> T.intercalate ", " ts -- JP: Just take the first 3?
        where ts = map renderContext $ Set.toList fs

instance RenderContext a => RenderContext [a] where
    renderContextTypeReference Proxy =
        "list of " <> renderContextTypeReference (Proxy @a)

    renderContext [] = "[]"
    renderContext ls = "[" <> T.intercalate ", " ts <> "]" -- JP: Just take the first 3?
        where ts = map renderContext ls

-- | Load a list of files given their filepath.
loadFiles :: [FilePath] -> IO Files
loadFiles fps = (Files . Set.fromList) <$> mapM loadFile fps

-- | Load a file context given its filepath.
loadFile :: FilePath -> IO File
loadFile fp = do
    -- Relative directory?
    let (directory, name) = FP.splitFileName fp

    -- Read file and hash contents.
    hash <- sha256 <$> BSL.readFile fp

    return $ File name directory hash

data Test = Test {
    testName :: T.Text
  , testContent :: Hash
  }
  deriving (Eq)

instance RenderContext Test where
    renderContextTypeReference Proxy = "test"
    renderContext (Test name _) = name

data Documentation = Documentation {
    documentationName :: T.Text
  , documentationContent :: Hash
  }

instance RenderContext Documentation where
    renderContextTypeReference Proxy = "documentation"
    renderContext (Documentation name _) = name