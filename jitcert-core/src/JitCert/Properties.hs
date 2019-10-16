module JitCert.Properties where

import           JitCert.GSN.Types

data IsCorrect = IsCorrect

instance Property IsCorrect where
    renderProperty IsCorrect = [SFText "is correct"]

data IsSafe = IsSafe

instance Property IsSafe where
    renderProperty IsSafe = [SFText "is safe"]

data IsDocumented = IsDocumented
-- JP: IsDocumentedIn ???

instance Property IsDocumented where
    renderProperty IsDocumented = [SFText "is documented"]

data Passes = Passes

instance Property Passes where
    renderProperty Passes = [SFText "passes"]

