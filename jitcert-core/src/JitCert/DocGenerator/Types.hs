module JitCert.DocGenerator.Types where

import qualified Data.Text.Lazy                as T

newtype DocGenerator = DocGenerator ([[TDoc]] -> [TDoc])

type TDoc = (Doc, MergeType)

data Doc
  = Paragraph [Doc]
  | Heading [Doc]
  | NumList [Doc]
  | List [Doc]
  | Txt T.Text

data MergeType
  = Flatten
  | Concat
