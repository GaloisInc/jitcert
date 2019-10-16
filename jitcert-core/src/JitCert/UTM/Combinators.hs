module JitCert.UTM.Combinators where

import JitCert.GSN.Types

-- Types
type Ctx c = Node c Context

-- Dummy pattern for now
data IsInside b c = IsInside (Node b Context) (Node c Context)
instance (RenderContext b, RenderContext c) => Property (IsInside b c) where
  renderProperty (IsInside b c) =
    [SFContext b, SFText "is inside", SFContext c]

newtype AccordingTo b = AccordingTo (Node b Context)
instance (RenderContext b) => Property (AccordingTo b) where
  renderProperty (AccordingTo b) =
    [SFText "according to ", SFContext b]
