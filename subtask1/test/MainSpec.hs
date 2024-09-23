module MainSpec (spec) where

import Test.Hspec
import qualified Editor.BufferSpec
import qualified Editor.CursorSpec
import qualified Editor.FileIOSpec
import qualified Editor.SyntaxHighlightSpec
import qualified Editor.MultiBufferSpec
import qualified Editor.UISpec

spec :: Spec
spec = do
  describe "Editor.Buffer" Editor.BufferSpec.spec
  describe "Editor.Cursor" Editor.CursorSpec.spec
  describe "Editor.FileIO" Editor.FileIOSpec.spec
  describe "Editor.SyntaxHighlight" Editor.SyntaxHighlightSpec.spec
  describe "Editor.MultiBuffer" Editor.MultiBufferSpec.spec
  describe "Editor.UI" Editor.UISpec.spec
