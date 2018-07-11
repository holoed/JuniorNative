module AnnotationsTests where

import Test.Hspec
import Fixpoint
import Ast
import Annotations

tests :: SpecWith ()
tests =
  describe "Annotations Tests" $ do

    it "Annotate a literal" $
      wrap 12 (lit $ I 42) `shouldBe` In (Ann 12 (Lit (I 42)))

    it "UnAnnotate a literal" $
      unwrap (In (Ann 12 (Lit (I 42)))) `shouldBe` In (Lit (I 42))
