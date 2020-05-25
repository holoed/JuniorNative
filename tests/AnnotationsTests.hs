module AnnotationsTests where

import Test.Hspec
import Fixpoint
import Primitives
import Ast
import Annotations

tests :: SpecWith ()
tests =
  describe "Annotations Tests" $ do

    it "Annotate a literal" $
      wrap 12 (lit $ I 42) `shouldBe` In (Ann 12 (Lit (I 42)))

    it "UnAnnotate a literal" $
      unwrap (In (Ann 12 (Lit (I 42)))) `shouldBe` In (Lit (I 42))

    it "Map annotations" $
      mapAnn (*2) (wrap 25 (lit $ I 42)) `shouldBe` In (Ann 50 (Lit (I 42)))
