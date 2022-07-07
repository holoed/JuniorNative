{-# OPTIONS_GHC -Wno-type-defaults #-}
module UnitTests.AnnotationsSpec where

import Test.Sandwich ( describe, it, shouldBe, TopSpec, parallel )
import Fixpoint ( Fix(In) )
import Primitives ( Prim(I) )
import Ast ( ExpF(Lit) )
import Annotations ( Ann(Ann), unwrap, wrap, mapAnn )

tests :: TopSpec
tests = parallel $
  describe "Annotations Tests" $ do

    it "Annotate a literal" $
      wrap 12 (In $ Lit $ I 42) `shouldBe` In (Ann 12 (Lit (I 42)))

    it "UnAnnotate a literal" $
      unwrap (In (Ann 12 (Lit (I 42)))) `shouldBe` In (Lit (I 42))

    it "Map annotations" $
      mapAnn (*2) (wrap 25 (In $ Lit $ I 42)) `shouldBe` In (Ann 50 (Lit (I 42)))