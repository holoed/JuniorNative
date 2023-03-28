{-# OPTIONS_GHC -Wno-type-defaults #-}
module UnitTests.AnnotationsSpec where

import Test.Sandwich ( describe, it, shouldBe, TopSpec, parallel )
import Junior.Utils.Fixpoint ( Fix(In) )
import Junior.Parser.Primitives ( Prim(I) )
import Junior.Core.Ast ( ExpF(Lit, Var) )
import Junior.Utils.Annotations ( Ann(Ann), unwrap, wrap, mapAnn, mapAnnM )
import Control.Monad.Identity (Identity(..))

tests :: TopSpec
tests = parallel $
  describe "Annotations Tests" $ do

    it "Annotate a literal" $
      wrap 12 (In $ Lit $ I 42) `shouldBe` In (Ann 12 (Lit (I 42)))

    it "UnAnnotate a literal" $
      unwrap (In (Ann 12 (Lit (I 42)))) `shouldBe` In (Lit (I 42))

    it "Map annotations" $
      mapAnn (*2) (wrap 25 (In $ Lit $ I 42)) `shouldBe` In (Ann 50 (Lit (I 42)))

    it "Annotate a variable" $
      wrap "test" (In $ Var "x") `shouldBe` In (Ann "test" (Var "x"))

    it "UnAnnotate a variable" $
      unwrap (In (Ann "test" (Var "x"))) `shouldBe` In (Var "x")

    it "Map annotations with a monad" $ do
      let input = wrap 10 (In $ Var "x")
          expected = In (Ann 20 (Var "x"))
          result = runIdentity $ mapAnnM (\x -> return (x * 2)) input
      result `shouldBe` expected

    it "Map annotations with a monad and no side-effects" $ do
      let input = wrap "A" (In $ Var "x")
          expected = In (Ann "B" (Var "x"))
          result = runIdentity $ mapAnnM (\_ -> return "B") input
      result `shouldBe` expected