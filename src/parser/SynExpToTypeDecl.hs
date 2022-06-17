{-# LANGUAGE TupleSections #-}
module SynExpToTypeDecl where

import qualified Ast
import qualified PAst
import Annotations ( Ann(Ann) )
import Fixpoint ( Fix(In) )
import TypesPrinter () 

toTypeDecl :: PAst.SynExp -> [Ast.TypeDecl]
toTypeDecl (In (Ann _ (PAst.TypeDecl t ts)))= [Ast.TypeDecl t ts]
toTypeDecl _ = []

