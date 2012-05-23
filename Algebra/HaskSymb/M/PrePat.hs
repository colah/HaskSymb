{-# LANGUAGE MultiParamTypeClasses, ViewPatterns, Rank2Types, FlexibleContexts, FlexibleInstances, GADTs, NoMonomorphismRestriction, TemplateHaskell, TypeSynonymInstances #-}

module Algebra.HaskSymb.M.PrePat {- (PrePat(..), BoundVar(..), makePat) -} where

import Prelude hiding (const)
import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import qualified Data.List as List
import Data.Pattern

import Algebra.HaskSymb.Definitions

type SymbPat a = PrePat a a

isConst (constD -> Just _) = True
isConst         _          = False

instance (Symbolic Integer a, Eq a) => Symbolic Integer (SymbPat a) where
	constD _ = Nothing
	constC m = Const (e "constC" $$ n m, constC m)
	varD   _ = Nothing
	varC   v@(last -> 'C') = Guard (e "isConst", isConst) $ Free v
	varC   v               = Free v

instance (SymbolicSum a, Eq a, Symbolic Integer a) => SymbolicSum (SymbPat a) where
	sumD _ = Nothing
	sumC pats = PreProcess (e "sumD",sumD) $ 
		ListPat [Commutative, CompressExtra (e "sumC", sumC),FillMissing (e "constC" $$ n 0, constC 0)] pats

instance (SymbolicProd a, Eq a, Symbolic Integer a) => SymbolicProd (SymbPat a) where
	prodD _ = Nothing
	prodC pats = PreProcess (e "prodD", prodD) $ 
		ListPat [Commutative, CompressExtra (e "prodC", prodC),FillMissing (e "constC" $$ n 1, constC 1)] pats

instance (SymbolicDiff a, Eq a, Symbolic Integer a) => SymbolicDiff (SymbPat a) where
	diffD _ = Nothing
	diffC pat = PreProcess (e "diffD", diffD) pat

makePat :: SymbPat a -> Q Pat
makePat = finishPat

e name = VarE (mkName name)
n num =  LitE (IntegerL (fromIntegral num))
($$) = AppE

