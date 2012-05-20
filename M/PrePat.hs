{-# LANGUAGE MultiParamTypeClasses, ViewPatterns, TupleSections, Rank2Types, FlexibleContexts, FlexibleInstances, GADTs, NoMonomorphismRestriction, TemplateHaskell #-}

module M.PrePat {- (PrePat(..), BoundVar(..), makePat) -} where

import Prelude hiding (const)
import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Data.Data
import qualified Data.List as List
import Data.Pattern

import Definitions

type SymbPat a = PrePat a a

isConst (constD -> Just _) = True
isConst         _          = False

instance (Symbolic Integer a, Data a, Eq a) => Symbolic Integer (SymbPat a) where
	constD _ = Nothing
	constC m = Const (e "constC" $$ n m, constC m)
	varD   _ = Nothing
	varC   v@(last -> 'C') = Guard (e "isConst", isConst) $ Free v
	varC   v               = Free v

instance (SymbolicSum a, Data a, Eq a, Symbolic Integer a) => SymbolicSum (SymbPat a) where
	sumD _ = Nothing
	sumC pats = PreProcess (e "sumD",sumD) $ 
		ListPat [Commutative, CompressExtra (e "sumC", sumC)] pats

instance (SymbolicProd a, Data a, Eq a, Symbolic Integer a) => SymbolicProd (SymbPat a) where
	prodD _ = Nothing
	prodC pats = PreProcess (e "prodD", prodD) $ 
		ListPat [Commutative, CompressExtra (e "prodC", prodC)] pats

makePat :: SymbPat a -> Q Pat
makePat = finishPat

e name = VarE (mkName name)
n num =  LitE (IntegerL (fromIntegral num))
($$) = AppE

