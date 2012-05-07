{-# LANGUAGE MultiParamTypeClasses, ViewPatterns, TupleSections #-}

module M.PrePat(PrePat(..), BoundVar(..), makePat) where

import Prelude hiding (const)
import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import Definitions

data PrePat = Free String
			| Bound (Exp) [BoundVar]
		deriving (Show)

isFree (Free _) = True
isFree _ = False

data BoundVar = Wild | BoundVar String
	deriving (Show)

instance Symbolic Integer PrePat where
	var s = Free s
	const n = Bound (LitE $ IntPrimL n) [Wild]

instance SymbolicSum PrePat where
	sumV _ = Nothing
	sumS l = 
		let
			frees = map (\(Free a) -> a) $ filter isFree l
			(bindings, bounds) = unzip $ map (\(Bound a b) -> (a,b)) $ filter (not.isFree) l
			e name = VarE (mkName name)
			n num =  LitE (IntegerL (fromIntegral num))
			($$) = AppE
			satisfySum' = 
				e "satisfy" 
				$$ e "sumV" 
				$$ e "sumS" 
				$$ TupE [e "fromIntegral" $$ n (length frees), e "fromIntegral" $$  n (length bounds)] 
				$$ ListE bindings
		in Bound satisfySum'  (concat bounds ++ (map BoundVar frees))


instance SymbolicProd PrePat where
	prodV _ = Nothing
	prodS l = 
		let
			frees = map (\(Free a) -> a) $ filter isFree l
			(bindings, bounds) = unzip $ map (\(Bound a b) -> (a,b)) $ filter (not.isFree) l
			e name = VarE (mkName name)
			n num =  LitE (IntegerL (fromIntegral num))
			($$) = AppE
			satisfySum' = 
				e "satisfy" 
				$$ e "prodV" 
				$$ e "prodS" 
				$$ TupE [e "fromIntegral" $$ n (length frees), e "fromIntegral" $$  n (length bounds)] 
				$$ ListE bindings
		in Bound satisfySum'  (concat bounds ++ (map BoundVar frees))


makePat :: PrePat -> Pat
makePat (Free name) = VarP (mkName name)
makePat (Bound exp vars) = ViewP exp (ConP (mkName "Just") [ListP $ map makeVarPat vars])
	where
		makeVarPat (BoundVar s) = VarP (mkName s)
		makeVarPat  Wild        = WildP

