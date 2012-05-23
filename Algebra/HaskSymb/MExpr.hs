
{-# LANGUAGE RankNTypes, ViewPatterns, FlexibleInstances, MultiParamTypeClasses #-}

module Algebra.HaskSymb.MExpr (MExpr(..)) where

import Algebra.HaskSymb.Definitions
import Data.List as List
import Data.Maybe as Maybe

data MExpr a =  C a | V String | Sum [MExpr a] | Prod [MExpr a]
				| Cos (MExpr a) | Sin (MExpr a) | Tan (MExpr a)
				| D (MExpr a)
--	deriving Show

collectCopies :: (Eq a) => [a] -> [(a, [Int])]
collectCopies vars = map (\var -> (var, poslist 0 var vars )) $ List.nub vars
	where
		poslist _     _     []     = []
		poslist shift match (x:xs) =  
			if x == match 
				then shift : poslist (shift+1) match xs
				else  poslist (shift+1) match xs



instance (Show a, Eq a, Num a) => Show (MExpr a) where
	show = show2 0 where
		show2 :: (Show a, Eq a, Num a) => Int -> MExpr a -> String
		show2 _ (Sum []) = "EMPTYSUM"
		show2 _ (Prod []) = "EMPTYPROD"
		show2 n (D val) = if n > 0 || (Maybe.isJust . sumD $ val)
							then "d(" ++ show2 0 val ++ ")"
							else "d" ++ show2 n val
		show2 n (Sin val) = "sin(" ++ show2 n val ++ ")"
		show2 n (Cos val) = "cos(" ++ show2 n val ++ ")"
		show2 n (Tan val) = "tan(" ++ show2 n val ++ ")"
		show2 n@0 (Sum vals) = 
			concat $ List.intersperse " + " $ map (show2 n) $ reverse $ List.sortBy cmp vals
				where 
					(Prod a) `cmp` (Prod b) = length a `compare` length b
					_        `cmp` (Prod _) = LT
					(Prod _) `cmp`   _      = GT
					_        `cmp`   _      = EQ
		show2 0 a = show2 1 a
		show2 n@1 (Prod vals) = 
			pre ++ (concat $ List.intersperse "*" $ map (showWithPow.lengthifySecond) $ collectCopies nonconsts)
				where
					isConst (C a) = True
					isConst _     = False
					consts = map (\(C n) -> n) $ filter isConst vals
					nonconsts = filter (not.isConst) vals
					pre = if null consts || product consts == 1
						then ""
						else show $ product consts
					lengthifySecond (a,b) = (a, length b)
					showWithPow (a, 1) = show2 n a
					showWithPow (a, 2) = show2 n a ++ "²"
					showWithPow (a, 3) = show2 n a ++ "³"
					showWithPow (a, 4) = show2 n a ++ "⁴"
					showWithPow (a, 5) = show2 n a ++ "⁵"
					showWithPow (a, 6) = show2 n a ++ "⁶"
					showWithPow (a, 7) = show2 n a ++ "⁷"
					showWithPow (a, 8) = show2 n a ++ "⁸"
					showWithPow (a, 9) = show2 n a ++ "⁹"
					showWithPow (a, m) = show2 n a ++ "^" ++ show m
		show2 1 (C a) = show a
		show2 1 (V s) = s
		show2 1 a = "(" ++ show2 0 a ++ ")"



instance (Num a, Eq a) => Eq (MExpr a) where
	a == b = a === b

instance (Num a, Eq a, Show a) => Num (MExpr a) where
	fromInteger n = C $ fromIntegral n
	a + b = sumC' [a,b]
	a * b = prodC' [a,b]
	negate = ((-1)*)
	abs _ = error "absolute value not supported in MExpr"
	signum _ = error "no signum provided in MExpr"
	
	

instance (Num a, Eq a) => Symbolic a (MExpr a) where
	constC = C
	constD (C n) = Just n
	constD _ = Nothing
	varC   = V
	varD (V s) = Just s
	varD _ = Nothing

instance (Num a) => SymbolicSum (MExpr a) where
	sumC = Sum
	sumD (Sum l) = Just l
	sumD _ = Nothing

instance (Num a) => SymbolicProd (MExpr a) where
	prodC = Prod
	prodD (Prod l) = Just l
	prodD _ = Nothing

instance SymbolicDiff (MExpr a) where
	diffC a = D a
	diffD (D a) = Just a
	diffD _     = Nothing
