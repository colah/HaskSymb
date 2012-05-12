
{-# LANGUAGE RankNTypes, ViewPatterns, FlexibleInstances, MultiParamTypeClasses  #-}

module Mexpr (MExpr(..), module BasicAlgs, module M.View) where

import Definitions
import Data.List as List
import BasicAlgs hiding ((+), (*))
import M.View

data MExpr a =  C a | V String | Sum [MExpr a] | Prod [MExpr a]

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
		show2 n@0 (Sum vals) = concat $ List.intersperse "+" $ map (show2 n) vals
		show2 0 a = show2 1 a
		show2 n@1 (Prod vals) = 
			concat $ List.intersperse "*" $ map (showWithPow.lengthifySecond) $ collectCopies vals2
				where
					isConst (C a) = True
					isConst _     = False
					consts = map (\(C n) -> n) $ filter isConst vals
					nonconsts = filter (not.isConst) vals
					vals2 = if null consts || product consts == 1
						then nonconsts
						else (C $ product consts):nonconsts
					lengthifySecond (a,b) = (a, length b)
					showWithPow (a, 1) = show2 n a
					showWithPow (a, 2) = show2 n a ++ "²"
					showWithPow (a, 3) = show2 n a ++ "³"
					showWithPow (a, 4) = show2 n a ++ "⁴"
					showWithPow (a, 5) = show2 n a ++ "⁵"
					showWithPow (a, m) = show2 n a ++ "^" ++ show m
		show2 1 (C a) = show a
		show2 1 (V s) = s
		show2 1 a = "(" ++ show2 0 a ++ ")"

instance Num a => Eq (MExpr a) where
	a == b = a === b

instance Num a => Num (MExpr a) where
	fromInteger n = C $ fromIntegral n
	a + b = sumC' [a,b]
	a * b = prodC' [a,b]
	negate = ((-1)*)
	abs _ = error "absolute value not supported in MExpr"
	signum _ = error "no signum provided in MExpr"
	
	

instance (Num a) => Symbolic a (MExpr a) where
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

