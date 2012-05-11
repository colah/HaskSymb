
{-# LANGUAGE RankNTypes, ViewPatterns, FlexibleInstances, MultiParamTypeClasses  #-}

module Mexpr (MExpr(..), module BasicAlgs) where

import Definitions
import Data.List as List
import BasicAlgs

data MExpr a =  C a | V String | Sum [MExpr a] | Prod [MExpr a]

instance (Show a) => Show (MExpr a) where
	show = prettyShow

prettyShow :: (Show a) => MExpr a -> String 
prettyShow a = show2 0 a where
		show2 :: (Show a) => Int -> MExpr a -> String
		show2 n@0 (Sum vals) = concat $ List.intersperse "+" $ map (show2 n) vals
		show2 0 a = show2 1 a
		show2 n@1 (Prod vals) = concat $ List.intersperse "*" $ map (show2 n) vals
		show2 1 (C a) = show a
		show2 1 (V s) = s
		show2 1 a = "(" ++ show2 0 a ++ ")"

instance Symbolic Int (MExpr Int) where
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

