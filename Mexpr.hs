
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
	constS = C
	constV (C n) = Just n
	constV _ = Nothing
	varS   = V
	varV (V s) = Just s
	varV _ = Nothing

instance (Num a) => SymbolicSum (MExpr a) where
	sumS = Sum
	sumV (Sum l) = Just l
	sumV _ = Nothing

instance (Num a) => SymbolicProd (MExpr a) where
	prodS = Prod
	prodV (Prod l) = Just l
	prodV _ = Nothing

