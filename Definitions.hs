{-# LANGUAGE MultiParamTypeClasses, RankNTypes, FlexibleInstances, FunctionalDependencies  #-}

module Definitions where

import Data.List as List

class Symbolic a b| b-> a where
	const :: a -> b
	var   :: String -> b

class SymbolicSum a where
	sumS :: [a] -> a
	sumV ::  a  -> Maybe [a]

class SymbolicProd a where
	prodS :: [a] -> a
	prodV ::  a  -> Maybe [a]

class SymbolicDiff a where
	diff :: a -> a
	diffV :: a -> Maybe a

class SymbolicInt a where
	int :: a -> a
	intV :: a -> Maybe a

