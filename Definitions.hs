{-# LANGUAGE MultiParamTypeClasses, RankNTypes, FunctionalDependencies, ViewPatterns, NoMonomorphismRestriction #-}

module Definitions where

import Prelude hiding (const)
import Data.List as List
import Debug.Trace

class Symbolic a b| b-> a where
	constS :: a -> b
	constV :: b -> Maybe a
	varS :: String -> b
	varV :: b -> Maybe String


class SymbolicSum a where
	sumS :: [a] -> a
	sumV ::  a  -> Maybe [a]

sumS' vals = 
	let
		isSum (sumV -> Just _) = True
		isSum _ = False
		sums = filter isSum vals
		nonsums = filter (not.isSum) vals
	in if null sums
		then sumS nonsums
		else sumS $ nonsums ++ concat (map (\(sumV -> Just a) -> a) sums)


class SymbolicProd a where
	prodS :: [a] -> a
	prodV ::  a  -> Maybe [a]

prodS' vals = 
	let
		isProd (prodV -> Just _) = True
		isProd _ = False
		prods = filter isProd vals
		nonprods = filter (not.isProd) vals
	in if null prods
		then prodS nonprods
		else prodS $ nonprods ++ concat (map (\(prodV -> Just a) -> a) prods)


class SymbolicDiff a where
	diff :: a -> a
	diffV :: a -> Maybe a

class SymbolicInt a where
	int :: a -> a
	intV :: a -> Maybe a


listEq (==) (a:as) b =
	let
		amatches = filter (==a) b
		nonamatches = filter (not .(==a)) as
	in
		if null amatches
		then False
		else listEq (==) as (tail amatches ++ nonamatches)
listEq _ [] [] = True

(===) :: (SymbolicSum a, Symbolic b a, Eq b, SymbolicProd a, Show a) => a -> a -> Bool
(constV -> Just a) === (constV -> Just b) = a == b
(varV   -> Just a) === (varV   -> Just b) = a == b
(sumV  -> Just as) === (sumV  -> Just bs) = listEq (===) as bs
(sumV -> Just [a]) === (              b ) = a === b
(              a ) === (sumV -> Just [b]) = a === b
(prodV -> Just as) === (prodV -> Just bs) = listEq (===) as bs
(prodV-> Just [a]) === (              b ) = a === b
(              a ) === (prodV-> Just [b]) = a === b
a                  ===                b   = False
