{-# LANGUAGE MultiParamTypeClasses, RankNTypes, FunctionalDependencies, ViewPatterns, NoMonomorphismRestriction #-}

module Definitions where

import Prelude hiding (const)
import Data.List as List

-- This is the heart of things.

-- For every mathematical property one may wish to implement, we make a 
-- constructor (names postpended with C) and a destructor
-- that may produce it (names postpended with D; best used with ViewPatterns)

-- b has both variables and constants of type a
class Symbolic a b| b-> a where
	constC :: a -> b
	constD :: b -> Maybe a
	varC :: String -> b
	varD :: b -> Maybe String

-- a can be added
class SymbolicSum a where
	sumC :: [a] -> a
	sumD ::  a  -> Maybe [a]

sumC' vals = 
	let
		isSum (sumD -> Just _) = True
		isSum _ = False
		sums = filter isSum vals
		nonsums = filter (not.isSum) vals
	in if null sums
		then sumC nonsums
		else sumC $ nonsums ++ concat (map (\(sumD -> Just a) -> a) sums)

-- a can be multiplied
class SymbolicProd a where
	prodC :: [a] -> a
	prodD ::  a  -> Maybe [a]

prodC' vals = 
	let
		isProd (prodD -> Just _) = True
		isProd _ = False
		prods = filter isProd vals
		nonprods = filter (not.isProd) vals
	in if null prods
		then prodC nonprods
		else prodC $ nonprods ++ concat (map (\(prodD -> Just a) -> a) prods)


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
(constD -> Just a) === (constD -> Just b) = a == b
(varD   -> Just a) === (varD   -> Just b) = a == b
(sumD  -> Just as) === (sumD  -> Just bs) = listEq (===) as bs
(sumD -> Just [a]) === (              b ) = a === b
(              a ) === (sumD -> Just [b]) = a === b
(prodD -> Just as) === (prodD -> Just bs) = listEq (===) as bs
(prodD-> Just [a]) === (              b ) = a === b
(              a ) === (prodD-> Just [b]) = a === b
a                  ===                b   = False
