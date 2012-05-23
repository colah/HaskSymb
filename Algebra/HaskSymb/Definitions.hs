{-# LANGUAGE MultiParamTypeClasses, RankNTypes, FunctionalDependencies, ViewPatterns, NoMonomorphismRestriction #-}

module Algebra.HaskSymb.Definitions where

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
		sumC'' [x] = x
		sumC'' xs = sumC xs
		isSum (sumD -> Just _) = True
		isSum _ = False
		sums = filter isSum vals
		nonsums = filter (not.isSum) vals
		isConst (constD -> Just _) = True
		isConst       _            = False
		consts = map (\(constD -> Just a) -> a) $ filter isConst vals
	in if null sums
		then if null consts
			then sumC'' vals
			else sumC'' $ 
				(if sum consts == 0 then [] else [constC (sum consts)]) ++ filter (not.isConst) vals
		else sumC' $ nonsums ++ concat (map (\(sumD -> Just a) -> a) sums)



-- a can be multiplied
class SymbolicProd a where
	prodC :: [a] -> a
	prodD ::  a  -> Maybe [a]

prodC' vals = 
	let
		prodC'' [x] = x
		prodC'' xs = prodC xs
		isProd (prodD -> Just _) = True
		isProd _ = False
		prods = filter isProd vals
		nonprods = filter (not.isProd) vals
		isConst (constD -> Just _) = True
		isConst       _            = False
		consts = map (\(constD -> Just a) -> a) $ filter isConst vals
		nonconsts = filter (not.isConst) vals
	in if null prods
		then if null consts
			then prodC'' vals
			else case (nonconsts, product consts) of
				([],n) -> constC n
				(_, 0) -> constC 0
				(l, 1) -> prodC l
				(l, n) -> prodC $ (constC n) : l
		else prodC' $ nonprods ++ concat (map (\(prodD -> Just a) -> a) prods)

-- a can be differentiated
class SymbolicDiff a where
	diffC :: a -> a
	diffD :: a -> Maybe a

{-cleanLCons :: ([a] -> a) -> (a -> Maybe [a]) -> ([b] -> a) -> (a -> b) -> (b -> Maybe a) -> a -> a
cleanLCons      cons            dest           constCons     constC        constD  (dest -> Just vals) = 
	let
		--isCons :: a -> Bool
		isCons (dest -> Just _) = True
		isCons        _         = False
		--isConst :: a -> Bool
		isConst (constD -> Just _) = True
		isConst       _            = False
		--conses :: [a]
		conses = filter isCons vals
		--consts :: [b]
		consts = map (\(constD -> Just a) -> a) $ filter isConst vals
	in if null conses
		then cons $ [constC (constCons consts)] ++ filter (not.isConst) vals
		else cleanLCons cons dest constCons constC constD $ cons $
			filter (not . isCons) vals ++ concat (map (\(dest -> Just a) -> a) conses)
-}



listEq (==) (a:as) b =
	let
		amatches = filter (==a) b
		nonamatches = filter (not .(==a)) b
	in
		if null amatches
		then False
		else listEq (==) as (tail amatches ++ nonamatches)
listEq _ [] [] = True
listEq _ _  _  = False

(===) :: (SymbolicSum a, Symbolic b a, Eq b, SymbolicProd a, SymbolicDiff a) => a -> a -> Bool
(constD -> Just a) === (constD -> Just b) = a == b
(varD   -> Just a) === (varD   -> Just b) = a == b
(sumD  -> Just as) === (sumD  -> Just bs) = listEq (===) as bs
(sumD -> Just [a]) === (              b ) = a === b
(              a ) === (sumD -> Just [b]) = a === b
(prodD -> Just as) === (prodD -> Just bs) = listEq (===) as bs
(prodD-> Just [a]) === (              b ) = a === b
(              a ) === (prodD-> Just [b]) = a === b
(diffD -> Just a ) === (diffD -> Just b ) = a === b
a                  ===                b   = False

--instance Eq
