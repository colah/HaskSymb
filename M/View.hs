{-# LANGUAGE ViewPatterns #-}

module M.View(satisfy, satisfy2, requireEq, coaleceView, matchesConst) where

import Data.List as List
import Control.Monad
import Definitions

-- thinking ahead
-- [m|a*(b+c)] -- one free var, one not
-- satisfy prodD (1,1) [satisfy sumD (2,0) []]  -> Just [a,b,c]

permutationsOfLenAndOthers n l = 
	let
		subOfLenAndOthers :: Int -> [a] -> [a] -> [([a],[a])]
		subOfLenAndOthers 0 oth   l    = []
		subOfLenAndOthers 1 oth   l    = [sep n l | (_,n) <- zip l [0,1..] ]
			where sep n (splitAt n -> (start, center:end)) = ([center], start++end)
		subOfLenAndOthers _ oth   []   = []
		subOfLenAndOthers n oth (x:xs) = (map (\(a,b) -> (x:a,b)) $ subOfLenAndOthers (n-1) oth xs) ++ subOfLenAndOthers n (oth ++ [x]) xs
	in do
		(subs, others) <- subOfLenAndOthers n [] l
		subsperm <- List.permutations subs
		return (subsperm, others)

satisfy2 :: (Show a) => (a -> Maybe [a]) -> ([a] -> a) -> (Int, Int) -> ([a] -> Maybe [a]) -> a -> Maybe [a]
satisfy2 view viewRecons (free, 0) _ (view -> Just vals) | length vals >= free =
	let
		(firstFrees, lastFree) = splitAt (free-1) vals
	in Just $ firstFrees ++ [if length lastFree == 1 then head lastFree else viewRecons lastFree]
satisfy2 view viewRecons (free, bound) boundrules (view -> Just vals) | length vals >= free + bound =
	let
		solutionSpace = permutationsOfLenAndOthers bound vals
		solutions = do
			(solution, unused) <- solutionSpace
			let 
				solveAttempt = boundrules solution
				(firstFrees, lastFree) = splitAt (free -1) unused
				freeVals = firstFrees ++ [if length lastFree == 1 then head lastFree else viewRecons lastFree]
			case solveAttempt of
				Nothing -> []
				Just  boundsols -> [boundsols ++ freeVals]
	in if not $ null solutions then Just $ head solutions else Nothing
satisfy2 a b c d e = Nothing

coaleceView :: [a -> Maybe [a]] -> [a] -> Maybe [a]
coaleceView boundRules = fmap (concat) . sequence . zipWith ($) boundRules

satisfy a b c = satisfy2 a b c . coaleceView

matchesConst :: (Symbolic a b, Eq a) =>  a -> b -> Maybe [b]
matchesConst n  a@(constD -> Just m) | m == n = Just [a]
matchesConst _        _               = Nothing

--requireEq :: [Int] -> Maybe [a] -> Maybe [a]

requireEq [] child (child -> a) = a
requireEq eqpos@(eq1:eqothers) child (child -> Just vals) | maximum eqpos < length vals  = 
	let
		drop n (splitAt n -> (a, _:b)) =  a ++ b
		drop _ l = l
	in if all (=== (vals !! eq1)) $ map (vals !!) eqothers
		then Just $ (foldl1 (.) [drop n | n <- eqothers]) vals
		else Nothing
requireEq _ _ _ = Nothing

