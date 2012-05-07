{-# LANGUAGE ViewPatterns #-}

module M.View(satisfy) where

import Data.List as List
import Control.Monad
import Definitions

-- thinking ahead
-- [m|a*(b+c)] -- one free var, one not
-- satisfy prodV (1,1) [satisfy sumV (2,0) []]  -> Just [a,b,c]

satisfy :: (Show a) => (a -> Maybe [a]) -> ([a] -> a) -> (Int, Int) -> [a -> Maybe [a]] -> a -> Maybe [a]
satisfy view viewRecons (free, 0) _ (view -> Just vals) | length vals >= free =
	let
		(firstFrees, lastFree) = splitAt (free-1) vals
	in Just $ firstFrees ++ [viewRecons lastFree]
satisfy view viewRecons (free, bound) boundrules (view -> Just vals) | length vals >= free + bound =
	let
		solutionSpace = permutationsOfLenAndOthers bound vals
		solutions = do
			(solution, unused) <- solutionSpace
			let 
				solveAttempt = sequence $ zipWith ($) boundrules solution
				(firstFrees, lastFree) = splitAt (free -1) unused
				freeVals = firstFrees ++ [viewRecons lastFree]
			case solveAttempt of
				Nothing -> []
				Just  boundsols -> [concat boundsols ++ freeVals]
	in if not $ null solutions then Just $ head solutions else Nothing
satisfy a b c d e = Nothing

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
