{-# LANGUAGE MultiParamTypeClasses, ViewPatterns, TupleSections #-}

module M.PrePat(PrePat(..), BoundVar(..), makePat) where

import Prelude hiding (const)
import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import qualified Data.List as List

import Definitions

data PrePat = Free String
			| Bound (Exp) [BoundVar]
		deriving (Show)

isFree (Free _) = True
isFree _ = False

e name = VarE (mkName name)
n num =  LitE (IntegerL (fromIntegral num))
($$) = AppE

collectStringCopies :: [String] -> [(String, [Int])]
collectStringCopies vars = map (\var -> (var, poslist 0 var vars )) $ List.nub vars
	where
		poslist _     _     []     = []
		poslist shift match (x:xs) =  
			if x == match 
				then shift : poslist (shift+1) match xs
				else  poslist (shift+1) match xs


data BoundVar = Wild | BoundVar String
	deriving (Show)

instance Symbolic Integer PrePat where
	varC s = Free s
	varD _ = Nothing
	constC m = Bound (e "matchesConst" $$ (e "fromIntegral" $$ n m)) [Wild]
	constD _ = Nothing

instance SymbolicSum PrePat where
	sumD _ = Nothing
	sumC l = 
		let
			frees = map (\(Free a) -> a) $ filter isFree l
			(bindings, bounds) = unzip $ map (\(Bound a b) -> (a,b)) $ filter (not.isFree) l
			varlist = frees ++ (map (\(BoundVar name) -> name) $ concat bounds)
			satisfySum' = 
				e "satisfy" 
				$$ e "sumD" 
				$$ e "sumC" 
				$$ TupE [e "fromIntegral" $$ n (length frees), e "fromIntegral" $$  n (length bounds)] 
				$$ ListE bindings
			--satisfyEq' :: [Int] -> Maybe a -> Exp
			satisfyEq' posList a =
				e "requireEq"
				$$ ListE (map (\pos -> e "fromIntegral" $$ n pos) posList)
				$$ a
			multvars :: [(String, [Int])]
			multvars = filter (\(a,b) -> length b >= 2) $ collectStringCopies varlist
			eqreq = if null multvars
				then id
				else foldl1 (.) $ map (\(var, poslist) -> satisfyEq' poslist ) multvars
		in Bound (eqreq satisfySum')  (concat bounds ++ (map BoundVar (List.nub frees)))

instance SymbolicProd PrePat where
	prodD _ = Nothing
	prodC l = 
		let
			frees = map (\(Free a) -> a) $ filter isFree l
			(bindings, bounds) = unzip $ map (\(Bound a b) -> (a,b)) $ filter (not.isFree) l
			e name = VarE (mkName name)
			n num =  LitE (IntegerL (fromIntegral num))
			($$) = AppE
			satisfySum' = 
				e "satisfy" 
				$$ e "prodD" 
				$$ e "prodC" 
				$$ TupE [e "fromIntegral" $$ n (length frees), e "fromIntegral" $$  n (length bounds)] 
				$$ ListE bindings
		in Bound satisfySum'  (concat bounds ++ (map BoundVar frees))


makePat :: PrePat -> Pat
makePat (Free name) = VarP (mkName name)
makePat (Bound exp vars) = ViewP exp (ConP (mkName "Just") [ListP $ map makeVarPat vars])
	where
		makeVarPat (BoundVar s) = VarP (mkName s)
		makeVarPat  Wild        = WildP

