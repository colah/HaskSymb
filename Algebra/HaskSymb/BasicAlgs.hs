{-# LANGUAGE MultiParamTypeClasses, ViewPatterns, TemplateHaskell, QuasiQuotes, NoMonomorphismRestriction #-}

module Algebra.HaskSymb.BasicAlgs (simplify, expand, collectTerms, diff) where

import Prelude hiding ((+), (*))
import Algebra.HaskSymb.M.QQ
import Algebra.HaskSymb.Definitions

infixl 6 + 
a+b = sumC' [a,b]
infixl 7 *
a*b = prodC' [a,b]

simplify = collectTerms . expand

--expand :: (SymbolicSum a, SymbolicProd a) => a -> a
expand [m|  a+b  |] = expand a + expand b
expand [m|a*(b+c)|] = expand (a*b) + expand (a*c)
expand       a      = a


collectTerms [m| aC*x + bC*x + c |] = collectTerms $ (aC+bC)*x + c
collectTerms [m|    x + bC*x + c |] = collectTerms $ (constC 1 +bC)*x + c
collectTerms [m|    x +    x + c |] = collectTerms $ (constC 2)*x + c
collectTerms          a             =  a

d a = collectTerms $ d' a
	where
		d' [m| a+b |] = d' a + d' b
		d' [m| a*b |] = a * d' b + b * d' a
		d' [m|  aC |] = constC 0
		d'      a     = diffC a

diff a b = collectTerms $ expand $ diff' a b
	where
		diff' var expr | var == expr = 1
		diff' var [m| a + b|] = diff' var a + diff' var b
		diff' var [m| a * b|] = a* (diff' var b) + b* (diff' var a)
		diff' var [m| aC |] = 0
		diff' var a = 0


