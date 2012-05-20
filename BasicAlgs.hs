{-# LANGUAGE MultiParamTypeClasses, ViewPatterns, TemplateHaskell, QuasiQuotes, NoMonomorphismRestriction #-}

module BasicAlgs where

import Prelude hiding ((+), (*))
import M.QQ
import Definitions

a+b = sumC' [a,b]
a*b = prodC' [a,b]

--expand :: (SymbolicSum a, SymbolicProd a) => a -> a
expand [m|  a+b  |] = expand a + expand b
expand [m|a*(b+c)|] = expand (a*b) + expand (a*c)
expand       a      = a


collectTerms [m| aC*x + bC*x + c |] = collectTerms $ (aC+bC)*x + c
collectTerms          a             =  a

exapndAndCollect = collectTerms . expand


