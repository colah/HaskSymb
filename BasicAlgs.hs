{-# LANGUAGE MultiParamTypeClasses, ViewPatterns, TemplateHaskell, QuasiQuotes #-}

module BasicAlgs where

import Prelude hiding ((+), (*))
import M.QQ
import Definitions

a+b = sumC' [a,b]
a*b = prodC' [a,b]

--expand :: (SymbolicSum a, SymbolicProd a) => a -> a
expand [m|  a+b  |] = expand a + expand b
expand [m|a*(b+c)|] = expand (a*b) + expand (a*c)
expand [m|  a*b  |] = expand a * expand b
expand       a      = a


collectTerms [m| aC*x + bC*x + c |] = collectTerms $ (aC+bC)*x         + c
collectTerms [m| aC*x +    x + c |] = collectTerms $ (aC+ constC 1)*x  + c
collectTerms [m|    x +    x + c |] = collectTerms $ (constC 2)*x      + c
collectTerms [m| aC*x + bC*x     |] = collectTerms $ (aC+bC)*x
collectTerms [m| aC*x +    x     |] = collectTerms $ (aC+ constC 1)*x
collectTerms [m|    x +    x     |] = collectTerms $ (constC 2)*x
collectTerms          a             =  a

--exapndAndCollect = collectTerms . expand


