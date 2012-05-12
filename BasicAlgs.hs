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

collectTerms [m| aK*x + bK*x |] = (aK+bK)*x
collectTerms [m|    x + bK*x |] = (constC 1+bK)*x
collectTerms [m| aK*x +    x |] = (aK+ constC 1)*x
collectTerms [m|    x +    x |] = (constC 2)*x
collectTerms [m| aK*x + bK*x + c |] = collectTerms $ (aK+bK)*x         + c
collectTerms [m|    x + bK*x + c |] = collectTerms $ (constC 1+bK)*x   + c
collectTerms [m| aK*x +    x + c |] = collectTerms $ (aK+ constC 1)*x  + c
collectTerms [m|    x +    x + c |] = collectTerms $ (constC 2)*x      + c
collectTerms          a        =  a

--exapndAndCollect = collectTerms . expand
