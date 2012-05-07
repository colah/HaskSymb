{-# LANGUAGE MultiParamTypeClasses, ViewPatterns, TemplateHaskell, QuasiQuotes #-}

module BasicAlgs where

import Prelude hiding ((+), (*))
import M.QQ
import Definitions

a+b = sumS [a,b]
a*b = prodS [a,b]

--expand :: (SymbolicSum a, SymbolicProd a) => a -> a
expand [m|a + b|] = expand a + expand b
expand [m|a*(b+c)|] = expand (a*b) + expand (a*c)
expand [m|a*b|] = expand a * expand b
expand a = a

--test2 = prodS [var "a", var "b", sumS [var "a", var "c"]] :: VarPoly Int

