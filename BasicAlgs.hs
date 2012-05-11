{-# LANGUAGE MultiParamTypeClasses, ViewPatterns, TemplateHaskell, QuasiQuotes #-}

module BasicAlgs where

import Prelude hiding ((+), (*))
import M.QQ
import Definitions

a+b = sumC [a,b]
a*b = prodC [a,b]

--expand :: (SymbolicSum a, SymbolicProd a) => a -> a
expand [m|  a+b  |] = expand a + expand b
expand [m|a*(b+c)|] = expand (a*b) + expand (a*c)
expand [m|  a*b  |] = expand a * expand b
expand       a      = a

test [m|a+a|] = a
test [m|2*a|] = a
