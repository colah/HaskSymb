{-# LANGUAGE MultiParamTypeClasses, ViewPatterns #-}


module Algebra.HaskSymb.M.QQ(m, isConst) where

-- Based on http://www.haskell.org/haskellwiki/Quasiquotation

import Data.Pattern
import Algebra.HaskSymb.Definitions
import Algebra.HaskSymb.M.PrePat (isConst)
import Algebra.HaskSymb.M.Parser
import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote


--m  :: QuasiQuoter
m  =  QuasiQuoter quoteMathExp quoteMathPat (fail "") (fail "")


quoteMathPat :: String -> TH.PatQ
quoteMathPat s =  do  loc <- TH.location
                      let pos =  (TH.loc_filename loc,
                                 fst (TH.loc_start loc),
                                 snd (TH.loc_start loc))
                      expr <- parsePat pos s
                      return expr

quoteMathExp :: String -> TH.ExpQ
quoteMathExp = fail "no expression support for math right now"

