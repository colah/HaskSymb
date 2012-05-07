{-# LANGUAGE MultiParamTypeClasses, ViewPatterns, TupleSections #-}


module M.QQ(m, satisfy) where

-- Based on http://www.haskell.org/haskellwiki/Quasiquotation

import Definitions
import M.View
import M.Parser
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

