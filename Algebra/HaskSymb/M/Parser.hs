{-# LANGUAGE MultiParamTypeClasses, ViewPatterns, FlexibleContexts, ScopedTypeVariables, Rank2Types, NoMonomorphismRestriction #-}

module Algebra.HaskSymb.M.Parser (parsePat) {-(mexpr, parsePat)-} where

import Prelude hiding (const)
import Algebra.HaskSymb.Definitions
import Algebra.HaskSymb.M.PrePat
import Text.Parsec
import Text.ParserCombinators.Parsec.Expr
import Language.Haskell.TH

-- The obvious generalization of sepBy and sepBy1
sepBy2 seg sep = do
	x <- seg
	sep
	xs <- sepBy1 seg sep
	return (x:xs)



-- essentially from the aformentione quasiquoter tutorial

parsePat :: (String, Int, Int) -> String -> PatQ
parsePat (file, line, col) s =
    case runParser p () "" s of
      Left err  -> fail $ show err
      Right e   -> e
  where
    p = do  pos <- getPosition
            setPosition $
              (flip setSourceName) file $
              (flip setSourceLine) line $
              (flip setSourceColumn) col $
              pos
            many space;
            e  <- mexpr';
			many space;
            eof
            return e



-- The 'n' argument is the fixity level we are at

data Proxy = Proxy
	deriving Eq

instance Symbolic Integer Proxy where
	constD _ = Nothing
	varD _ = Nothing
	constC _ = Proxy
	varC _ = Proxy

instance SymbolicSum Proxy where
	sumD _ = Nothing
	sumC _ = Proxy

instance SymbolicProd Proxy where
	prodD _ = Nothing
	prodC _ = Proxy

mexpr' :: Parsec [Char] st (Q Pat)
mexpr' = do
	a :: SymbPat Proxy <- mexpr 0
	return (makePat a)


mexpr :: (Symbolic Integer a, SymbolicSum a, SymbolicProd a, Eq a) =>
			Int -> Parsec [Char] st (SymbPat a)

mexpr n@5 = 
	(try $ do
			many space
			a <- many1 digit
			many space
			return $ constC (read a :: Integer) 
	) <|> (try $ do 
			many space
			a <- many1 letter
			many space
			return $ varC a
	) <|> (try $ do
		char '('
		many space
		a <- mexpr 0
		many space
		char ')'
		many space
		return a
	)

mexpr n@4 =
	( try $ do
		a <- sepBy2 (mexpr (n+1)) (many space >> char '*' >> many space)
		return $ prodC a
	) <|> (mexpr (n+1))

mexpr n@3 =
	( try $ do
		a <- sepBy2 (mexpr (n+1)) (many space >> char '+' >> many space)
		return $ sumC a
	) <|> (mexpr (n+1))

mexpr 0 = do
	many space
	a <- mexpr 3
	many space
	return a
