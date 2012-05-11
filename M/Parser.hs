{-# LANGUAGE MultiParamTypeClasses, ViewPatterns, TupleSections #-}

module M.Parser(mexpr, parsePat) where

import Prelude hiding (const)
import Definitions
import M.PrePat
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Language.Haskell.TH

-- The obvious generalization of sepBy and sepBy1
sepBy2 seg sep = do
	x <- seg
	sep
	xs <- sepBy1 seg sep
	return (x:xs)

-- essentially from the aformentione quasiquoter tutorial
parsePat :: Monad m => (String, Int, Int) -> String -> m Pat
parsePat (file, line, col) s =
    case runParser p () "" s of
      Left err  -> fail $ show err
      Right e   -> return (makePat e)
  where
    p = do  pos <- getPosition
            setPosition $
              (flip setSourceName) file $
              (flip setSourceLine) line $
              (flip setSourceColumn) col $
              pos
            many space;
            e <- mexpr 0;
			many space;
            eof
            return e

-- The 'n' argument is the fixity level we are at

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
