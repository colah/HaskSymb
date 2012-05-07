{-# LANGUAGE MultiParamTypeClasses, ViewPatterns, TupleSections #-}

module M.Parser(mexpr, parsePat) where

import Prelude hiding (const)
import Definitions
import M.PrePat
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Language.Haskell.TH

sepBy2 seg sep = do
	x <- seg
	sep
	xs <- sepBy1 seg sep
	return (x:xs)

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

mexpr n@5 = 
	(try $ do
			many space
			a <- many1 digit
			many space
			return $ const (read a :: Integer) 
	) <|> (try $ do 
			many space
			a <- many1 letter
			many space
			return $ var a
	) <|> (try $ do
		char '('
		many space
		a <- mexpr 0
		many space
		char ')'
		many space
		return a
	)

-- Should use something like sepBy2 ... but it doesn't exist!!!
mexpr n@4 =
	( try $ do
		a <- sepBy2 (mexpr (n+1)) (many space >> char '*' >> many space)
		return $ prodS a
	) <|> (mexpr (n+1))

mexpr n@3 =
	( try $ do
		a <- sepBy2 (mexpr (n+1)) (many space >> char '+' >> many space)
		return $ sumS a
	) <|> (mexpr (n+1))

mexpr 0 = do
	many space
	a <- mexpr 3
	many space
	return a
