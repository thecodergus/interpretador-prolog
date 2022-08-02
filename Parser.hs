{-# LANGUAGE NoMonadFailDesugaring #-} -- "Only the hell stops me" -Gus
 
module Parser (
	Termo (Atomo, Numero, Variavel, Composto),
	Regra (Regra),
	Programa,
	parse,
	termo,
	programa
) where

import Data.Char
import Data.List
import Control.Monad
import Control.Applicative

data Termo = Atomo String
			| Numero Int
			| Variavel String
			| Composto String [Termo]

data Regra = Regra Termo [Termo]

type Programa = [Regra]

instance Show Termo where
	show (Atomo a) = a
	show (Numero n) = show n
	show (Variavel x) = x
	show termo@(Composto f args) =
		if f == "."  
			then showList' termo
			else f ++ "(" ++ (intercalate ", " [ show arg | arg <- args ]) ++ ")"
			

instance Show Regra where
	show (Regra head body) = (show head) ++ (
												if body /= [] 
													then " :- "
													else ""													
											) ++ intercalate ", " (map show body) ++ "."

showList' :: Termo -> String
showList' list = "[" ++ showList'' list ++ "]"

showList'' :: Termo -> String
showList'' (Composto "." [head, Variavel v]) = (show head) ++ "|" ++ v
showList'' (Composto "." [head, tail]) = (show head) ++ (
															if tail /= Atomo "[]" 
																then ", "
																else ""
														) ++ showList'' tail

instance Eq Termo where
	(==) a b = (show a) == (show b)

data Parser a = Parser (String -> [(a, String)])

instance Functor Parser where
	fmap = liftM

instance Applicative Parser where
	pure v = Parser (\i -> [(v, i)])
	(<*>) = ap

instance Monad Parser where
	p >>= f = Parser(
						\cs -> concat [parse' (f val) cs' | (val, cs') <- parse' p cs]
					)
	return = pure

instance MonadPlus Parser where
	mzero = Parser (\i -> [])
	mplus p q = Parser (\i -> parse' p i ++ parse' q i)

instance Alternative Parser where
	empty = mzero
	(<|>) = mplus
	many p = some p `mplus` return []
	some p = do
		a <- p
		as <- many p
		return (a:as)

parse :: Parser a -> String -> a
parse (Parser f) i = fst $ head $ f i

parse' :: Parser a -> String -> [(a, String)]
parse' (Parser f) i = f i

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do
					char c
					string cs
					return (c:cs)

item :: Parser Char
item = Parser (
				\i -> case i of
					[] -> []
					(c:cs) -> [(c, cs)]
			)

sat :: (Char -> Bool) -> Parser Char
sat pred = do
			c <- item
			if pred c 
				then return c 
				else mzero

sat2 :: (Char -> Bool) -> (Char -> Bool) -> Parser String
sat2 initChar insideChar = do
							c <- sat initChar
							cs <- many (sat insideChar)
							return (c:cs)

space :: Parser String
space = many (sat isSpace)

triml :: Parser a -> Parser a
triml p = do
			space
			r <- p
			return r

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = do
				a <- p
				as <- many (do{
								sep;
								p
								})
				return (a:as)

dot = triml $ char '.'
comma = triml $ char ','

argList1 :: Parser a -> Parser [a]
argList1 p = p `sepBy` comma

argList :: Parser a -> Parser [a]
argList p = argList1 p <|> return []

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

smiley = triml $ string ":-"

var :: Parser Termo
var = fmap Variavel $ sat2 isUpper isIdentChar

atomo :: Parser Termo
atomo = fmap Atomo $ sat2 (\c -> isLower c || c == '_') (\c -> isIdentChar c || c == '_')

numero :: Parser Termo
numero = fmap (Numero . read) (some (sat isNumber))

com :: Parser Termo
com = fmap (uncurry Composto) $ do
									Atomo f <- triml atomo
									char '('
									args <- argList1 $ triml termo
									triml $ char ')'
									return (f, args)

emptyList :: Termo
emptyList = Atomo "[]"

termo :: Parser Termo
termo = list <|> com <|> atomo <|> var <|> numero

rule :: Parser Regra
rule = fmap (uncurry Regra) $ do
								ruleHead <- termo
								body <- (do {
												smiley;
												argList termo
											} <|> return [])
								dot
								return (ruleHead, body)

programa :: Parser Programa
programa = do
			space
			rs <- many rule
			return rs

list :: Parser Termo
list = do
		char '['
		do {
			triml $ char ']';
			return emptyList
		} <|> list'

list' :: Parser Termo
list' = fmap (uncurry Composto) $ do
									head <- listHead
									tail <- listTail
									return (".", [head, tail])

list'' :: Parser Termo
list'' = do
			triml $ char ']'
			return emptyList

listHead :: Parser Termo
listHead = triml termo <|> list''

listTail :: Parser Termo
listTail = do {
				triml $ char '|';
				triml list <|> listTailVar
				} <|> do {
							triml $ char ',';
							list'
						} <|> list''

listTailVar :: Parser Termo
listTailVar = do
				var <- triml var
				char ']'
				return var