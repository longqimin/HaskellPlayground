{-# LANGUAGE PackageImports #-}

import "parsec3" Text.Parsec
import "parsec3" Text.Parsec.Char

simple :: Parsec String () Char
simple = letter

openClose :: Parsec String () Char
openClose = do {
	char '('
	; char ')'
}

parens :: Parsec String () ()
parens = do {
	char '('
	; parens
	; char ')'
	; parens
	}
	<|> return ()

testOr :: Parsec String () String
testOr = string "(a)"
		<|> string "(b)"

testOr1 :: Parsec String () String
testOr1 = try (string "(a)")
		<|> string "(b)"

nesting :: Parsec String () Int 
nesting = do {
	char '('
	; n <- nesting
	; char ')'
	; m <- nesting
	; return (max (n+1) m)
	}
	<|> return 0

word :: Parsec String () String
word = many1 letter <?> "word"

sentence :: Parsec String () [String]
sentence = do {
	ws <- sepBy1 word separator
	; oneOf ".?!" <?> "end of sentence"
	; return ws
}

separator :: Parsec String () ()
separator = skipMany1 (space <|> char ',' <?> "")

run p input = 
	case parse p "" input of
		Left err -> do
			putStr "parse error at"
			print err
		Right x -> print x

