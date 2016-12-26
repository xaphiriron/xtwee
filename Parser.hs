{-# LANGUAGE FlexibleContexts #-}

module Parser
	( parseTweeFile
	) where

import Text.Parsec hiding (spaces)
import qualified Text.Parsec as Parsec (spaces)

import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe
import Data.List (dropWhileEnd)

import Types

parseTweeFile :: String -> String -> Either ParseError [TweePassage]
parseTweeFile filename contents = parse twee filename contents

twee :: Stream s m Char => ParsecT s u m [TweePassage]
twee = many tiddler <* eof

tiddler :: Stream s m Char => ParsecT s u m TweePassage
tiddler = TweePassage
	<$> (whitespace *> header)
	<*> (trim "\n\r" <$> manyTill anyChar (try $ lookAhead $ (string "\n::" *> return ()) <|> eof))

header :: Stream s m Char => ParsecT s u m TweeHeader
header = TweeHeader
	<$> (string "::" *> word)
	<*> (spaces *> (fromMaybe mempty <$> optionMaybe tags))

tags :: Stream s m Char => ParsecT s u m (Set String)
tags = fmap Set.fromList $ between (char '[') (char ']')
	$ (many1 $ noneOf " \n\t\r]") `sepBy` (many $ oneOf " \t")

word :: Stream s m Char => ParsecT s u m String
word = many1 $ noneOf " \n\t\r"

spaces :: Stream s m Char => ParsecT s u m ()
spaces = many (oneOf " \t") *> return ()

whitespace :: Stream s m Char => ParsecT s u m ()
whitespace = Parsec.spaces

trim :: [Char] -> String -> String
trim rm = dropWhileEnd (`elem` rm) . dropWhile (`elem` rm)
