module Main (main) where

import System.Environment (getArgs)
import System.Environment.Executable (getExecutablePath)
import System.Console.GetOpt
import System.Directory (doesDirectoryExist, doesFileExist)
import Control.Applicative
import Control.Monad (liftM)
import Data.List (intercalate, dropWhileEnd)
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import Data.Char (toLower)
import Data.Time
import System.Locale (defaultTimeLocale)
import Text.Regex.Posix ((=~), MatchText)
import Data.Array ((!), bounds, inRange)

-- implemented: target, merge, help
data Flags = Author | Merge | Plugin | Rss | Target | Help
	deriving (Show, Read, Eq)

main :: IO ()
main = do
	(opts, files, errs) <-
		liftM (getOpt RequireOrder options) getArgs
	path <- getExecutablePath
	let envpath = intercalate "/" . init . explode '/' $ path
	case ((not . null $ errs) || (Help, []) `elem` opts, null files) of
		(True, _) -> putStrLn $ usageInfo (intercalate "\n" errs) options
		(_, True) -> putStrLn "xtwee: no source files specified"
		_ -> do
			case lookup Merge opts of
				Nothing -> return ()
				Just mergepath -> do
					merge <- readFile $ concat [envpath, "/", mergepath]
					putStrLn merge
			let author = fromMaybe "twee" $ lookup Author opts
			now <- getCurrentTime
			let time = formatTime defaultTimeLocale "%Y%m%d%H%M" now
			story <- liftM concat .
				fmap (fmap $ tiddle time author) .
					sequence . fmap readFile $
						files
			let target = fromMaybe "jonah" $ lookup Target opts
			header <- readFile $
				concat [envpath, "/targets/", target, "/header.html"]
			template <- parseTemplate envpath target story header
			putStrLn template
	where
		options :: [OptDescr (Flags, String)]
		options =
			[ Option "a" ["author"] (ReqArg ((,) Author) "twee")
				"sets TiddlyWiki author string"
			, Option "m" ["merge"] (ReqArg ((,) Merge) "foo.html")
				"include given file in output"
			, Option "p" ["plugin"] (ReqArg ((,) Plugin) "jquery")
				"include additional plugin files"
			, Option "r" ["rss"] (NoArg (Rss, []))
				"generate RSS output, rather than HTML"
			, Option "t" ["target"] (ReqArg ((,) Target) "jonah")
				"select target template"
			, Option [] ["help"] (NoArg (Help, []))
				"this help string"
			]

explode :: Eq a => a -> [a] -> [[a]]
explode b cs =
	let (a, rs) = break (== b) cs
	in a : if null rs then [] else explode b $ drop 1 rs

parseTemplate :: FilePath -> String -> String -> String -> IO String
parseTemplate envpath target story template = do
	let (prev, match, unparsed) = template =~ "\"([A-Z]+)\"" :: (String, String, String)
	if null match
		then return template
		else
			liftM mconcat . sequence $
				[ return prev
				, loadTemplateSection envpath target story $ dropWhileEnd (== '"') . dropWhile (== '"') $ match
				, parseTemplate envpath target story unparsed
				]

loadTemplateSection :: FilePath -> String -> String -> String -> IO String
loadTemplateSection envpath target story match
	| match == "VERSION" = return "xtwee 1.0, w/ Twine core 1.4"
	| match == "STORY" = return story
	| otherwise = do
		let lmatch = toLower <$> match
		dir <- doesDirectoryExist $ concat [envpath, "/targets/", lmatch]
		file <- doesFileExist $ concat [envpath, "/targets/", target, "/", lmatch, ".js"]
		genfile <- doesFileExist $ concat [envpath, "/targets/", lmatch, ".js"]
		case (dir, file, genfile) of
			(True, _, _) -> readFile $ concat [envpath, "/targets/", lmatch, "/code.js"]
			(_, True, _) -> readFile $ concat [envpath, "/targets/", target, "/", lmatch, ".js"]
			(_, _, True) -> readFile $ concat [envpath, "/targets/", lmatch, ".js"]
			_ -> error $ "xtwee: no match to " ++ match ++ " in template file"

tiddle :: String -> String -> String -> String
tiddle now author s = concat
	[ "<div tiddler=\"untitled passage\">"
	, joinLines . tiddle' now author . tiddlyEscape $ s
	, "</div>"
	]
	where
		joinLines =
			concat . fmap (\c -> if c == '\n' then "\\n" else pure c)

tiddlyEscape :: String -> String
tiddlyEscape = concat . fmap escape
	where
		escape '<' = "&lt;"
		escape '>' = "&gt;"
		escape '"' = "&quot;"
		escape c = pure c

-- this is terrifying garbage. there's a correct way to do this and this isn't it (this leads to: spurious empty "untitled passage" tiddlers; maybe other issues)
-- todo: break story string into (title line, passage text) chunks, with anything before the first chunk living in an automatic "untitled passage" chunk, then `concat . fmap toTiddler` them. toTiddler would also give a good place to do all the escaping, rather than having to do it in two passes as current (b/c we have to escape html before writing our own html but we can't escape newlines until after we've used them to find the `::passage headers`)
tiddle' :: String -> String -> String -> String
tiddle' now author story =
	let (prev, match, unparsed) =
		story =~ "^::([^\\|\\[]+)(\\s+\\[(.*)\\])?\\s*$" :: (String, MatchText String, String)
	in
		if bounds match == (1, 0)
			then story
			else
				concat
					[ prev
					, tiddler now author match
					, tiddle' now author $ drop 1 unparsed -- to strip the next newline (which we know matched b/c of the $ in the regex)
					]

tiddler :: String -> String -> MatchText String -> String
tiddler now author match =
	concat
		[ "</div>"
		, "<div tiddler=\""
		, fst $ match ! 1
		, "\" tags=\""
		, if bounds match `inRange` 3
			then fst $ match ! 3
			else ""
		, "\" modified=\""
		, now
		, "\" created=\""
		, now
		, "\" modifier=\""
		, author
		, "\">"
		]
