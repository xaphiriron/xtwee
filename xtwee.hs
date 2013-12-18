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

-- implemented: author, merge, plugin (albeit w/ differences compared to the old twee functionality), target, help
data Flags = Author | Merge | Plugin | Rss | Target | Help
	deriving (Show, Read, Eq)
data TweeData = TweeData
	{ _envpath :: FilePath
	, _target :: String
	, _story :: String
	, _plugins :: [String]
	}

data TiddlerData = TiddlerData
	{ _time :: String
	, _author :: String
	, _rss :: Bool
	}

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
			now <- getCurrentTime
			let tiddlerData = TiddlerData
				{ _time = formatTime defaultTimeLocale "%Y%m%d%H%M" now
				, _author = fromMaybe "twee" $ lookup Author opts
				, _rss = isJust $ lookup Rss opts
				}
			story <- liftM concat .
				fmap (fmap $ tiddle tiddlerData) .
					sequence . fmap readFile $
						files
			let twee = TweeData
				{ _envpath = envpath
				, _target = fromMaybe "jonah" $ lookup Target opts
				, _story = story
				, _plugins = ("engine" :) . explode ',' .
					fromMaybe "" $ lookup Plugin opts
				}
			header <- readFile $
				concat [envpath, "/targets/", _target twee, "/header.html"]
			template <- parseTemplate twee header
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

parseTemplate :: TweeData -> String -> IO String
parseTemplate twee template = do
	let (prev, match, unparsed) = template =~ "\"([A-Z]+)\"" :: (String, String, String)
	if null match
		then return template
		else
			liftM mconcat . sequence $
				[ return prev
				, loadTemplateSection twee $ dropWhileEnd (== '"') . dropWhile (== '"') $ match
				, parseTemplate twee unparsed
				]

loadTemplateSection :: TweeData -> String -> IO String
loadTemplateSection t match
	| match == "VERSION" = return "Made in xtwee 1.0, with Twine core 1.4"
	| match == "STORY" = return $ _story t
	| otherwise = do
		let lmatch = toLower <$> match
		dir <- doesDirectoryExist $ concat [_envpath t, "/targets/", lmatch]
		file <- doesFileExist $ concat [_envpath t, "/targets/", _target t, "/", lmatch, ".js"]
		genfile <- doesFileExist $ concat [_envpath t, "/targets/", lmatch, ".js"]
		case (dir, file, genfile, lmatch `elem` _plugins t) of
			(True, _, _, _) -> readFile $ concat [_envpath t, "/targets/", lmatch, "/code.js"]
			(_, _, _, False) -> return "" -- plugin disabled
			(_, True, _, _) -> readFile $ concat [_envpath t, "/targets/", _target t, "/", lmatch, ".js"]
			(_, _, True, _) -> readFile $ concat [_envpath t, "/targets/", lmatch, ".js"]
			_ -> error $ "xtwee: no match to " ++ match ++ " in template file"

tiddle :: TiddlerData -> String -> String
tiddle t s = concat
	[ "<div tiddler=\"untitled passage\">"
	, joinLines . tiddle' t . tiddlyEscape $ s
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
tiddle' :: TiddlerData -> String -> String
tiddle' t story =
	let (prev, match, unparsed) =
		story =~ "^::([^\\|\\[]+)(\\s+\\[(.*)\\])?\\s*$" :: (String, MatchText String, String)
	in
		if bounds match == (1, 0)
			then story
			else
				concat
					[ prev
					, tiddler t match
					, tiddle' t . drop 1 $ unparsed -- to strip the next newline (which we know matched b/c of the $ in the regex)
					]

tiddler :: TiddlerData -> MatchText String -> String
tiddler t match =
	concat
		[ "</div>"
		, "<div tiddler=\""
		, fst $ match ! 1
		, "\" tags=\""
		, if bounds match `inRange` 3
			then fst $ match ! 3
			else ""
		, "\" modified=\""
		, _time t
		, "\" created=\""
		, _time t
		, "\" modifier=\""
		, _author t
		, "\">"
		]
