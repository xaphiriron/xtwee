module Main (main) where

import Control.Applicative
import Control.Monad (liftM)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (mconcat)

import System.Environment (getArgs)
import System.Environment.Executable (getExecutablePath)
import System.Console.GetOpt
import System.Directory (doesDirectoryExist, doesFileExist)
import Data.List (intercalate, dropWhileEnd)
import Data.Char (toLower)

import Data.Time (formatTime, getCurrentTime, defaultTimeLocale)

import qualified Data.Set as Set
import Text.Parsec (ParseError)

import Text.Regex.Posix ((=~), MatchText)
import Data.Array ((!), bounds, inRange)

import Types
import Parser

import Debug.Trace

-- implemented: author, merge, plugin (albeit w/ differences compared to the old twee functionality), target, help
-- todo, from Twine 1.4: -o --obfuscate
data Flags = Author | Merge | Plugin | Rss | Target | Help
	deriving (Eq)

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
			merge <- case lookup Merge opts of
				Nothing -> return ""
				Just mergepath -> do
					readFile $ concat [envpath, "/", mergepath]
			now <- getCurrentTime
			let tiddlerData = TiddlerData
				{ _time = formatTime defaultTimeLocale "%Y%m%d%H%M" now
				, _author = fromMaybe "twee" $ lookup Author opts
				, _rss = isJust $ lookup Rss opts
				}

			mstory <- processFiles tiddlerData files

			case mstory of
				Left err -> putStrLn $ show err
				Right story -> do
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
					putStrLn $ merge ++ template
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

processFiles :: TiddlerData -> [FilePath] -> IO (Either ParseError String)
processFiles tiddlerData files = do
	fileData <- sequence (readFile <$> files)
	return $ (\tweeData -> toTiddlyHtml tiddlerData =<< concat tweeData) <$>
		(sequence $ uncurry parseTweeFile <$> zip files fileData)


explode :: Eq a => a -> [a] -> [[a]]
explode b cs =
	let (a, rs) = break (== b) cs
	in a : if null rs then [] else explode b $ drop 1 rs

trim :: [Char] -> String -> String
trim rm = dropWhileEnd (`elem` rm) . dropWhile (`elem` rm)

parseTemplate :: TweeData -> String -> IO String
parseTemplate twee template = do
	let (prev, match, unparsed) = template =~ "\"([A-Z]+)\"" :: (String, String, String)
	if null match
		then return template
		else
			liftM mconcat . sequence $
				[ return prev
				, loadTemplateSection twee . trim "\"" $ match
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

toTiddlyHtml :: TiddlerData -> TweePassage -> String
toTiddlyHtml t (TweePassage (TweeHeader title tags) content) = if "Twine.private" `elem` tags
	then ""
	else concat
		[ "<div tiddler=\""
		, title
		, "\" tags=\""
		, unwords $ Set.toList tags
		, "\" modified=\""
		, _time t
		, "\" created=\""
		, _time t
		, "\" modifier=\""
		, _author t
		, "\">"
		, tiddlyEscape . trim "\n\t\r" $ content
		, "</div>"
		]
	where
		tiddlyEscape :: String -> String
		tiddlyEscape = (escape =<<)
			where
				escape :: Char -> [Char]
				escape '<' = "&lt;"
				escape '>' = "&gt;"
				escape '"' = "&quot;"
				escape '\n' = "\\n"
				escape c = pure c
