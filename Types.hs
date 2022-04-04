module Types
	( TweeHeader(..)
	, TweePassage(..)
	, TweeData(..)
	, TiddlerData(..)
	) where

import Data.Map (Map)
import Data.Set (Set)

data TweeHeader = TweeHeader String (Set String)
	deriving (Eq, Ord, Show, Read)
data TweePassage = TweePassage TweeHeader String
	deriving (Eq, Ord, Show, Read)

data TweeData = TweeData
	{ _envpath :: FilePath
	, _target :: String
	, _story :: String
	, _passages :: Map String TweePassage
	, _plugins :: [String]
	}
	deriving (Eq, Ord, Show, Read)

data TiddlerData = TiddlerData
	{ _time :: String
	, _author :: String
	, _rss :: Bool
	}
	deriving (Eq, Ord, Show, Read)
