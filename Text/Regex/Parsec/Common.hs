{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Common supports the Lazy Parsec backend.  It defines all the data
-- types except Pattern and exports everything but the contructors of
-- Pattern.
module Text.Regex.Parsec.Common where

import Data.IntMap (IntMap)
import Text.ParserCombinators.Parsec (GenParser)
import Text.Regex.Parsec.Pattern (Pattern)

-- | 'RegexOption' control whether the pattern is multiline or
-- case-sensitive like Text.Regex and whether to capture the subgroups
-- (\1, \2, etc).
data CompOption = CompOption
  { multiline :: Bool,
    caseSensitive :: Bool,
    captureGroups :: Bool,
    strategy :: RegexOptionStrategy
  }

data RegexOptionStrategy
  = Find_LongestMatch
  | Find_FirstLeft
  | Find_FirstRight
  | Find_All

newtype ExecOption = ExecOption ()

{-
-- | This is a convenience value of RegexOption with multiline,
-- caseSensitive, and captureGroups all True and longestMatch False.
defaultRegexOption :: RegexOption
defaultRegexOption = RegexOption {multiline = True
                                 ,caseSensitive = True
                                 ,captureGroups = True
                                 ,strategy = Find_LongestMatch
                                 }
-}

-- | 'FullState' is the opaque data type used to hold the RegexParser
-- state and a user defined state.
data FullState userStateType = FullState
  { userState :: !userStateType,
    accepted :: !Int,
    openSub :: !(IntMap (String, Int)),
    closedSub :: !MatchedStrings,
    posixSub :: !Opened
  }

data Opened = Opened [Closed] Int (String, Int) Opened | EndOpened [Closed] deriving (Show)

data Closed = Closed [Closed] Int (String, (Int, Int)) deriving (Show)

-- | 'MatchedStrings' is an IntMap where the keys are PatternIndex
-- numbers and the values are completed substring captures.
--
-- This has now been augmented to also remember the offset and length
-- of the matched string.
type MatchedStrings = IntMap (String, (Int, Int))

-- | 'RegexParser' is the type of CharParser that uses the state from this module.
type RegexParser userState = GenParser Char (FullState userState)

type RegexP = RegexParser () [MatchedStrings]

type RegexPS s = RegexParser s [MatchedStrings]

type BoolMultiline = Bool

type BoolCaseSensitive = Bool

type StringRegex = String

type StringInput = String

type StringBeforeMatch = String

type StringOfMatch = String

type StringAfterMatch = String

type StringSubgroups = String

type StringSubPattern = String

type AboutMatch = (StringBeforeMatch, StringOfMatch, StringAfterMatch, [StringSubgroups])

-- | Datatype memo-izing the parsed regular expression and meta-data
data Regex = Regex
  { asString :: StringRegex,
    asPattern :: Pattern,
    capture :: RegexP,
    capture' :: RegexPS ShowS,
    noCapture :: RegexP,
    noCapture' :: RegexPS ([String] -> [String]),
    userInt :: RegexPS Int,
    allMatches :: RegexP,
    frontAnchor :: Bool,
    groups :: Int
  }
