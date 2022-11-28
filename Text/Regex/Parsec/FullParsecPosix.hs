-- |
-- This is a cloned copy of FullParsec which is being used to explore how
-- to change from PCRE leftmost semantics to posix longest semantics.
--
-- The other semantics are being ripped out to clarify things.
--
-- First: Need to fix this bug with capturing:
-- *Text.Regex.Impl.Test> fiddle "ba" "((a)*(b)*)*"
-- "PCRE"
-- "(0,2)(2,2)(1,2)(0,1)"
-- "Parsec/pcre"
-- "(0,2)(2,2)(1,2)(0,1)"
-- "PosixRE"
-- "(0,2)(2,2)(-1,-1)(-1,-1)"
-- "Parsec/posix"
-- "(0,2)(2,2)(1,2)(0,1)"
-- "TRE"
-- "(0,2)(1,2)(1,2)(-1,-1)"
--
-- *Text.Regex.Impl.Test> fiddle' "ba" "((a)*(b)*)*"
-- "PCRE"
-- ("","ba","",["","a","b"])
-- "Parsec/pcre"
-- ("","ba","",["","a","b"])
-- "PosixRE"
-- ("","ba","",["","",""])
-- "Parsec/posix"
-- ("","ba","",["","a","b"])
-- "TRE"
-- ("","ba","",["a","a",""])
--
-- These show the backref is cleared as soon as the parent group is reopened, unlike PCRE:
--
-- *Text.Regex.Impl.Test> fiddle' "aaabac" "((a)*b|\\2)*"
-- "PCRE"
-- ("","aaaba","c",["a","a"])
-- "Parsec/pcre"
-- ("","aaaba","c",["a","a"])
-- "PosixRE"
-- ("","aaab","ac",["aaab","a"])
-- "Parsec/posix"
-- ("","aaaba","c",["a","a"])
-- "TRE"
-- ("","aaab","ac",["aaab","a"])
--
-- *Text.Regex.Impl.Test> fiddle' "aaabac" "((b\\3c)|(a)*)*"
-- "PCRE"
-- ("","aaabac","",["","bac","a"])
-- "Parsec/pcre"
-- ("","aaabac","",["","bac","a"])
-- "PosixRE"
-- ("","aaa","bac",["","",""])
-- "Parsec/posix"
-- ("","aaabac","",["","bac","a"])
-- "TRE"
-- ("","aaa","bac",["aaa","","a"])
--
-- This means keeping track of the nesting of sub-expressions (the PGroup
-- patterns).  Everything is okay until a PGroup which was previously
-- captures is opened.  Now we need to destroy all the nested captures.
--
-- The current state is interesting:
--
-- data Closed = Closed i data [Closed]
-- data Opened = Opened i data [Closed] (Maybe Opened)
--
-- Initially it is (ignoring the data):
-- Opened 0 [] Nothing
-- When a group #1 is opened this is updated to
-- Opened 0 [] (Just (Opened 1 [] Nothing))
-- Then a subgroup #2 is opened
-- Opened 0 [] (Just (Opened 1 [] (Just Opened 2 [] Nothing)))
-- It should now be impossible to close #1.  This is a dynamic property.
-- This #2 is later closed:
-- Opened 0 [] (Just (Opened 1 [Closed 2 []] Nothing))
-- If #2 is opened, then go back to the previous step, else close #1 and get
-- Opened 0 [Closed 1 [Closed 2 []]] Nothing
-- It should now be impossible for #2 to be re-opened.  This is a dynamic property.
-- If #1 is re-opened then it's closed list is supposed to be cleared:
-- Opened 0 [] (Just (Opened 1 [] Nothing))
--
-- data Closed = Closed i data [Closed]
-- data Opened = Opened i data [Closed] Opened
--             | End [Closed]
--
-- Consider the nesting:
-- 0 1 2 2 3 3 1 4 5 5 4 0  <= group number
-- ( ( ( ) ( ) ) ( ( ) ) )
--  0 1 2 3 4 5 6 7 8 9 A B <= stage index below
--
-- data Closed = Closed [Closed] i (String,(offset,length))
-- data Opened = Opened [Closed] i (String,(offset)) Opened
--             | End [Closed]
--
-- Try it with the reverse nesting and re-ordering the arguments...
-- 0 Opened [] 0 (End [])
-- 1 Opened [] 1 (Opened [] 0 (End []))
-- 2 Opened [] 2 (Opened [] 1 (Opened [] 0 (End [])))
-- 3 Opened [Closed [] 2] 1 (Opened [] 0 (End []))
-- 4 Opened [] 3 (Opened [Closed [] 2] 1 (Opened [] 0 (End [])))
-- 5 Opened [Closed [] 3,Closed [] 2] 1 (Opened [] 0 (End []))
-- 6 Opened [Closed [Closed [] 3,Closed [] 2] 1] 0 (End [])
-- 7 Opened [] 4 (Opened [Closed [Closed [] 3,Closed [] 2] 1] 0 (End []))
-- 8 Opened [] 5 (Opened [] 4 (Opened [Closed [Closed [] 3,Closed [] 2] 1] 0 (End [])))
-- 9 Opened [Closed [] 5] 4 (Opened [Closed [Closed [] 3,Closed [] 2] 1] 0 (End [])))
-- A Opened [Closed [Closed [] 5] 4,Closed [Closed [] 3,Closed [] 2] 1] 0 (End [])
-- B End [Closed [Closed [Closed [] 5] 4,Closed [Closed [] 3,Closed [] 2] 1] 0]
-- -- we could open more groups here without problems...
--
-- What could have been closed at any of the stages?
-- Only the outermost Opened group.
--
-- What could have been re-opened at any of the stages?
-- 3 : 2 => 2
-- 5 : 3 (leaving group 2 closed) => 4
-- 6 : 1 (clearing groups 2 and 3) => 1
-- 9 : 5 (leaving groups 1(2 3) closed ) => 8
-- A : 4 (clearing group 5, leaving groups 1(2 3) closed) => 7
-- B : 0 (clearing groups 1(2 3) 4(5)) => 0
--
-- In a more general Pattern, one could imaging repeating subpatterns
-- that are not also subexpressions, but that do contain concatenated
-- subexpressions.  In which case you could also re-open:
-- 5 : 2 (clearing group 3) => 2
-- A : 1 (clearing groups 2 3 4 5) => 1
--
-- Creation:
--
-- The outermost opened group may be turned into a child of a new outermost Opened
--
-- Closing:
--
-- The outermost group may be deleted and it's closed group prepended to it's opened child's outmost group's closed list
--
-- Re-Opening:
--
-- The head of the closed list of the outermost Opened may be removed and turned into a new outermost Opened
-- or more generally (for repeated subpatterns which are not also subexpressions):
-- An element of the closed list of the outermost Opened may be removed (leaving the tail and removing previous elements) and turned into a new outermost Opened.
--
-- Lookup of Back-reference:
--
-- Consider (9).  You are looking for index x:
-- The top Opened is index 4
--   if x>4 then search 4's closed list
--     the first Closed element is index 5
--       if x>5 then then look in (Closed [] 5)'s empty closed list and fail
--       if x<5 then look at the rest of the empty list and fail
--   if x<4 then search the child Opened
--     this Opened is index 0
--     if x>0 then search the closed list
--       the first Closed element is index 3
--         if x>3 then look in (Closed [] 3)'s empty list and fail
--         if x<3 then look at the rest of the list
--           the next Closed element is index 2
--             if x>2 then look in (Closed [] 2)'s empty list and fail
--             if x<2 then look at the rest of the empty list and fail
--     if x<0 then search the child End's empty list and fail
--
-- This module is similar to CompatParsec, but produces a parser with a
-- configurable strategy.  CompatParsec takes all branches and finds the
-- longest match, but FullParsec can also take the branches in left to
-- right order and stop on the first successful match to the full
-- pattern.  This choice is made via the longestMatch field of
-- RegexOption.  To help control the the parser, this module accepts lazy
-- and possessive modifiers to help guide matching.
--
-- Unlike Text.Regex or Text.Regex.Lazy.Compat, NUL characters get no
-- special treatement and are permitted in the string form of regular
-- expressions and in the input to be matched.
--
-- Repetitions of a sub-pattern that accepts an empty string are detected
-- to prevent inifinite looping.  These checks for accepting an empty
-- string are not done if the sub-pattern can be proven to never accept
-- and empty string.
--
-- Capturing sub-group strings is all or nothing at the moment and is
-- controlled by RegexOption.  In neither case is the whole string (group
-- 0) captured.  That can be added by calling initState and finalState
-- before and after the parser returned by patternToParsec.
module Text.Regex.Parsec.FullParsecPosix
  ( patternToParsec,
    patternToParsecCont,
    hasFrontCarat,
  )
where

{- By Chris Kuklewicz, 2006. BSD License, see the LICENSE file. -}

import Control.Monad (liftM, replicateM_ {- ,msum -}, when)
import Control.Monad.Fix (fix)
import Data.Char (toLower, toUpper)
import qualified Data.IntMap as I
import Data.List (nub, sort)
import qualified Data.Set as Set (toList)
import Text.ParserCombinators.Parsec
  ( anyChar,
    char,
    eof,
    getParserState,
    getPosition,
    lookAhead,
    noneOf,
    oneOf,
    optional,
    pzero,
    setParserState,
    sourceColumn,
    sourceLine,
    string,
    try,
    unexpected,
    (<|>),
  )
import Text.Regex.Parsec.Common
  ( CompOption (..),
    MatchedStrings,
    RegexOptionStrategy (..),
    RegexParser,
  )
import Text.Regex.Parsec.Pattern
  ( Pattern (..),
    cannotMatchNull,
    hasFrontCarat,
    simplify,
  )
import Text.Regex.Parsec.ReadRegex (decodePatternSet)
import Text.Regex.Parsec.RegexParsecState
  ( eqSubs,
    finalStateP,
    incState,
    initStateP,
    lookupAccepted,
    lookupSubP,
    lookupSubsP,
    plusState,
    startSubP,
    stopSubP,
  )

-- | This applies 'simplify' to the provided pattern and wraps the
-- parsec parser in initState and finalState so that the whole
-- matching string is assigned to group 0.
--
-- The returned parser does nothing to the user state.
--
-- For ill-formed patterns this may call 'error', such as for PBound
-- values with negative mino or max or with min > max.  It is also an
-- error if the Pattern contains back references but the captureGroups
-- RegexOption is set to False.  It is also an error if PLazy is
-- applied to anything but PQuest, PPlus, PStar, or PBound.
patternToParsec :: CompOption -> Pattern -> RegexParser userState [MatchedStrings]
patternToParsec opt p = do
  initStateP
  patternToParsecCont
    opt
    (simplify p)
    ( do
        f <- finalStateP
        return [f]
    )

-- | This takes an option structure and a Pattern and parser to act as
-- the continuation of the parser created from the Pattern.  This is
-- used to build patternToParsec.
--
-- The returned parser does nothing to the user state.
--
-- For ill-formed patterns this may call 'error', such as for PBound
-- values with negative mino or max or with min > max.  It is also an
-- error if the Pattern contains back references but the captureGroups
-- RegexOption is set to False.  It is also an error if PLazy is
-- applied to anything but PQuest, PPlus, PStar, or PBound.
patternToParsecCont ::
  CompOption ->
  Pattern ->
  RegexParser userState [b] ->
  RegexParser userState [b]
patternToParsecCont
  ( CompOption
      { multiline = multi,
        caseSensitive = sensitive,
        captureGroups = captureG,
        strategy = find
      }
    ) = reflectParsec
    where
      reflectParsec :: Pattern -> RegexParser userState [b] -> RegexParser userState [b]
      reflectParsec pIn cont =
        case pIn of
          PEmpty -> cont
          PCarat ->
            if multi
              then do
                col <- liftM sourceColumn getPosition
                when (1 /= col) (unexpected "Not anchored at start of line")
                cont
              else do
                pos <- getPosition
                let (line, col) = (sourceLine pos, sourceColumn pos)
                when (1 /= line || 1 /= col) (unexpected "Not anchored at start of input")
                cont
          PDollar ->
            if multi
              then (lookAhead ((char '\n' >> return ()) <|> eof)) >> cont
              else eof >> cont
          PGroup i p ->
            if captureG
              then startSubP i >> reflectParsec p (stopSubP i >> cont)
              else reflectParsec p cont
          -- There should be no empty POr patterns in a well-formed Pattern, but
          -- 'error' is hard to catch so let it slide.
          --    POr [] -> error "Empty POr Pattern"
          POr [] -> cont
          -- Need to make a longest-match version of POr
          POr ps -> case find of
            Find_LongestMatch ->
              let branches = map (\p -> reflectParsec p cont) ps
               in longestMatch branches
            _ -> undefined
          -- There should be no empty PConcat patterns in a well-formed Pattern, but
          -- 'error' is hard to catch so let it slide
          --    PConcat [] -> error "Error PConcat Pattern"
          PConcat [] -> cont
          PConcat ps -> foldr reflectParsec cont ps
          -- Greedy is the default
          PQuest p -> greedyOpt p cont
          PPlus p -> reflectParsec p (greedy p)
          PStar p -> greedy p
          PBound 0 Nothing p -> greedy p
          PBound i Nothing p
            | i > 0 -> exact i p (greedy p)
            | otherwise -> error $ "PBound with invalude parameters: " ++ show i ++ " and Nothing"
          PBound i (Just j) p
            | i == j -> exact i p cont
            | 0 <= i && i < j -> exact i p (greedyTo p (j - i))
            | otherwise -> error $ "PBound with invalude parameters: " ++ show i ++ " and " ++ show j
          -- Lazy
          PLazy (PQuest p) -> lazyOpt p cont
          PLazy (PPlus p) -> reflectParsec p (lazy p)
          PLazy (PStar p) -> lazy p
          PLazy (PBound 0 Nothing p) -> lazy p
          PLazy (PBound i Nothing p)
            | i > 0 -> exact i p (lazy p)
            | otherwise -> error $ "PBound with invalude parameters: " ++ show i ++ " and Nothing"
          PLazy (PBound i (Just j) p)
            | i == j -> exact i p cont
            | 0 <= i && i < j -> exact i p (lazyTo p (j - i))
            | otherwise -> error $ "PBound with invalude parameters: " ++ show i ++ " and " ++ show j
          -- Applying PLazy to non-repeating patterns makes no sense and is an error
          PLazy err -> error $ "PLazy applied to invalid pattern : " ++ show err
          -- Possessive
          PPossessive (PQuest p) -> possessiveOpt p
          PPossessive (PPlus p) -> reflectParsec p (possessive p)
          PPossessive (PStar p) -> possessive p
          PPossessive (PBound 0 Nothing p) -> possessive p
          PPossessive (PBound i Nothing p)
            | i > 0 -> exactPos i p (possessive p)
            | otherwise -> error $ "PBound with invalude parameters: " ++ show i ++ " and Nothing"
          PPossessive (PBound i (Just j) p)
            | i == j -> exactPos i p cont
            | 0 <= i && i < j -> exactPos i p (possessiveTo p (j - i))
            | otherwise -> error $ "PBound with invalude parameters: " ++ show i ++ " and " ++ show j
          -- Applying PPossessive to other patterns makes sense, so instead of
          -- an error...
          --    PPossessive err -> error $ "PPossessive applied to invalid pattern : "++show err
          -- ...the pattern is handled by giving a (reOk) continuation.
          -- This will prevent backtracking to any 'try' statements created by
          -- reflectParsec p once all of p matches.
          PPossessive p -> reflectParsec p (reOk) >> cont
          -- The operations below actually check the input for a match, accept
          -- valid characters, and advance the state
          PDot ->
            if multi
              then parseChar (noneOf "\n") >> cont
              else parseChar anyChar >> cont
          PAny patset ->
            if sensitive
              then
                let chars = Set.toList . decodePatternSet $ patset
                 in parseChar (oneOf chars) >> cont
              else
                let chars = nub . sort $ concatMap ($ Set.toList (decodePatternSet patset)) [map toLower, map toUpper]
                 in parseChar (oneOf chars) >> cont
          PAnyNot patset ->
            if sensitive
              then
                let chars = Set.toList . decodePatternSet $ patset
                 in parseChar (noneOf chars) >> cont
              else
                let chars = nub . sort $ concatMap ($ Set.toList (decodePatternSet patset)) [map toLower, map toUpper]
                 in parseChar (noneOf chars) >> cont
          PEscape c -> acceptChar c >> cont
          PBack i ->
            if captureG
              then do
                maybeSubP <- lookupSubP i
                case maybeSubP of
                  Nothing -> unexpected ("Cannot find subexpression \\" ++ show i)
                  Just sub -> acceptString sub >> cont
              else error "Pattern with back reference used with RegexOption captureGroups set to False"
          PChar c -> acceptChar c >> cont
          PString s -> acceptString s >> cont
        where
          -- Define longestMatch for Find_LongestMatch
          howFar branch =
            lookAhead
              ( do
                  result <- try branch
                  len <- lookupAccepted
                  subs <- lookupSubsP
                  saveGame <- getParserState
                  return (Just (len, subs, (saveGame, result)))
              )
              <|> return Nothing
          -- Who to copy?  I'll choose http://www.boost.org/libs/regex/doc/faq.html
          -- This is also from http://www.opengroup.org/onlinepubs/009695399/xrat/xbd_chap09.html
          -- This implements leftmost-longest rule for each subexpression in order of group #.
          compareSubs s1 s2 =
            let s1s = I.toAscList s1
                s2s = I.toAscList s2
                check [] [] = EQ -- neither is "better" than the other
                check _ [] = GT -- x is more defined, so is "better"
                check [] _ = LT -- y is more defined, so is "better"
                check ((xKey, (_, (xOff, xLen))) : xs) ((yKey, (_, (yOff, yLen))) : ys) =
                  case compare xKey yKey of
                    LT -> GT -- x is more defined, so is "better"
                    GT -> LT -- y is more defined, so is "better"
                    EQ -> case compare xOff yOff of
                      LT -> GT -- x is leftmost, so is "better"
                      GT -> LT -- y is leftmost, so is "bettern
                      EQ -> case compare xLen yLen of
                        GT -> GT -- x is longer, so is "better"
                        LT -> LT -- y is longer, so is "better"
                        EQ -> check xs ys -- x==y, apply recursion
             in check s1s s2s
          longestMatch branches = do
            allFar <- mapM howFar branches
            let best = foldl maxFst Nothing allFar
                maxFst a Nothing = a
                maxFst Nothing b = b
                maxFst a@(Just (aL, aS, _)) b@(Just (bL, bS, _)) =
                  case compare aL bL of
                    GT -> a -- a is longer, so is "better"
                    LT -> b -- b is longer, so is "better"
                    EQ -> case compareSubs aS bS of
                      GT -> a -- aS is "better"
                      LT -> b -- bS is "better"
                      EQ -> a -- break tie in favor of leftmost branch
            case best of
              Nothing -> pzero
              Just (_, _, (saveGame, result)) -> do
                setParserState saveGame
                return result
          {-
                longestMatch branches = do
                  allFar <- mapM howFar branches
                  let best = foldl maxFst Nothing allFar
                      maxFst a Nothing = a
                      maxFst Nothing b = b
                      maxFst a@(Just (aL,_,_)) b@(Just (bL,_,_)) =
                        case compare aL bL of
                          GT -> a -- a is longer, so is "better"
                          LT -> b -- b is longer, so is "better"
                          EQ -> a -- break ties to the left
                  case best of Nothing -> pzero
                               Just (_,_,(saveGame,result)) -> do setParserState saveGame
                                                                  return result
          -}
          -- Define allBranches for Find_All
          (<||>) = case find of
            Find_LongestMatch -> \a b -> longestMatch [a, b]
            _ -> undefined
          -- Provide shortcut to 'cont' when 'cps' matches zero characters and same Subs are in effect
          -- This effectively brackets the matching of cps.
          whenNull cps c = do
            before <- lookupAccepted
            beforeSubs <- lookupSubsP
            cps
              ( do
                  after <- lookupAccepted
                  if after > before
                    then c -- progress
                    else do
                      afterSubs <- lookupSubsP
                      if eqSubs afterSubs beforeSubs
                        then cont -- shortcut
                        else c
              )
          -- p{i} p{i,i}  There is no attempt to short-circuit accepting "" here
          exact i p cont' = foldr reflectParsec cont' (replicate i p)
          exactPos i p cont' =
            let p' = reflectParsec p (reOk)
             in replicateM_ i p' >> cont'
          -- main p? p?* p?+ when you don't worry about accepting ""
          greedyOpt p c = try (reflectParsec p c) <||> cont
          lazyOpt p c = try cont <||> (reflectParsec p c)
          possessiveOpt p = optional (reflectParsec p (reOk)) >> cont
          possessiveOpt' p c = (try (reflectParsec p (reOk)) >> c) <||> cont
          -- helper p? p?* p?+ when you are worried about accepting ""
          greedySafe p c = try (whenNull (reflectParsec p) c) <||> cont
          lazySafe p c = try cont <||> whenNull (reflectParsec p) c
          possessiveSafe p c = whenNull (try (reflectParsec p (reOk)) >>) c
          -- p* p*? p*+
          greedy p = if cannotMatchNull p then fix (greedyOpt p) else fix (greedySafe p)
          lazy p = if cannotMatchNull p then fix (lazyOpt p) else fix (lazySafe p)
          possessive p = if cannotMatchNull p then fix (possessiveOpt' p) else fix (possessiveSafe p)
          -- p{0,n} p{0,n}? p{0,n}+
          useTo n use = foldr ($) cont (replicate n use)
          greedyTo p n = useTo n $ if cannotMatchNull p then greedyOpt p else greedySafe p
          lazyTo p n = useTo n $ if cannotMatchNull p then lazyOpt p else lazySafe p
          possessiveTo p n = useTo n $ if cannotMatchNull p then possessiveOpt' p else possessiveSafe p
          -- Do bookeeping when advancing, check for case sensitivity option
          parseChar :: RegexParser userState Char -> RegexParser userState ()
          parseChar c = c >> incState
          acceptChar :: Char -> RegexParser userState ()
          acceptChar c =
            if sensitive
              then char c >> incState
              else foo c >> incState
          acceptString :: String -> RegexParser userState ()
          acceptString s =
            if (not sensitive) && (map toLower s /= map toUpper s)
              then sequence (map foo s) >> plusState (length s)
              else string s >> plusState (length s)
          foo :: Char -> RegexParser userState Char
          foo c
            | toLower c /= toUpper c = oneOf [toLower c, toUpper c]
            | otherwise = char c
          reOk = return []
