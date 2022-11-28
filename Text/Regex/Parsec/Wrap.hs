{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Regex.Parsec.Wrap(
  Regex(..),CompOption(..),ExecOption(..),(=~),(=~~),RegexOptionStrategy(..),
  MatchedStrings,wrapCompile,wrapMatch,wrapMatchAll,wrapCount,wrapTest) where

import Text.Regex.Base.RegexLike(RegexOptions(..),RegexMaker(..),RegexContext(..))
import Text.Regex.Parsec.Common(Regex(..),CompOption(..),RegexOptionStrategy(..),ExecOption(..),
                                StringRegex,MatchedStrings,FullState)
import Text.Regex.Parsec.ReadRegex(parseRegex)
import Text.Regex.Parsec.RegexParsecState(newState,updateUserState,getUserState,incState,lookupAccepted)
import Text.Regex.Parsec.FullParsec(patternToParsec,hasFrontCarat)
import Text.ParserCombinators.Parsec(GenParser,(<|>),option,runParser,getInput,try,anyChar,eof)

-- | This is a newtype of 'RegexOption' in "Text.Regex.Lazy.Common".

instance RegexOptions Regex CompOption ExecOption where
  blankCompOpt = CompOption {multiline = False
                            ,caseSensitive = True
                            ,captureGroups = True
                            ,strategy = Find_LongestMatch}
  defaultCompOpt = CompOption  {multiline = True
                               ,caseSensitive = True
                               ,captureGroups = True
                               ,strategy = Find_LongestMatch}
  blankExecOpt = ExecOption ()
  defaultExecOpt = ExecOption ()
  setExecOpts _ r = r
  getExecOpts _ = ExecOption ()

(=~) ::(RegexMaker Regex CompOption ExecOption source,RegexContext Regex source1 target) => source1 -> source -> target
(=~) x r = let q :: Regex
               q = makeRegex r
           in match q x

(=~~) ::(RegexMaker Regex CompOption ExecOption source,RegexContext Regex source1 target,Monad m,MonadFail m) => source1 -> source -> m target
(=~~) x r = do (q :: Regex) <- makeRegexM r
               matchM q x

wrapCompile :: CompOption
            -> ExecOption
            -> StringRegex
            -> Either String Regex
wrapCompile options _ s =
  case parseRegex s of
    Left parseError -> Left (show parseError)
    Right (pat,maxSubs) ->
      let r0  = patternToParsec (options)  pat
          r1  = patternToParsec (options {captureGroups=False}) pat
          r2  = patternToParsec (options {strategy=Find_All})   pat
          r3  = patternToParsec (options {captureGroups=False}) pat -- Int
          r = Regex {asString=s,asPattern=pat
                    ,capture=r0,capture'=r0
                    ,noCapture=r1,noCapture'=r1
                    ,allMatches=r2,userInt=r3
                    ,frontAnchor=(hasFrontCarat pat) && (not (multiline options))
                    ,groups=maxSubs}
      in Right r

-- I think the above could be built on a "matchHere with offset" thing
wrapMatch :: Int -> Regex -> [Char] -> Either String (Maybe MatchedStrings)
wrapMatch index (Regex {capture=regex,frontAnchor=anchored}) source =
  let parser = (try regex)
           <|> (anyChar >> incState >> parser)
           <|> (eof >> return [])
      once = option [] (try regex)
      result = runParser (if anchored then once else parser) (newState index ()) "wrapMatch" source
  in case result of
       Left err -> Left (show err)
       Right [] -> Right Nothing
       Right (x:_) -> Right (Just x)

wrapMatchAll :: Regex -> [Char] -> Either String [MatchedStrings]
wrapMatchAll r@(Regex {capture=regex,frontAnchor=anchored}) source =
  let parser = (try regex >>= found)
           <|> (anyChar >> incState >> parser)
           <|> (eof >> return Nothing)
      found [] = return Nothing
      found (x:_) = do pos <- lookupAccepted
                       here <- getInput
                       return (Just (x,pos,here))
      loop acc pos here =
        let result = runParser parser (newState pos ()) "wrapMatchAll" here
        in case result of
             Left err -> Left (show err)
             Right Nothing -> Right (acc [])
             Right (Just (x,pos',here')) ->
               if pos'>pos
                 then loop (acc.(x:)) pos' here'
                 else Right (acc [x])
   in if anchored -- punt to wrapMatch
        then either Left (Right . (maybe [] (:[]))) (wrapMatch 0 r source)
        else loop id 0 source

wrapTest :: Regex -> [Char] -> Either String Bool
wrapTest (Regex {noCapture=regex,frontAnchor=anchored}) source =
  let parser = (try regex >> return True)
           <|> (anyChar >> parser)
           <|> (eof >> return False)
      once = option False (try regex >> return True)
      use :: GenParser Char (FullState ()) Bool
      use = if anchored then once else parser
      result = runParser use (newState 0 ()) "wrapTest" source
  in case result of
       Left err -> Left (show err)
       Right x -> Right x

wrapCount :: Regex -> [Char] -> Either String Int
wrapCount r@(Regex {userInt=regex,frontAnchor=anchored}) source =
  let parser = (try regex >> updateUserState succ >> parser)
           <|> (anyChar >> parser)
           <|> (eof >> getUserState)
  in if anchored -- punt to wrapMatch
       then case wrapMatch 0 r source of
              Left err -> Left err
              Right Nothing -> Right 0
              Right (Just _) -> Right 1
       else case runParser parser (newState 0 0) "wrapCount" source of
              Left err -> Left (show err)
              Right n -> Right n
