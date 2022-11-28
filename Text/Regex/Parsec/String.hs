{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- This offers type class instances for 'RegexMaker' and
-- 'RegexLike'. This is usually used via import "Text.Regex.Full".
--
-- It is worth noting all methods in 'RegexLike' are re-implemented for
-- efficiency instead of taking the default definitions.
--
-- This exports instances of the high level API and the medium level
-- API of 'compile','execute', and 'regexec'.
module Text.Regex.Parsec.String
  ( -- ** Types
    Regex,
    MatchOffset,
    MatchLength,
    CompOption (..),
    ExecOption (..),

    -- ** Medium level API functions
    compile,
    execute,
    regexec,
  )
where

import Data.Array (Array, accumArray, elems)
import qualified Data.IntMap as I (toList, (!))
import Text.Regex.Base.Impl (polymatch, polymatchM)
import Text.Regex.Base.RegexLike (MatchArray, MatchLength, MatchOffset, MatchText, RegexContext (..), RegexLike (..), RegexMaker (..))
import Text.Regex.Parsec.Wrap
  ( CompOption (..),
    ExecOption (..),
    MatchedStrings,
    Regex (Regex, groups),
    wrapCompile,
    wrapCount,
    wrapMatch,
    wrapMatchAll,
    wrapTest,
  )

instance RegexContext Regex String String where
  match = polymatch
  matchM = polymatchM

instance RegexMaker Regex CompOption ExecOption String where
  makeRegexOpts opts e source =
    unwrap $
      wrapCompile opts e source
  makeRegexOptsM opts e source =
    either fail return $
      wrapCompile opts e source

instance RegexLike Regex String where
  matchTest r s = unwrap $ wrapTest r s
  matchCount r s = unwrap $ wrapCount r s
  matchOnce r@(Regex {groups = g}) s =
    fmap (toArr g) (unwrap (wrapMatch 0 r s))
  matchOnceText r@(Regex {groups = g}) s =
    fmap
      ( \mt ->
          let (_, (o, l)) = mt I.! 0
           in (take o s, toMT g mt, drop (o + l) s)
      )
      (unwrap (wrapMatch 0 r s))
  matchAll r@(Regex {groups = g}) s = map (toArr g) (unwrap $ wrapMatchAll r s)
  matchAllText r@(Regex {groups = g}) s = map (toMT g) (unwrap $ wrapMatchAll r s)

unwrap :: Either String v -> v
unwrap x = case x of
  Left err -> error ("Text.Regex.Parsec.String died: " ++ err)
  Right v -> v

toArr :: Int -> MatchedStrings -> MatchArray
toArr maxSubs ms = accumArray (\_ (_, ol) -> ol) (-1, 0) (0, maxSubs) (I.toList ms)

toMT :: Int -> MatchedStrings -> MatchText String
toMT maxSubs ms = accumArray (\_ new -> new) ("", (-1, 0)) (0, maxSubs) (I.toList ms)

compile ::
  -- | Flags (summed together)
  CompOption ->
  -- | Flags (summed together)
  ExecOption ->
  -- | The regular expression to compile (ASCII only, no null bytes)
  String ->
  -- | Returns: the compiled regular expression
  Either String Regex
compile = wrapCompile

execute ::
  -- | Compiled regular expression
  Regex ->
  -- | String to match against
  String ->
  Either String (Maybe (Array Int (MatchOffset, MatchLength)))
execute r@(Regex {groups = g}) s = either Left (Right . (fmap (toArr g))) (wrapMatch 0 r s)

regexec ::
  -- | Compiled regular expression
  Regex ->
  -- | String to match against
  String ->
  Either String (Maybe (String, String, String, [String]))
regexec r@(Regex {groups = g}) s =
  case wrapMatch 0 r s of
    Left err -> Left err
    Right Nothing -> Right Nothing
    Right (Just ms) ->
      Right . Just $
        let (main, (o, l)) = ms I.! 0
         in ( take o s,
              main,
              drop (o + l) s,
              map fst (tail (elems (toMT g ms)))
            )
