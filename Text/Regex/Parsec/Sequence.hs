{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- This offers type class instances for 'RegexMaker' and
-- 'RegexLike'. This is usually used via import "Text.Regex.Full".
--
-- It is worth noting that 'RegexMaker' and 'RegexLike' methods for
-- 'ByteString' are built on those for 'String' except the default
-- 'matchOnceText' and 'matchAllText' are used since they are already
-- efficient for use on 'ByteString' (and they use 'matchOnce' and
-- 'matchAll').
--
-- This exports instances of the high level API and the medium level
-- API of 'compile','execute', and 'regexec'.
module Text.Regex.Parsec.Sequence
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
-- build on these instances for RegexMaker and RegexLike

import Data.Sequence (Seq, ViewL (..))
import qualified Data.Sequence as S
import Text.Regex.Base.Impl (polymatch, polymatchM)
import Text.Regex.Base.RegexLike (MatchArray, MatchLength, MatchOffset, MatchText, RegexContext (..), RegexLike (..), RegexMaker (..))
import Text.Regex.Parsec.Common (MatchedStrings)
import Text.Regex.Parsec.String ()
import Text.Regex.Parsec.Wrap (CompOption (..), ExecOption (..), Regex (..), wrapCompile, wrapMatch)

instance RegexContext Regex (Seq Char) (Seq Char) where
  match = polymatch
  matchM = polymatchM

instance RegexMaker Regex CompOption ExecOption (Seq Char) where
  makeRegexOpts opts e source = makeRegexOpts opts e (unpack source)
  makeRegexOptsM opts e source = makeRegexOptsM opts e (unpack source)

instance RegexLike Regex (Seq Char) where
  matchOnce r bs = matchOnce r (unpack bs)
  matchAll r bs = matchAll r (unpack bs)
  matchTest r bs = matchTest r (unpack bs)
  matchCount r bs = matchCount r (unpack bs)

-- Use default matchOnceText and matchAllText

{-# INLINE unpack #-}
unpack :: (Seq Char) -> String
unpack s = case S.viewl s of
  EmptyL -> []
  (h :< t) -> h : unpack t

compile ::
  -- | Flags (summed together)
  CompOption ->
  -- | Flags (summed together)
  ExecOption ->
  -- | The regular expression to compile
  (Seq Char) ->
  -- | Returns: the compiled regular expression
  Either String Regex
compile c e bs = wrapCompile c e (unpack bs)

execute ::
  -- | Compiled regular expression
  Regex ->
  -- | (Seq Char) to match against
  (Seq Char) ->
  Either String (Maybe (Array Int (MatchOffset, MatchLength)))
execute r@(Regex {groups = g}) bs = either Left (Right . (fmap (toArr g))) (wrapMatch 0 r (unpack bs))

regexec ::
  -- | Compiled regular expression
  Regex ->
  -- | (Seq Char) to match against
  (Seq Char) ->
  Either String (Maybe ((Seq Char), (Seq Char), (Seq Char), [(Seq Char)]))
regexec r@(Regex {groups = g}) bs =
  case wrapMatch 0 r (unpack bs) of
    Left err -> Left err
    Right Nothing -> Right Nothing
    Right (Just ms) ->
      Right . Just $
        let (_, (o, l)) = ms I.! 0
         in ( S.take o bs,
              S.take l (S.drop o bs),
              S.drop (o + l) bs,
              map
                ( \(_, (o', l')) ->
                    if (-1) == o'
                      then S.empty
                      else S.take l' (S.drop o' bs)
                )
                (tail (elems (toMT g ms)))
            )

toMT :: Int -> MatchedStrings -> MatchText String
toMT maxSubs ms = accumArray (\_ new -> new) ("", (-1, 0)) (0, maxSubs) (I.toList ms)

toArr :: Int -> MatchedStrings -> MatchArray
toArr maxSubs ms = accumArray (\_ (_, ol) -> ol) (-1, 0) (0, maxSubs) (I.toList ms)
