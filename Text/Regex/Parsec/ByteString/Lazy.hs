{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
This offers type class instances for 'RegexMaker' and
'RegexLike'. This is usually used via import "Text.Regex.Full".

It is worth noting that 'RegexMaker' and 'RegexLike' methods for
'ByteString' are built on those for 'String' except the default
'matchOnceText' and 'matchAllText' are used since they are already
efficient for use on 'ByteString' (and they use 'matchOnce' and
'matchAll').

This exports instances of the high level API and the medium level
API of 'compile','execute', and 'regexec'.
-}
module Text.Regex.Parsec.ByteString.Lazy(
  -- ** Types
  Regex
 ,MatchOffset
 ,MatchLength
 ,CompOption(..)
 ,ExecOption(..)
  -- ** Medium level API functions
 ,compile
 ,execute
 ,regexec
 ) where

import Data.Array(Array,elems,accumArray)
import qualified Data.IntMap as I (toList,(!))
import Text.Regex.Parsec.Common(MatchedStrings)
import Text.Regex.Base.RegexLike(RegexMaker(..),RegexLike(..),RegexContext(..),MatchText,MatchArray,MatchOffset,MatchLength)
import Text.Regex.Parsec.Wrap(Regex(..),CompOption(..),ExecOption(..),wrapCompile,wrapMatch)
import Text.Regex.Parsec.String() -- build on these instances for RegexMaker and RegexLike
import Data.ByteString.Lazy.Char8(ByteString)
import qualified Data.ByteString.Lazy.Char8 as B(empty,unpack,take,drop)
import Text.Regex.Base.Impl(polymatch,polymatchM)

instance RegexContext Regex ByteString ByteString where
  match = polymatch
  matchM = polymatchM

instance RegexMaker Regex CompOption ExecOption ByteString where
  makeRegexOpts opts e source = makeRegexOpts opts e (B.unpack source)
  makeRegexOptsM opts e source = makeRegexOptsM opts e (B.unpack source)

instance RegexLike Regex ByteString where
  matchOnce r bs = matchOnce r (B.unpack bs)
  matchAll r bs = matchAll r (B.unpack bs)
  matchTest r bs = matchTest r (B.unpack bs)
  matchCount r bs = matchCount r (B.unpack bs)
-- Use default matchOnceText and matchAllText

compile  :: CompOption -- ^ Flags (summed together)
         -> ExecOption -- ^ Flags (summed together)
         -> ByteString -- ^ The regular expression to compile
         -> Either String Regex -- ^ Returns: the compiled regular expression
compile c e bs = wrapCompile c e (B.unpack bs)

execute :: Regex      -- ^ Compiled regular expression
        -> ByteString -- ^ ByteString to match against
        -> Either String (Maybe (Array Int (MatchOffset,MatchLength)))
execute r@(Regex {groups=g}) bs = either Left (Right . (fmap (toArr g))) (wrapMatch 0 r (B.unpack bs))

regexec :: Regex      -- ^ Compiled regular expression
        -> ByteString -- ^ ByteString to match against
        -> Either String (Maybe (ByteString, ByteString, ByteString, [ByteString]))
regexec r@(Regex {groups=g}) bs =
  case wrapMatch 0 r (B.unpack bs) of
    Left err -> Left err
    Right Nothing -> Right Nothing
    Right (Just ms) -> Right . Just $
      let (_,(o,l)) = ms I.! 0
      in (B.take (toEnum o) bs
         ,B.take (toEnum l) (B.drop (toEnum o) bs)
         ,B.drop (toEnum (o+l)) bs
         ,map (\(_,(o',l')) -> if (-1)==o'
                                 then B.empty
                                 else B.take (toEnum l') (B.drop (toEnum o') bs))
              (tail (elems (toMT g ms))))

toMT :: Int -> MatchedStrings -> MatchText String
toMT maxSubs ms =  accumArray (\_ new->new) ("",(-1,0)) (0,maxSubs) (I.toList ms)

toArr :: Int -> MatchedStrings -> MatchArray
toArr maxSubs ms = accumArray (\_ (_,ol)->ol) (-1,0) (0,maxSubs) (I.toList ms)
