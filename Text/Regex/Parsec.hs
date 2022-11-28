{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- The "Text.Regex.Parsec" module provides a backend for regular
-- expressions. To use it should be imported along with
-- "Text.Regex.Base".  If you import this along with other backends, then
-- you should do so with qualified imports, perhaps renamed for
-- convenience.
--
-- The main data type exported is 'Regex'. The 'CompOption' provides some
-- different choices when compiling the regular expression. The
-- 'ExecOption' type is simply newtype'd () at this time.  The main
-- functions are '=~' and '=~~' which use the DFA 'Regex' type.  These
-- are all exported by "Text.Regex.Lib.WrapLazy".
--
-- This backend uses "Text.ParserCombinators.Parsec" to create parsers to
-- perform the regular expression matching.  It allows for the syntax of
-- the original "Text.Regex" plus enhancements:
--
-- 1. Substring capture with back-references in the regular expression
--
-- 2. Controlled matching repetitions with {n} {n,} {,m} {n,m} forms
--
-- 3. Lazy matching with ?? +? *? {n,m}? forms
--
-- 4. Possessive matching with ?+ ++ *+ {n,m}+ forms
--
-- 5. Longest match, leftmost match, and rightmost matching strategies (see 'CompOption')
--
-- 6. Can handle NUL characters in both regular expression and search string
--
-- 7. This backend tries not to break when doing multiple matches with a regular expression that can match 0 characters
--
-- The parsec parser operators on ['Char'], so 'ByteString' input is
-- converted to such a list during processing.
--
-- There are two main modes for the CompOption strategy:
--  Find_LongestMatch is the default, similary to Posix and the DFA
--  Find_FirstLeft is like PCRE
--
-- Find_LongestMatch requires that nested subexpressions always capture
-- nested segments of the source sting.  Find_FirstLeft requires that
-- subexpressions capture the last successful use of that subexpression,
-- and so nested subexpression may or may not captured nested segments of
-- the source string.  Quickcheck and I cannot find any differences
-- between Find_FirstLeft and the regex-pcre backend.
--
-- Find_LongestMatch runs into the problem that while the
-- leftmost-longest whole match is well defined, the choice for what the
-- subpatterns and subexpressions match has not been well defined.  The
-- actual choice made is currently in flux as I develop it further, but
-- the 0.75 version now maximizes (as leftmost-longest) each captured
-- subexpression in order.  This can produce results that differ from
-- regex-posix and regex-tre.  Opinions on the desired behavior are
-- welcome.
module Text.Regex.Parsec (module Export) where

import Data.Version (Version (..))
import qualified Text.Regex.Base as Export
import qualified Text.Regex.Parsec.ByteString as Export
import qualified Text.Regex.Parsec.ByteString.Lazy as Export
import qualified Text.Regex.Parsec.Sequence as Export
import qualified Text.Regex.Parsec.String as Export
import Text.Regex.Parsec.Wrap
  ( CompOption (..),
    ExecOption (..),
    Regex,
    RegexOptionStrategy (..),
    (=~),
    (=~~),
  )
