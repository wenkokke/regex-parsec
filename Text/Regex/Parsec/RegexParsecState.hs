{-# OPTIONS -funbox-strict-fields #-}
-- | The Parsec parser needs to keep track of various state
-- information while matching a regular expression.  This includes the
-- number of accepted characters and the in-progross and completed
-- substring matches.  This module defines that state and some
-- convenience functions.
--
-- A user defined state can be maintained via newState, getUserState,
-- setUserState, and updateUserState.
--
-- The 'FullState' type is opaque, allowing for better abstraction.
-- Note that calling stopSub when startSub has not been called will
-- trigger an 'error', and that initState/finalState call startSub
-- 0/stopSub 0 respectively.
module Text.Regex.Parsec.RegexParsecState 
    (
 -- ** Create the full state of the parsec parser
     initState,finalState,newState,initStateP,finalStateP
 -- ** Manipulate the user state of the parser
    ,getUserState,setUserState,updateUserState
 -- ** Manupulate the accepted character counter
    ,incState,plusState,lookupAccepted
-- ** Compare Matched Strings
    ,eqSubs
 -- ** Manipulate the captured substrings, PCRE style
    ,startSub,stopSub,lookupSub,lookupSubs
 -- ** Manipulate the captured substrings, Posix style
    ,startSubP,stopSubP,lookupSubP,lookupSubsP
    ) where

{- By Chris Kuklewicz, 2006. BSD License, see the LICENSE file. -}

import Text.Regex.Parsec.Common(RegexParser, MatchedStrings, FullState(..), Opened(..), Closed(..))
import Text.ParserCombinators.Parsec(updateState, setState, getState, getInput)
import Control.Monad(liftM)
import qualified Data.IntMap as I(fromAscList,toAscList, updateLookupWithKey,insert, empty, lookup)

trace :: a -> b -> b
trace _ b = b

-- | 'initState' forgets all substring matching (in-progress or
-- complete) and starts sub 0 (the whole match).  This operation
-- preserves the user state and accepted character count
initState :: RegexParser userState ()
initState = do -- keep only userState and accepted, reset capture
  updateState $ \state -> state {openSub = I.empty, closedSub = I.empty,posixSub = EndOpened []}
  startSub 0

initStateP :: RegexParser userState ()
initStateP = do -- keep only userState and accepted, reset capture
  updateState $ \state -> state {openSub = I.empty, closedSub = I.empty,posixSub = EndOpened []}
  startSubP 0

-- | 'finalState' stops sub 0 (the whole match) and returns an IntMap
-- of all the completed substring matches.  Any in-progress matches
-- are ignored.
finalState :: RegexParser userState MatchedStrings
finalState = do stopSub 0
                lookupSubs

-- | 'finalState' stops sub 0 (the whole match) and returns an IntMap
-- of all the completed substring matches.  Any in-progress matches
-- are ignored.
finalStateP :: RegexParser userState MatchedStrings
finalStateP = do stopSubP 0
                 lookupSubsP

-- | This takes a value of the user state and returns the full parsec
-- state with zero accepted characters to n and no in-progress or completed
-- substring matches.
newState :: Int -> userState -> FullState userState
newState n user = FullState {userState = user
                            ,accepted = n
                            ,posixSub = EndOpened []
                            ,openSub = I.empty
                            ,closedSub = I.empty}

-- | This returns the value of the user state
getUserState :: RegexParser userState userState
getUserState = liftM userState getState

-- | The replaces the user state with the provided value
setUserState :: userState -> RegexParser userState ()
setUserState user = do state <- getState
                       setState $ state {userState = user}

-- | This applies the given function to the current value of the user state
updateUserState :: (userState -> userState) -> RegexParser userState ()
updateUserState f = updateState (\state@FullState{userState=user}->state{userState=f user})

-- | This adds one to the number of accepted characters
incState :: RegexParser userState ()
incState = updateState (\state@(FullState {accepted=pos})->state {accepted=succ pos})

-- | This adds the provided number to the count of accepted characters
plusState :: Int -> RegexParser userState ()
plusState n = updateState (\state@(FullState {accepted=pos})->state {accepted=n + pos})


-- | 'lookupAccepted' return the number of accepted input characters.
-- This is typicaly counted from the call to initState.
lookupAccepted :: RegexParser userState Int
lookupAccepted = liftM accepted getState

-- | Called with PatternIndex i, this makes the i'th substring match
-- inprogress and marks the start of the i'th substring match at the
-- current position.  This does affect any value for the completed
-- i'th substring match.
startSub :: Int -> RegexParser userState ()
startSub i = do 
  state@(FullState {accepted=n,openSub=open}) <- getState
  here <- getInput
  let open' = I.insert i (here,n) open
      state' = state {openSub = open'}
  setState state'

-- | Called with PatternIndex i, this captures the input from point
-- where startSub i was called until the current location.  It removes
-- the in-progress i'th substring and sets the i'th completed
-- substring to the new value, overwriting any previous value.
--
-- The completed value is available from lookupSub i or in the map
-- provided by lookupSubs.
--
-- If the i'th substring is not open when this is called then stopSub
-- will call 'error'.
stopSub :: Int -> RegexParser userState ()
stopSub i = do
  state@(FullState {accepted=n,openSub=open,closedSub=closed}) <- getState
  let (mEntry,open') = I.updateLookupWithKey del i open
      del _ _ = Nothing
  case mEntry of
    Nothing -> error ("RegexParsecState: Could not closeSub "++show i)
    Just (here,pos) -> let len = n-pos
                           sub = take len here
                           closed' = I.insert i (sub,(pos,len)) closed
                           state' = state {openSub=open',closedSub=closed'}
                       in setState state'

-- | 'lookupSub' i returns Just the completed captured between 'startSub'
-- i and 'stopSub' i or Nothing if there has been no capture.
lookupSub :: Int -> RegexParser userState (Maybe String)
lookupSub i = do msol <- liftM ((I.lookup i).closedSub) getState
                 case msol of
                   Nothing -> return Nothing
                   Just (sub,_) -> return (Just sub)

-- | 'lookupSubs' returns an IntMap of all the completed substring captures.
lookupSubs :: RegexParser userState MatchedStrings
lookupSubs = liftM closedSub getState

eqSubs :: MatchedStrings -> MatchedStrings -> Bool
eqSubs a b = let a' = map (\ (k,(_,ol)) -> (k,ol)) . I.toAscList $ a
                 b' = map (\ (k,(_,ol)) -> (k,ol)) . I.toAscList $ b
             in a' == b'

startSubP :: Int -> RegexParser userState ()
startSubP i = trace ("startSub: "++show i) $ do
  state@(FullState {accepted=pos,posixSub=sub}) <- getState
  trace (">startSubP "++show i++" : "++show sub) $ return ()  
  here <- getInput
  let sub' = 
        case sub of
          Opened closed j dat next ->
            case break (\(Closed _ k _)->i==k) closed of
              (_,[]) -> Opened [] i (here,pos) sub
              (_,_:closed') -> Opened [] i (here,pos) (Opened closed' j dat next)
          EndOpened closed ->
            case break (\(Closed _ k _)->i==k) closed of
              (_,[]) -> Opened [] i (here,pos) sub
              (_,_:closed') -> Opened [] i (here,pos) (EndOpened closed')
  setState (state {posixSub=sub'})
  trace ("<startSubP "++show i++" : "++show sub') $ return ()  

stopSubP :: Int -> RegexParser userstate ()
stopSubP i = trace ("stopSub: "++show i) $ do
  state@(FullState {accepted=pos',posixSub=sub}) <- getState
  trace (">stopSubP "++show i++" : "++show sub) $ return ()  
  let sub' =
        case sub of
          Opened closed j (here,pos) next | j==i ->
            let len = pos'-pos
                capture = Closed closed i (take len here,(pos,len))
            in case next of
                 Opened closed' j' dat' next' ->
                   Opened (capture:closed') j' dat' next'
                 EndOpened closed' ->
                   EndOpened (capture:closed')
                                          | otherwise ->
            error ("Malformed nesting when stopSubP group #"++show i++" when Opened group was #"++show j)
          EndOpened _ -> error ("Malformed nesting when stopSubP group #"++show i++" when Opened group was EndOpened")
  setState (state {posixSub=sub'})
  trace ("<stopSubP "++show i++" : "++show sub') $ return ()  

lookupSubP :: Int -> RegexParser userstate (Maybe String)
lookupSubP i =
  let fetchOpen (EndOpened closed) = fetchClosed closed
      fetchOpen (Opened greater j (string,_) lesser) =
        case compare i j of
          GT -> fetchClosed greater
          EQ -> Just string
          LT -> fetchOpen lesser
      fetchClosed [] = Nothing
      fetchClosed (Closed greater j (string,_) : lesser) =
        case compare i j of
          GT -> fetchClosed greater
          EQ -> Just string
          LT -> fetchClosed lesser
  in trace ("lookupSub: "++show i) $ liftM (fetchOpen . posixSub) getState

lookupSubsP :: RegexParser userstate MatchedStrings
lookupSubsP = 
  let traverseOpen acc (EndOpened closed) = traverseClosed acc closed
      traverseOpen acc (Opened greater _ _ lesser) =
        let acc' = traverseClosed acc greater
        in traverseOpen acc' lesser
      traverseClosed acc [] = acc
      traverseClosed acc (Closed greater j dat : lesser) =
        let acc' = (j,dat) : traverseClosed acc greater
        in traverseClosed acc' lesser
  in do (FullState {posixSub=sub}) <- getState
        let value = I.fromAscList . (traverseOpen []) $ sub 
        trace ("lookupSubs " ++ show sub ++ "\n" ++ show value) $ return value

