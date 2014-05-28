{-# LANGUAGE GADTs, BangPatterns #-}

-- (C) Copyright Collin J. Doering 2014
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- File: NFA.hs
-- Author: Collin J. Doering <collin.doering@rekahsoft.ca>
-- Date: May 26, 2014

-- | TODO: Comment
module NFA
       ( NFA (NFA)
       , NFAState (NFAState,NFAEndState)
       , NFAStartState (NFAStartState)
       , nextNFAStates
       , computeNFA
       , convertNFAToDFA
       ) where

import DFA

-- | TODO: Comment
newtype NFA a = NFA { runNFA :: NFAStartState a }

-- | TODO: Comment
newtype NFAStartState a = NFAStartState { runNFAStartState :: NFAState a }

-- | TODO: Comment
-- data NFAState a = NFAState { runNFAState :: a -> [NFAState a] }
--                 | NFAEndState { runNFAState :: a -> [NFAState a] }

-- | TODO: Comment
data NFAState a where
  NFAState :: (Enum a, Bounded a) => { runNFAState :: a -> [NFAState a] } -> NFAState a
  NFAEndState :: (Enum a, Bounded a) => { runNFAState :: a -> [NFAState a] } -> NFAState a

-- data NFAEState a = NFAEState 

-- | TODO: Comment
nextNFAStates :: a -> [NFAState a] -> [NFAState a]
nextNFAStates x = concatMap (($ x) . runNFAState)

-- | TODO: Comment
computeNFA :: [a] -> NFA a -> Bool
computeNFA ys y = computeNFA' ys [runNFAStartState . runNFA $ y]
  where computeNFA' [] cs = flip any cs $ \s -> case s of
          (NFAState _) -> False
          (NFAEndState _) -> True
        computeNFA' (x:xs) !cs = computeNFA' xs $ nextNFAStates x cs

-- | TODO: Comment
convertNFAToDFA :: (Enum a, Bounded a) => NFA a -> DFA a
convertNFAToDFA n = convertNFAToDFA' $ runNFAStartState . runNFA $ n
  where convertNFAToDFA' = undefined
