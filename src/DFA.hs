{-# LANGUAGE GADTs, BangPatterns #-}

-- (C) Copyright Collin Doering 2013
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

-- File: FSM.hs
-- Author: Collin J. Doering <rekahsoft@gmail.com>
-- Date: Jul 20, 2013

-- | TODO: Comment
module DFA
       ( DFA (DFA)
       , DFAState (DFAState,DFAAcceptState)
       , DFAStartState (DFAStartState)
       , nextDFAState
       , computeDFA
       , minimizeDFA
       , deadDFAState
       , deadDFAAcceptState
       , constToDFA
       , kleeneStar
       ) where

-- | TODO: Comment
newtype DFA a = DFA { runDFA :: DFAStartState a }
                    -- , dfaStates :: [DFAState a]
                    -- , dfaAcceptStates :: [DFAState a] }

-- | TODO: Comment
newtype DFAStartState a = DFAStartState { runDFAStartState :: DFAState a }

-- | TODO: Comment
-- data DFAState a = DFAState { runDFAState :: a -> DFAState a }
--                 | DFAAcceptState { runDFAState :: a -> DFAState a }

-- | TODO: Comment
data DFAState a where
  DFAState           :: (Enum a, Bounded a) => { runDFAState :: a -> DFAState a } -> DFAState a
  DFAAcceptState     :: (Enum a, Bounded a) => { runDFAState :: a -> DFAState a } -> DFAState a
  DFADeadState       :: (Enum a, Bounded a) => { runDFAState :: a -> DFAState a } -> DFAState a
  DFAAcceptDeadState :: (Enum a, Bounded a) => { runDFAState :: a -> DFAState a } -> DFAState a
    
-- | TODO: Comment
nextDFAState :: a -> DFAState a -> DFAState a
nextDFAState x s = runDFAState s x

-- | TODO: Comment
computeDFA :: DFA a -> [a] -> Bool
computeDFA y ys = computeDFA' ys $ runDFAStartState . runDFA $ y
  where computeDFA' [] (DFAState _) = False
        computeDFA' [] (DFAAcceptState _) = True
        computeDFA' _  (DFADeadState _) = False
        computeDFA' _  (DFAAcceptDeadState _) = True
        computeDFA' (x:xs) !s = computeDFA' xs $ nextDFAState x s

-- | TODO: Comment
minimizeDFA :: DFA a -> DFA a
minimizeDFA = undefined

-- | TODO: Comment
deadDFAState :: (Enum a, Bounded a) => DFAState a
deadDFAState = DFADeadState $ const deadDFAState

-- | TODO: Comment
deadDFAAcceptState :: (Enum a, Bounded a) => DFAState a
deadDFAAcceptState = DFAAcceptDeadState $ const deadDFAAcceptState

-- | TODO: Comment
constToDFA :: (Eq a, Enum a, Bounded a) => [a] -> DFA a
constToDFA [] = DFA $ DFAStartState $ DFAAcceptState $ const deadDFAState
constToDFA ys = DFA $ DFAStartState $ constToDFA' ys
  where constToDFA' [] = DFAAcceptState $ const deadDFAState
        constToDFA' (x:xs) = DFAState $ \y -> if x == y then constToDFA' xs else deadDFAState

-- | TODO: Comment
kleeneStar :: (Eq a, Enum a, Bounded a) => a -> DFA a
kleeneStar x = let s = DFAAcceptState $ \y -> if x == y then s else deadDFAState
               in DFA (DFAStartState s)
