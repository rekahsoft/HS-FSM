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

newtype DFA a = DFA { runDFA :: DFAStartState a }

newtype DFAStartState a = DFAStartState { runDFAStartState :: DFAState a }

data DFAState a = DFAState { runDFAState :: a -> DFAState a }
                | DFAEndState { runDFAState :: a -> DFAState a }

newtype NFA a = NFA { runNFA :: NFAStartState a }

newtype NFAStartState a = NFAStartState { runNFAStartState :: NFAState a }

data NFAState a = NFAState { runNFAState :: a -> [NFAState a] }
                | NFAEndState { runNFAState :: a -> [NFAState a] }

-- data NFAEState a = NFAEState 

nextDFAState :: a -> DFAState a -> DFAState a
nextDFAState x s = runDFAState s x

computeDFA :: [a] -> DFA a -> Bool
computeDFA ys y = computeDFA' ys $ runDFAStartState . runDFA $ y
  where computeDFA' [] (DFAState _) = False
        computeDFA' [] (DFAEndState _) = True
        computeDFA' (x:xs) s = computeDFA' xs $ nextDFAState x s

convertNFAToDFA :: (Enum a, Bounded a) => NFA a -> DFA a
convertNFAToDFA n = convertNFAToDFA' $ runNFAStartState . runNFA $ n
  where convertNFAToDFA' = undefined

minimizeDFA :: DFA a -> DFA a
minimizeDFA = undefined

nextNFAStates :: a -> [NFAState a] -> [NFAState a]
nextNFAStates x = concatMap (($ x) . runNFAState)

computeNFA :: [a] -> NFA a -> Bool
computeNFA ys y = computeNFA' ys [runNFAStartState . runNFA $ y]
  where computeNFA' [] cs = flip any cs $ \s -> case s of
          (NFAState _) -> False
          (NFAEndState _) -> True
        computeNFA' (x:xs) cs = computeNFA' xs $ nextNFAStates x cs

data Binary = Zero | One
            deriving (Enum,Bounded,Show)

-- Even Binary number DFA
dfa :: DFA Binary
dfa = let start = DFAStartState s0
          s0 = DFAEndState $ \x -> case x of
            Zero -> s0 
            One -> s1
          s1 = DFAState $ \x -> case x of
            Zero -> s0
            One -> s1
      in DFA start

singleOneDFA :: DFA Binary
singleOneDFA = let start = DFAStartState s0
                   s0 = DFAState $ \x -> case x of
                     Zero -> s1
                     One -> s2
                   s1 = DFAState $ const s1
                   s2 = DFAEndState $ const s1
               in DFA start

singleOneOrZeroDFA :: DFA Binary
singleOneOrZeroDFA = let start = DFAStartState s0
                         s0 = DFAState $ const s1
                         s1 = DFAEndState $ const s2
                         s2 = DFAState $ const s2
                     in DFA start

-- Odd Binary number NFA
nfa :: NFA Binary
nfa = let start = NFAStartState s0
          s0 = NFAState $ \x -> case x of
            Zero -> [s0]
            One -> [s0,s1]
          s1 = NFAEndState $ const []
      in NFA start

main :: IO ()
main = print $ computeDFA [One,One,Zero,One,Zero] dfa
