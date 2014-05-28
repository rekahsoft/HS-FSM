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

-- File: Tests.hs
-- Author: Collin J. Doering <rekahsoft@gmail.com>
-- Date: Jul 29, 2013

--import Test.QuickCheck
import DFA
import NFA

data Binary = Zero | One
            deriving (Enum,Bounded,Show)

-- instance Arbitrary Binary where
--   arbitrary = elements [Zero, One]

-- Even Binary number DFA
evenDFA :: DFA Binary
evenDFA = let start = DFAStartState s0
              s0 = DFAAcceptState $ \x -> case x of
                Zero -> s0 
                One -> s1
              s1 = DFAState $ \x -> case x of
                Zero -> s0
                One -> s1
          in DFA start

oddDFA :: DFA Binary
oddDFA = let start = DFAStartState s0
             s0 = DFAState $ \x -> case x of
               Zero -> s0
               One -> s1
             s1 = DFAAcceptState $ \x -> case x of
               Zero -> s0
               One -> s1
         in DFA start
  
singleOneDFA :: DFA Binary
singleOneDFA = let start = DFAStartState s0
                   s0 = DFAState $ \x -> case x of
                     Zero -> s1
                     One -> s2
                   s1 = DFAState $ const s1
                   s2 = DFAAcceptState $ const s1
               in DFA start

singleOneOrZeroDFA :: DFA Binary
singleOneOrZeroDFA = let start = DFAStartState s0
                         s0 = DFAState $ const s1
                         s1 = DFAAcceptState $ const s2
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
main = print $ computeDFA evenDFA [One,One,Zero,One,Zero]
