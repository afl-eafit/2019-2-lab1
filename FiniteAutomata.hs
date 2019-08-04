{- | This module is used to represent Finite Automata. -}
{-# LANGUAGE ScopedTypeVariables #-}
module FiniteAutomata where

import Data.Set as Set
import Data.Map as Map
import Data.Bits ((.&.))

import Codec.Picture

whitePx :: PixelRGB8
whitePx  = PixelRGB8 255 255 255

blackPx :: PixelRGB8
blackPx  = PixelRGB8 0 0 0

imageAlphabet :: Set Int
imageAlphabet  = Set.fromList [0, 1, 2, 3]

type ImagePixels = [[PixelRGB8]]

{- | Represents the Transition Function (delta function) of a FiniteAutomata.
     The outer map stores the transitions from a orgin state,
     its value (the inner map) stores the symbols that -}
type TransitionFunction state symbol = Map state (Map symbol (Set state))

{- | Represents a Finite Automata.
     The type can represent DFA and NFA, with
     states of type @state@ and input symbols
     of type @symbol@. -}
data FiniteAutomata state symbol = FA
  {
      -- | The set of states.
      states       :: Set state
      -- | The set of input symbols.
    , alphabet     :: Set symbol
      -- | The Transition Function.
    , delta        :: TransitionFunction state symbol
      -- | The initial state.
    , initialState :: state
      -- | The set of final or accepting states.
    , acceptState  :: Set state
  }

{- | Adds a transition to the given TransitionFunction.
     @(state, symbol, state)@ is a tuple whose elements
     represent the origin state, the symbol that executes
     the transition and the end state, respectively.-}
addTransition :: forall state symbol. (Ord state, Ord symbol) =>
  (state, symbol, state) ->
    TransitionFunction state symbol -> TransitionFunction state symbol
addTransition (q, s, q') =
  Map.insertWith (Map.unionWith Set.union) q (Map.singleton s q'')
  where q'' :: Set state
        q''  = Set.singleton q'

-- | Formats how to display instances of FiniteAutomata.
instance (Show state, Show symbol) => Show (FiniteAutomata state symbol) where
  show (FA st sy tf is ac) =
    "States:         "   ++ show st ++
    "\nAlphabet:       " ++ show sy ++
    "\nDelta:          " ++ show tf ++
    "\nInitial States: " ++ show is ++
    "\nAccept States:  " ++ show ac

-- ** Example Images **

-- | 2^3 x 2^3 Chess Board.
chessBoard :: ImagePixels
chessBoard  =
  [
   [if mod i 2 == 0
    then if mod j 2 == 0 then  blackPx else whitePx
    else if mod j 2 /= 0 then blackPx else whitePx | j <- js
   ] | i <- is
  ]
  where is :: [Int]
        is  = [0..7]
        js :: [Int]
        js  = [0..7]

-- | Sierpiński triangle.
sierpińskiTriangle :: ImagePixels
sierpińskiTriangle =
  [
   [if (i .&. j) /= 0 then whitePx else blackPx | j <- js]
   | i <- is
  ]
  where is :: [Int]
        is  = [0..127]
        js :: [Int]
        js  = [0..127]
