
{- | This module is used to represent finite automata. -}

{-# LANGUAGE ScopedTypeVariables #-}
module FiniteAutomata where

import Data.Set as Set
import Data.Map as Map
import Data.Bits ((.&.))

import Codec.Picture ( PixelRGB8(PixelRGB8) )

-- | White pixel RGB representation.
whitePx :: PixelRGB8
whitePx  = PixelRGB8 255 255 255

-- | Black pixel RGB representation.
blackPx :: PixelRGB8
blackPx  = PixelRGB8 0 0 0

-- | Alphabet used by the DFAs.
imageAlphabet :: Set Int
imageAlphabet  = Set.fromList [0, 1, 2, 3]

type ImagePixels = [[PixelRGB8]]

{- |
Represents the transition function (delta function) of a
'FiniteAutomata'. The outer map stores the transitions from an origin
state, its value (the inner map) stores the symbols that.
-}
type TransitionFunction state symbol = Map state (Map symbol (Set state))

{- |
Represents a finite automata. The type can represents DFA and NFA,
with states of type @state@ and input symbols of type @symbol@.
-}
data FiniteAutomata state symbol = FA
  { -- | The set of states.
    states       :: Set state
  -- | The set of input symbols.
  , alphabet     :: Set symbol
  -- | The transition function.
  , delta        :: TransitionFunction state symbol
  -- | The initial state.
  , initialState :: state
  -- | The set of final or accepting states.
  , acceptState  :: Set state
  }

{- |
Adds a transition to the given 'TransitionFunction'. The tuple
@(state, symbol, state)@ is a tuple whose elements represent the
origin state, the symbol that executes the transition and the end
state, respectively.
-}
addTransition :: forall state symbol. (Ord state, Ord symbol) =>
  (state, symbol, state) ->
    TransitionFunction state symbol -> TransitionFunction state symbol
addTransition (q, s, q') =
  Map.insertWith (Map.unionWith Set.union) q (Map.singleton s q'')
  where q'' :: Set state
        q''  = Set.singleton q'

-- | Formats how to display instances of 'FiniteAutomata'.
instance (Show state, Show symbol) => Show (FiniteAutomata state symbol) where
  show (FA st sy tf is ac) =
    "States:         "   ++ show st ++
    "\nAlphabet:       " ++ show sy ++
    "\nDelta:          " ++ show tf ++
    "\nInitial States: " ++ show is ++
    "\nAccept States:  " ++ show ac

-- ** Example Images **

-- | 2^3 x 2^3 chess board.
chessBoard :: ImagePixels
chessBoard  =
  [
   [ if mod i 2 == 0
     then if mod j 2 == 0 then blackPx else whitePx
     else if mod j 2 /= 0 then blackPx else whitePx | j <- idxs
   ] | i <- idxs
  ]
  where idxs :: [Int]
        idxs  = [0..7]

-- | Sierpiński triangle.
sierpinskiTriangle :: ImagePixels
sierpinskiTriangle =
  [
   [ if (i .&. j) /= 0 then whitePx else blackPx | j <- idxs ]
   | i <- idxs
  ]
  where idxs :: [Int]
        idxs  = [0..127]
