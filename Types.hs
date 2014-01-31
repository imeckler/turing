{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module Types where

import System.Random
import Control.Arrow
import Data.Map (Map)
import Graphics.Gloss.Data.Color

import Data.Ix
import Data.Array

data Alphabet = O | X | A1 | A2 | A3 | A4
  deriving (Show, Eq, Ord, Bounded, Enum)

{-
data Color = Red | Green | Blue | Yellow | Purple | Black
  deriving (Show, Eq, Ord, Bounded, Enum)
  -}

toColor :: Enum a => a -> Color
toColor x = [white, red, green, blue, yellow, magenta, black] !! fromEnum x

data Move = U | D | L | R
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Ix Alphabet where
  range (x, y) = [x..y]

  index (x, _) a = fromEnum a - fromEnum x
  
  inRange (x, y) a = x <= a && a <= y

type Rule = ((Integer, Alphabet), (Move, Integer, Alphabet))

-- type TM = Map (Integer, Alphabet) (Move, Integer, Alphabet)
type TM = Array (Integer, Alphabet) (Move, Integer, Alphabet)

alphaToBool = \case { O -> False; X -> True }
boolToAlpha = \case { False -> O; True -> X }

-- intToMove :: Int -> Move
intToBoundedEnum :: forall a. (Bounded a, Enum a) => Int -> a
intToBoundedEnum = toEnum . (`mod` size)
  where size = (fromEnum (maxBound :: a)) + 1

instance Random Alphabet where
  random = first intToBoundedEnum . random
  randomR (x, y) = first intToBoundedEnum . randomR (fromEnum x, fromEnum y)

instance Random Move where
  random = first intToBoundedEnum . random
  randomR (x, y) = first intToBoundedEnum . randomR (fromEnum x, fromEnum y)

