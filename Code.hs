{-# LANGUAGE LambdaCase #-}
import Control.Arrow
import System.Random
import Control.Monad
import Control.Applicative
import Control.Monad.State
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Types


pair :: (Integral a, Num a) => (a, a) -> a
pair (x, y) = (x^2 + 2 * x * y + y^2 + 3 * x + y) `div` 2

sqrt' :: (Floating a, Integral b) => b -> a
sqrt' x = sqrt (fromIntegral x)

unpair :: Integral a => a -> (a, a)
unpair n = (0 + d, k - d)
  where tri m    = m * (m + 1) `div` 2
        invTri m = floor (-0.5 + (sqrt (1 + 8 * fromIntegral m) / 2))
        k        = invTri n
        d        = n - tri k

unpairS :: (Integral a) => State a a
unpairS = do
  n <- get
  let (k, n') = unpair n
  put n'
  return k

decodeMove :: Integral a => State a Move
decodeMove = do
  n <- get
  let (n', p) = divMod n 2
  put n'
  return (if p == 0 then L else R)

decodeAlpha :: Integral a => State a Alphabet
decodeAlpha = (\case { L -> X; R -> O}) <$> decodeMove

-- decodeRule :: Integer -> (Rule, Integer)

swap (x, y) = (y, x)

decodeState :: Integer -> State Integer Integer
decodeState nq = state (swap . (`divMod` nq))

decodeRule :: Integer -> State Integer Rule
decodeRule nq = do
  q  <- decodeState nq
  a  <- decodeAlpha
  m  <- decodeMove
  q' <- decodeState nq
  a' <- decodeAlpha
  return ((q, a), (m, q', a'))

decodeTM = do
  nq <- (+ 1) <$> unpairS
  loop (nq + 2)
  where
    loop nq = do
      n <- get
      if n == 0 then pure [] else (:) <$> decodeRule nq <*> loop nq


