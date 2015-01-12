{-# LANGUAGE NoMonomorphismRestriction, DeriveFunctor, LambdaCase #-}

module Game where

import qualified Data.Map as M
import Data.Map (Map)
import Control.Arrow
import Control.Monad
import Control.Monad.Free

data Player = Red | Blue

data GameF a
  = Put         Int Int a
  | LastMove (Maybe (Int, Int) -> a)
  | Inspect Player Int ([Int] -> a)
  deriving (Functor)

type Game = Free GameF

put :: Int -> Int -> Game ()
put n m = liftF $ Put n m ()

inspect :: Player -> Int -> Game [Int]
inspect p n = liftF $ Inspect p n id

lastMove :: Game (Maybe (Int, Int))
lastMove = liftF $ LastMove id

halting = forever $
  lastMove >>= maybe (return ()) (\(n, m) -> if n == m then put n 0 else return ())

type State = (Map Int [Int], Map Int [Int])

{-
move :: Player -> Strategy -> State -> (State, Strategy)
move player (Put k n m) state = (focus insert state, m)
  where insert = M.alter (Just . maybe [] (k:)) n
        focus = case player of { Red -> first; Blue -> second }

move _ (InspectRed  n cont) (r, b) = ((r, b), cont (M.findWithDefault [] n r))
move _ (InspectBlue n cont) (r, b) = ((r, b), cont (M.findWithDefault [] n b))

-- interp Blue (Put k n m) (r, b) = ((M.alter (maybe [] (Just . (k:))) n r, b), m)

runGame' :: State -> Strategy -> Strategy -> [State]
runGame' s mr mb =
  let (s' , mr') = move Red  mr s
      (s'', mb') = move Blue mb s'
  in s : runGame' s'' mr' mb'

{-
haltingProblem = go 0 where
  go :: Int -> Strategy
  go n = insert
-}
runGame :: Strategy -> Strategy -> [State]
runGame = runGame' (M.empty, M.empty)
-}
