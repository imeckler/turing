{-# LANGUAGE NoMonomorphismRestriction #-}

module Game where

import qualified Data.Map as M
import Data.Map (Map)
import Control.Arrow

data Strategy
  = Put         Int Int Strategy
  | InspectRed  Int ([Int] -> Strategy)
  | InspectBlue Int ([Int] -> Strategy)

type State = (Map Int [Int], Map Int [Int])

data Player = Red | Blue

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

runGame :: Strategy -> Strategy -> [State]
runGame = runGame' (M.empty, M.empty)
