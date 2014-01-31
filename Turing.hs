{-# LANGUAGE LambdaCase, TupleSections, BangPatterns #-}

-- TODO: Multiple heads in a single map
-- Make code more abstract to support 1D
-- Have 1d no-overwrite mode
-- Have different colors

import Data.IORef

import Data.Monoid
import Control.Arrow
import System.Random
import Control.Monad
import Control.Applicative
import Control.Monad.State
import qualified Data.Map as M
import Data.Map (Map)

import Data.Array
import Data.Maybe

import System.Environment (getArgs)

import Graphics.Gloss.Interface.IO.Animate

import Types

move :: Integral a => Move -> (a, a) -> (a, a)
move U = second (+ 1)
move D = second (subtract 1)
move L = first (subtract 1)
move R = first (+ 1)

randomRS :: Random a => (a, a) -> State StdGen a
randomRS bs = do
  g <- get
  let (x, g') = randomR bs g
  put g'
  return x

randomS :: Random a => State StdGen a
randomS = do
  g <- get
  let (x, g') = random g
  put g'
  return x

randomKeep :: a -> State StdGen (Maybe a)
randomKeep x = (\b -> if b then Just x else Nothing) <$> randomS

randomMove :: Integer -> State StdGen (Move, Integer, Alphabet)
randomMove n = (,,) <$> randomRS (U, R) <*> randomRS (1, n) <*> randomS


-- randTM :: Integer -> ()
randTM :: Integer -> State StdGen TM
randTM nq = array bds <$> mapM pairWithMove ((,) <$> [1..nq] <*> [(O)..A4])
  where bds = ((1, O), (nq, A4))
        pairWithMove i = (i,) <$> randomMove nq
{-
randTM nq = M.fromList <$> mapM pairWithMove ((,) <$> [1..nq] <*> [O, X, A1, A2, A3, A4])
  where pairWithMove i = (i,) <$> randomMove nq
-}
type Board = Map (Int, Int) Alphabet
type Tape  = Map Int Alphabet

step2D :: TM -> Board -> (Int, Int) -> Integer -> (Board, (Int, Int), Integer)
step2D m b pos q = (M.insert pos a' b, move dir pos, q')
  where (dir, q', a') = m ! (q, a)
        a = M.findWithDefault O pos b

step1D :: TM -> Tape -> Int -> Integer -> (Tape, Int, Integer)
step1D m t pos q = (M.insert pos a' t, pos', q')
  where (dir, q', a') = m ! (q, M.findWithDefault O pos t)
        pos'          = pos + case dir of { L -> -1; R -> 1; _ -> 0 }

runTM' :: (tm -> Map p a -> p -> Integer -> (Map p a, p, Integer)) -> p -> tm -> [(Map p a, p)]
runTM' step initPos m = go M.empty initPos 1
  where
    go b pos q =
      (b, pos) : case step m b pos q of { 
        (b', p', q') -> go b' p' q'
      }

runTM2D :: TM -> [(Map (Int, Int) Alphabet, (Int, Int))]
runTM2D = runTM' step2D (0, 0)

runTM1D :: TM -> [(Tape, Int)]
runTM1D = runTM' step1D 0

runTM1DStack :: TM -> [Picture]
runTM1DStack = map (pictures . fst) . scanl draw ([], 0) . runTM1D
  where draw (pics, !n) (tape, hPos) =
          ((map (\(x, a) -> drawCell (x, n) a) $ M.toList tape) ++ pics, n + 1)

drawCell :: Enum a => (Int, Int) -> a -> Picture
drawCell pos a = color (toColor a) (square pos)

square :: (Int, Int) -> Picture
square (x, y) = polygon [(x', y'), (x' + len, y'), (x' + len, y' + len), (x', y' + len)]
  where len      = 5
        (x', y') = (len * fromIntegral x, len * fromIntegral y)

drawBoard :: Board -> (Int, Int) -> Picture
drawBoard board headPos =
  pictures . (color red (square headPos) :) . map (uncurry drawCell) . M.toList $ board

drawTape :: Tape -> Int -> Picture
drawTape tape headPos =
  pictures
  . (color red (square (headPos, 0)) :)
  . map (\(i, a) -> drawCell (i, 0) a) . M.toList $ tape

stepFrame' :: (s -> Picture) -> IORef [s] -> Float -> Float -> IO Picture
stepFrame' draw stateRef frameRate t = do
  (s : ss) <- readIORef stateRef
  when (not (null ss)) $
    writeIORef stateRef ss
  pure (draw s)

stepFrame2D :: IORef [(Map (Int, Int) Alphabet, (Int, Int))] -> Float -> Float -> IO Picture
stepFrame2D = stepFrame' (uncurry drawBoard)

stepFrame1D :: IORef [(Tape, Int)] -> Float -> Float -> IO Picture
stepFrame1D = stepFrame' (uncurry drawTape)

stepFrame1DStack :: IORef [Picture] -> Float -> Float -> IO Picture
stepFrame1DStack = stepFrame' id

mkMain :: Num n => (tm -> s) -> (IORef s -> n -> b) -> tm -> IO b
mkMain run step = \tm -> do
  let states = run tm
  statesRef <- newIORef states
  return (step statesRef 0)

tm1DStackMain = mkMain runTM1DStack stepFrame1DStack
tm2DMain = mkMain runTM2D stepFrame2D

tm1DMain = mkMain runTM1D stepFrame1D


main :: IO ()
main = do
  (mode:x:_) <- getArgs
  tm <- evalState (randTM (read x)) <$> newStdGen
  writeFile "latest" (show tm)
  step <- case read mode :: Int of
    0 -> tm1DStackMain tm
    1 -> tm1DMain tm
    _ -> tm2DMain tm

  animateIO
    (InWindow "turing" (1,1) (200,200))
    white
    step

  {-
  let boards = runTM2D tm
  boardRef <- newIORef boards
  animateIO
    (InWindow "turing" (1, 1) (200,200))
    white
    (stepFrame boardRef 0)
  -}

-- mvs n = [(d, q', a') | q <- [1..n], a <- [O, X], q' <- [1..n], a' <- [O, X], d <- [U, D, L, R]]

{-
step2D :: TM -> Board -> (Int, Int) -> Integer -> Maybe (Board, (Int, Int), Integer)
step2D m b pos q = do
  (dir, q', a') <- M.lookup (q, a) m
  return (M.insert pos a' b, move dir pos, q')
  where a = M.findWithDefault O pos b

step1D :: TM -> Tape -> Int -> Integer -> Maybe (Tape, Int, Integer)
step1D m t pos q = do
  (dir, q', a') <- M.lookup (q, a) m
  let pos' = pos + case dir of { L -> -1; R -> 1; _ -> 0 }
  pure (M.insert pos a' t, pos', q')
  where a = M.findWithDefault O pos t

-}
{-
  playIO
    (InWindow "turing" (1, 1) (200,200))
    white
    100
    boards
    (return . drawBoard . head)
    (\_ b -> return b)
    (\_ -> return .tail)
  -}


-- Q,A,(Q,A,D)

{-
decodeTM = do
  nq <- unpairS
  n <- get
  if n == 0
    then return []
    else (:) <$> decodeRule nq <*> decodeTM
-}
--  takeWhile (/= ((0,X),(L,0,X))) <$> replicateM (fromIntegral k) decodeRule

