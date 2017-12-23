import qualified Data.Map as Map
import Data.Maybe
import Tablet
       (Instruction(RCV, SND, value), State(..), execute, parse, step)

--
-- This is a truly atrocious implementation.
-- More wanted to see whether it'd be possible to do it this way
-- (i.e. building on top of the single-threaded engine instead of
-- extending it.)  Yes, it is possible.  It must also be possible
-- to do it nicer, but I cannot find that today...
--
step1a :: [Instruction] -> State -> State
step1a program = step program sndHandler rcvHandler
  where
    sndHandler state n =
      State (Map.insert "send" n (registers state)) (1 + pc state) False
    rcvHandler state r =
      case Map.lookup "waiting" (registers state) of
        Nothing ->
          State (Map.insert "blocked" 1 (registers state)) (pc state) False
        Just value ->
          State
            (Map.delete "blocked" $
             Map.delete "waiting" $ Map.insert r value (registers state))
            (1 + pc state)
            False

step1 :: [Instruction] -> (State, [Int], [Int]) -> (State, [Int], [Int])
step1 program (state, queueIn, queueOut) = (newState, newQueueIn, newQueueOut)
  where
    readyToSend = not (null queueIn || Map.member "waiting" (registers state))
    preState
      | readyToSend =
        State
          (Map.insert "waiting" (head queueIn) (registers state))
          (pc state)
          (halt state)
      | otherwise = state
    postState = step1a program preState
    newState =
      State
        (Map.delete "send" $ registers postState)
        (pc postState)
        (halt postState)
    newQueueOut =
      case Map.lookup "send" $ registers postState of
        Nothing -> queueOut
        Just value -> queueOut ++ [value]
    newQueueIn
      | readyToSend = tail queueIn
      | otherwise = queueIn

step2 ::
     [Instruction]
  -> (State, State, [Int], [Int])
  -> (State, State, [Int], [Int])
step2 program (state0, state1, queue0, queue1) =
  (outState0, outState1, newQueue0, newQueue1)
  where
    (newState0, intQueue0, intQueue1) = step1 program (state0, queue0, queue1)
    (newState1, newQueue1, newQueue0) =
      step1 program (state1, intQueue1, intQueue0)
    deadlocked =
      Map.member "blocked" (registers newState0) &&
      Map.member "blocked" (registers newState1)
    outState0
      | deadlocked = State (registers newState0) (pc newState0) True
      | otherwise = newState0
    outState1
      | deadlocked = State (registers newState1) (pc newState1) True
      | otherwise = newState1

partB listing =
  length $
  filter snd1 $
  takeWhile allRunning $
  iterate (step2 program) (initialState0, initialState1, [], [])
  where
    program = parse listing
    initialState0 = State Map.empty 0 False
    initialState1 = State (Map.fromList [("p", 1)]) 0 False
    allRunning (s0, s1, _, _) = not (halt s0) && not (halt s1)
    snd1 (_, state, _, _) = isSND (program !! pc state)
    isSND SND {} = True
    isSND _ = False

--
-- Part A
--
partA listing =
  fromJust $
  Map.lookup "sound" $
  registers $
  head $
  dropWhile (not . halt) $ execute program sndHandler rcvHandler initialState
  where
    program = parse listing
    initialState = State Map.empty 0 False
    sndHandler state n =
      State (Map.insert "sound" n (registers state)) (1 + pc state) False
    rcvHandler state r
      | registers state Map.! r == 0 =
        State (registers state) (1 + pc state) False
      | otherwise = State (registers state) (pc state) True

run listing = (partA listing, partB listing)

main = interact $ show . run
