module InteractiveProover
    (
      startInteractiveProover
    ) where

import Formula
import FormulaParser
import Command
import Input
import Tactic
import InputParser
import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.List
import System.IO
import System.Directory
import System.Exit

type Context = M.Map String Formula
type Goal = [Formula]
type Trace = [(Tactic, [(Context, Goal)])]
type SingleGoal = Formula

receiveProblem :: IO (Context, Goal)
receiveProblem = do
  putStrLn "Input the problem"
  problemE <- parseProblem <$> getLine
  return $ case problemE of
    Right (ctx, g) -> (ctx, g)
    _ -> error "Wrong Problem"

runTactic :: Tactic -> Context -> SingleGoal -> Either String (Tactic, Context, Goal) -- Tacticはログをとるために返す
runTactic tac@(Intro (Just hypoName)) ctx goal = case goal of
  (Arrow left right) -> if M.notMember hypoName ctx
    then Right (tac, M.insert hypoName left ctx, [right])
    else Left $ "The name " ++ hypoName ++ " has already been used."
  _ -> Left "Cannot run the intro command."

runTactic tac@(Intro Nothing) ctx goal = case goal of
  (Arrow left right) ->
    let freshName = getFreshHypoName ctx 0
    in Right (tac, M.insert freshName left ctx, [right])
    where
      getFreshHypoName :: M.Map String Formula -> Int -> String
      getFreshHypoName ctx i = if M.member ("H" ++ show i) ctx
        then getFreshHypoName ctx $ i + 1
        else "H" ++ show i
  _ -> Left "Cannot run the intro command."

runTactic Intros ctx goal = case goal of
  (Arrow left right) ->
    let freshName = getFreshHypoName ctx 0
    in runTactic Intros (M.insert freshName left ctx) right
    where
      getFreshHypoName :: M.Map String Formula -> Int -> String
      getFreshHypoName ctx i = if M.member ("H" ++ show i) ctx
        then getFreshHypoName ctx $ i + 1
        else "H" ++ show i
  (SimpleProp p) -> Right (Intros, ctx, [goal])

runTactic tac@(Apply hypoName) ctx goal =
  if M.notMember hypoName ctx
    then Left $ "The hypothesis named " ++ hypoName ++ " does not exist."
    else
      let hypo = ctx M.! hypoName
      in case checkTail goal hypo [] of
        [] -> Left $ "Cannot run the apply command with the hypothesis named " ++ hypoName ++ "."
        goals -> Right (tac, ctx, goals)
  where
    checkTail goal prop@(SimpleProp p) newGoal =
      if goal == prop then newGoal else []
    checkTail goal arr@(Arrow left right) newGoal =
      if goal == right
        then left : newGoal
        else checkTail goal right $ left : newGoal

runTactic Assumption ctx goal =
  if goal `elem` M.elems ctx
    then Right (Assumption, ctx, [])
    else Left "Cannot run the assumption command."

runTactic tac@(Exact hypoName) ctx goal =
  if M.member hypoName ctx
    then if ctx M.! hypoName == goal
      then Right (tac, ctx, [])
      else Left $ "Cannot run the exact command with the hypothesis" ++ hypoName ++ "."
    else Left $ "The hypothesis named " ++ hypoName ++ " does not exist."

runCommand :: Command -> Trace -> IO Trace
runCommand (Log fileName) trc = do
  createDirectoryIfMissing False ".log"
  let filePath = ".log\\" ++ fileName ++ ".log"
  withFile filePath WriteMode $ \h ->
    hPutStrLn h . unlines . map show $ reverse trc
  return trc

runCommand Undo trc = case trc of
  [first] -> do -- This is the first log representing the problem, so cannot undo anymore.
    putStrLn "Cannot run the undo command."
    return trc
  (t : ts) -> return ts

runCommand Quit trc = putStrLn "Quit the proof." >> exitSuccess

interactiveProover :: Trace -> [(Context, Goal)] -> IO ()
interactiveProover trc [] = do
  putStrLn "==============================="
  putStrLn ""
  putStrLn "There is no Goal."
  putStrLn ""
  putStrLn "QED."
  putStrLn ""
  putStrLn "--------------------------------"
  putStrLn ""
  inputE <- parseInput <$> getLine
  case inputE of
    Right (CommandI cmd) -> do
      trcs' <- runCommand cmd trc
      let trc' = head trcs'
      interactiveProover trcs' $ snd trc'
    Right _ -> do
      putStrLn "There is no probelm."
      putStrLn ""
      interactiveProover trc []
    Left parseErr -> print parseErr >> interactiveProover trc []
  interactiveProover trc []
interactiveProover trc ((ctx, []) : rest) = interactiveProover trc rest
interactiveProover trc env@((ctx, goal@(g : gs)) : rest) = do
  putStrLn "================================"
  putStrLn ""
  printHypothesis ctx
  putStrLn ""
  printGoal g
  putStrLn $ case gs of
    [] -> ""
    _ -> "\n" ++ "    " ++ show (length gs) ++ " more subgoals remain."
  putStrLn "--------------------------------"
  putStrLn ""
  inputE <- parseInput <$> getLine
  putStrLn ""
  case inputE of
    Right (CommandI cmd) -> do
      trcs' <- runCommand cmd trc
      let trc' = head trcs'
      interactiveProover trcs' $ snd trc'
    Right (TacticI tac) ->
      case runTactic tac ctx g of
        Right (tac, ctx', g') -> case g' of
          [] -> interactiveProover ((tac, [(ctx', g')]) : trc) rest
          _ -> interactiveProover ((tac, [(ctx', g')]) : trc) ([(ctx', g')] ++ [(ctx, gs)] ++ rest)
        Left errMsg -> putStrLn errMsg >> interactiveProover trc env
    Left parseErr -> print parseErr >> interactiveProover trc env


startInteractiveProover :: IO ()
startInteractiveProover = do
  (ctx, gs) <- receiveProblem
  let trc = [(ID, [(ctx, gs)])]
  putStrLn ""
  interactiveProover trc [(ctx, gs)]

printHypothesis :: Context -> IO ()
printHypothesis ctx  = do
  putStrLn "Hypothesis"
  putStrLn ""
  putStrLn . unlines . map (\(h, f) -> h ++ ": " ++ f) . M.toAscList $ M.map show ctx

printGoal :: SingleGoal -> IO ()
printGoal goal = do
  putStrLn "Goal"
  putStrLn ""
  print goal


tupleToTriple :: (b, c) -> a -> (a, b, c)
tupleToTriple (b, c) a = (a, b, c)

fst' :: (a, b, c) -> a
fst' (x, _, _) = x

snd' :: (a, b, c) -> b
snd' (_, y, _) = y

third' :: (a, b, c) -> c
third' (_, _, z) = z
