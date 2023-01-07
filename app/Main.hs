{-# LANGUAGE InstanceSigs #-}

module Main (main) where

import Prelude hiding (id)
import Text.Read

--import Lib
-- Data Types
data Todo = Todo 
  { id :: Int,
    summary :: String,
    tags :: [String]
  }

instance Show Todo where 
  show  :: Todo -> String
  show t  =  "id: " ++ show (id t) ++ ", summary: `" ++ summary t ++ "`, tags: " ++ show (tags t) 

todo1 = Todo 1 "Create a Todo List using haskell" ["personal"] 
todo2 = Todo 2 "Meet M&M" ["personal"] 

showTodo :: Todo -> IO()
showTodo todo =  putStrLn (show todo) 

-- read user command from terminal 
readUserCommand :: [Todo] -> IO()
readUserCommand td = do 
  command <- getLine 
  userCommand2 command td

-- perform user commands
userCommand2 :: String -> [Todo] -> IO ()
userCommand2 command td =
    case command of
      ('e' : (' ' : todoId) ) -> do
          case readMaybe todoId :: Maybe Int of 
            Nothing -> do 
              putStrLn ("Wrong todoId: `" ++ todoId ++ "`")
              readUserCommand td
            Just v -> do 
              editTodo (read todoId :: Int) td "Test"
              readUserCommand td
      "l" -> do
          mapM_ showTodo [todo1, todo2]
          readUserCommand td
      "q" -> return ()
      _   -> do
          putStrLn ("Invalid  command: [" ++ command ++ "]")
          readUserCommand td

userCommand ::  String -> [Todo] -> IO ()
userCommand ('e' : (' ' : todoId) ) td = do
  case readMaybe todoId :: Maybe Int of 
    Nothing -> do 
      putStrLn ("Wrong todoId: `" ++ todoId ++ "`")
      readUserCommand td
    Just v -> do 
      editTodo (read todoId :: Int) td "Test"
      readUserCommand td
userCommand "l" td  = do
  mapM_ showTodo [todo1, todo2]
  readUserCommand td
userCommand "q" td = return  ()
userCommand command td = do 
  putStrLn ("Invalid  command: [" ++ command ++ "]")
  readUserCommand td

editTodo :: Int -> [Todo] -> String -> IO ()
editTodo todoId td newTodo =
  case editOne todoId td newTodo of
    Nothing -> do
      putStrLn "No ToDo found"
      readUserCommand td
    Just todo -> do 
      putStrLn ""
      print $ "New todo is " ++ newTodo
      readUserCommand td

editOne :: Int -> [a] -> String -> Maybe a
editOne n todos newTodo = 
  if  ( n < 0) || (n > length todos)
      then Nothing 
      else do
        Just (todos !! n) -- index n

------------------------------- 
main :: IO ()
main = do
  putStrLn "---  Main Menu  ---"
  putStrLn "l      : List ToDos"
  putStrLn "a      : Add a ToDo"
  putStrLn "e <id> : Edit Todo"
  putStrLn "q      : Quit"
  readUserCommand []



