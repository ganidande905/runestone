{-# LANGUAGE OverloadedStrings #-}

module Main where
import DB.Create as Create
import DB.Delete as Delete
import DB.Enter as Enter
import DB.List as List
import DB.Restore as Restore
import DB.Dump as Dump

import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["list-db"]     -> List.run
    ["create-db"]   -> Create.run
    ["delete-db"]   -> Delete.run
    ["enter-db"]    -> Enter.run
    ["restore-db"]  -> Restore.run
    ["dump-db"]     -> Dump.run
    []              -> printHelp
    _               -> putStrLn "Unknown command.\n" >> printHelp >> exitFailure

printHelp :: IO ()
printHelp = putStrLn $
  "Usage:\n" ++
  "  runestone list-db     # List all databases\n" ++
  "  runestone create-db   # Create a new database\n" ++
  "  runestone delete-db   # Delete a database\n" ++
  "  runestone enter-db    # Connect to a database using psql\n" ++
  "  runestone restore-db  # Restore a database from a .sql file\n" ++
  "  runestone dump-db     # Dump all databases to backups/\n"