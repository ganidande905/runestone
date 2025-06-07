{-# LANGUAGE OverloadedStrings #-}

module DB.Restore(run) where

import System.Process (callProcess)
import System.IO (hFlush, stdout)
import DB.Utils (getEnvOrFail, loadEnv)

run :: IO ()
run = do
  _ <- loadEnv

  putStr "Enter target database name to restore into: "
  hFlush stdout
  dbName <- getLine

  putStr "Enter path to SQL dump file: "
  hFlush stdout
  dumpPath <- getLine

  user <- getEnvOrFail "POSTGRES_USER"
  host <- getEnvOrFail "PGHOST"
  let portStr = show (5433 :: Int)

  putStrLn $ "Restoring dump from '" ++ dumpPath ++ "' into database '" ++ dbName ++ "'..."
  callProcess "psql" ["-U", user, "-h", host, "-p", portStr, "-d", dbName, "-f", dumpPath]
  putStrLn "Restore completed successfully."