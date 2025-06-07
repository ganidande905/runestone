{-# LANGUAGE OverloadedStrings #-}

module DB.Enter(run) where

import System.Process (callProcess)
import System.IO (hFlush, stdout)
import DB.Utils (getEnvOrFail, loadEnv, connectToDefaultDB)
import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.FromRow as PGS
import Control.Monad (forM_, when)

run :: IO ()
run = do
  _ <- loadEnv


  container <- getEnvOrFail "CONTAINER_NAME"
  user      <- getEnvOrFail "POSTGRES_USER"

  conn <- connectToDefaultDB
  dbs  <- PGS.query_ conn "SELECT datname FROM pg_database WHERE datistemplate = false;" :: IO [PGS.Only String]
  PGS.close conn

  putStrLn "Available Databases:"
  forM_ (zip [1..] dbs) $ \(i, PGS.Only name) ->
    putStrLn $ show i ++ ". " ++ name

  putStr "Enter the number of the DB to enter: "
  hFlush stdout
  choice <- getLine
  let index = read choice :: Int

  when (index < 1 || index > length dbs) $ error "❌ Invalid choice."

  let PGS.Only selectedDb = dbs !! (index - 1)

  putStrLn $ "Entering database '" ++ selectedDb ++ "' inside container '" ++ container ++ "'..."
  callProcess "docker" ["exec", "-it", container, "psql", "-U", user, "-d", selectedDb]