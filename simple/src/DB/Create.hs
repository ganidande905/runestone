{-# LANGUAGE OverloadedStrings #-}
module DB.Create (run) where
import qualified Database.PostgreSQL.Simple as PGS
import DB.Utils
import System.IO (hFlush, stdout)
import Data.String (fromString)

run :: IO ()
run = do
  putStr "Enter database name to create: "
  hFlush stdout
  dbName <- getLine
  conn <- connectToDefaultDB
  let sql = fromString ("CREATE DATABASE \"" ++ dbName ++ "\";")
  _ <- PGS.execute_ conn sql
  putStrLn $ "Database '" ++ dbName ++ "' created successfully."
  PGS.close conn