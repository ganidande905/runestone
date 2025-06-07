{-# LANGUAGE OverloadedStrings #-}
module DB.Delete(run) where
import qualified Database.PostgreSQL.Simple as PGS
import DB.Utils
import System.IO (hFlush, stdout)
import Data.String (fromString)
import Control.Monad (forM_)

run :: IO ()
run = do
  conn <- connectToDefaultDB

  dbs  <- PGS.query_ conn "SELECT datname FROM pg_database WHERE datistemplate = false;" :: IO [PGS.Only String]
  putStrLn "Available Databases:"
  forM_ (zip [1..] dbs) $ \(i, PGS.Only name) ->
    putStrLn $ show i ++ ". " ++ name

  putStr "Enter database name to drop: "
  hFlush stdout
  dbName <- getLine

  putStr $ "Are you sure you want to drop the database '" ++ dbName ++ "'? (y/n): "
  hFlush stdout
  confirm <- getLine

  if confirm /= "y" && confirm /= "Y"
    then do
      putStrLn "Operation cancelled."
      PGS.close conn
    else do
      let sql = fromString ("DROP DATABASE IF EXISTS \"" ++ dbName ++ "\";")
      _ <- PGS.execute_ conn sql
      putStrLn $ "Database '" ++ dbName ++ "' dropped successfully."
      PGS.close conn