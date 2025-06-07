{-# LANGUAGE OverloadedStrings #-}
module DB.Dump (run )where


import System.Process (callProcess)
import System.IO (hFlush, stdout)
import DB.Utils (getEnvOrFail, loadEnv)
import Data.Time (getCurrentTime, utctDay)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

run :: IO ()
run = do
  _ <- loadEnv


  putStr "Enter database name to dump: "
  hFlush stdout
  dbName <- getLine

  currentTime <- getCurrentTime
  let dateStr = show (utctDay currentTime)  


  let backupDir = "/Users/gani/FOSS/runestone/backups"
  let outFile = backupDir </> (dbName ++ "_" ++ dateStr ++ ".sql")


  createDirectoryIfMissing True backupDir


  user <- getEnvOrFail "POSTGRES_USER"
  host <- getEnvOrFail "PGHOST"
  let portStr = show (5433 :: Int)


  putStrLn $ "Dumping database '" ++ dbName ++ "' to '" ++ outFile ++ "'..."
  callProcess "pg_dump" ["-U", user, "-h", host, "-p", portStr, "-d", dbName, "-f", outFile]
  putStrLn "Dump completed successfully."