{-# LANGUAGE OverloadedStrings #-}

module DB.Utils
  ( loadEnv
  , getEnvOrFail
  , connectToDefaultDB
  ) where

import qualified Database.PostgreSQL.Simple as PGS
import System.Environment (lookupEnv)
import Configuration.Dotenv (loadFile, defaultConfig, configPath)

loadEnv :: IO ()
loadEnv = do
  let envPath = "/Users/gani/FOSS/runestone/simple/.env"
  _ <- loadFile defaultConfig { configPath = [envPath] }
  return ()

getEnvOrFail :: String -> IO String
getEnvOrFail key = do
  val <- lookupEnv key
  case val of
    Just v  -> return v
    Nothing -> error $ "Missing env var: " ++ key


connectToDefaultDB :: IO PGS.Connection
connectToDefaultDB = do
  loadEnv
  host <- getEnvOrFail "PGHOST"
  user <- getEnvOrFail "PGUSER"
  pass <- getEnvOrFail "PGPASSWORD"
  let port = 5433
  let db   = "postgres"

  PGS.connect PGS.defaultConnectInfo
    { PGS.connectHost     = host
    , PGS.connectPort     = port
    , PGS.connectUser     = user
    , PGS.connectPassword = pass
    , PGS.connectDatabase = db
    }