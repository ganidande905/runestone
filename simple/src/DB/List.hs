{-# LANGUAGE OverloadedStrings #-}
module DB.List(run) where
import qualified Database.PostgreSQL.Simple as PGS
import DB.Utils (connectToDefaultDB)
import Control.Monad (forM_)
run :: IO ()
run = do
    conn <- connectToDefaultDB
    dbs <- PGS.query_ conn "SELECT datname FROM pg_database WHERE datistemplate = false;" :: IO [PGS.Only String]
    forM_ dbs $ \(PGS.Only name) -> putStrLn ("- " ++ name)