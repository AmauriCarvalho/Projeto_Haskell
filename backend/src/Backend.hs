{-# LANGUAGE LambdaCase, GADTs, OverloadedStrings, ScopedTypeVariables #-}
module Backend where

import Common.Route
import Obelisk.Backend

import Database.PostgreSQL.Simple
import Data.Text
import Obelisk.Route
import Snap.Core
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Data.Aeson.Text

import Common.Api

import Database.PostgreSQL.Simple

migration :: Query
migration = "CREATE TABLE IF NOT EXISTS BRCar\
            \ (id SERIAL PRIMARY KEY, nome TEXT NOT NULL)"

migrationBr :: Query
migrationBr = "CREATE TABLE IF NOT EXISTS brasilcar\
              \ (id SERIAL PRIMARY KEY, marca TEXT NOT NULL, modelo TEXT NOT NULL,  ano INTEGER NOT NULL)"

migrationJp :: Query
migrationJp = "CREATE TABLE IF NOT EXISTS japancar\
              \ (id SERIAL PRIMARY KEY, marca TEXT NOT NULL, modelo TEXT NOT NULL,  ano INTEGER NOT NULL)"

migrationEu :: Query
migrationEu = "CREATE TABLE IF NOT EXISTS europecar\
              \ (id SERIAL PRIMARY KEY, marca TEXT NOT NULL, modelo TEXT NOT NULL,  ano INTEGER NOT NULL)"

getConn :: ConnectInfo
getConn = ConnectInfo "ec2-54-204-56-171.compute-1.amazonaws.com"
                       5432 -- porta
                       "gpicigaryyqcik"
                       "5eba18a21f3af4c23d4a8456db5397504888e948e3fab5e3b54efe02a1db1f97"
                       "d36b40961c4dn8"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      dbcon <- connect getConn
      serve $ do
        \case
         BackendRoute_ListarBr :/ () -> method GET $ do
            res :: [BrasilCar] <- liftIO $ do
                execute_ dbcon migrationBr
                query_ dbcon "SELECT * from brasilcar"
            modifyResponse $ setResponseStatus 200 "OK"
            writeLazyText (encodeToLazyText res)
         BackendRoute_BuscarBr :/ pid -> method GET $ do
            res :: [BrasilCar] <- liftIO $ do
              execute_ dbcon migrationBr
              query dbcon "SELECT * from brasilcar where id=?" (Only (pid :: Int))
            if res /= [] then do
              modifyResponse $ setResponseStatus 200 "OK"
              writeLazyText (encodeToLazyText (Prelude.head res))
            else
              modifyResponse $ setResponseStatus 404 "NOT FOUND"
         BackendRoute_BrasilCar :/ () -> method POST $ do
            brasilcar <- A.decode <$> readRequestBody 2000
            case brasilcar of
              Just br -> do
                  liftIO $ do
                    execute_ dbcon migrationBr
                    execute dbcon "INSERT INTO brasilcar(marca, modelo, ano) VALUES (?,?,?)" (brasilcarMarca br, brasilcarModelo br, brasilcarAno br)
                  modifyResponse $ setResponseStatus 200 "OK"
              _ -> modifyResponse $ setResponseStatus 500 "Erro"
         BackendRoute_EditarBr :/ pid -> method POST $ do
           bra <- A.decode <$> readRequestBody 2000
           case bra of
              Just brasilcar -> do
                  liftIO $ do
                    execute_ dbcon migrationBr
                    execute dbcon "UPDATE brasilcar SET marca = ?, \
                                \ modelo = ?, ano = ? WHERE id = ?"
                           (brasilcarMarca brasilcar,
                           brasilcarModelo brasilcar,
                           brasilcarAno brasilcar,
                           pid)
                  modifyResponse $ setResponseStatus 200 "OK"
              Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
         BackendRoute_ListarJp :/ () -> method GET $ do
             res :: [JapanCar] <- liftIO $ do
                 execute_ dbcon migrationJp
                 query_ dbcon "SELECT * from japancar"
             modifyResponse $ setResponseStatus 200 "OK"
             writeLazyText (encodeToLazyText res)
         BackendRoute_BuscarJp :/ pid -> method GET $ do
            res :: [JapanCar] <- liftIO $ do
              execute_ dbcon migrationJp
              query dbcon "SELECT * from japancar where id=?" (Only (pid :: Int))
            if res /= [] then do
              modifyResponse $ setResponseStatus 200 "OK"
              writeLazyText (encodeToLazyText (Prelude.head res))
            else
              modifyResponse $ setResponseStatus 404 "NOT FOUND"
         BackendRoute_JapanCar :/ () -> method POST $ do
            japancar <- A.decode <$> readRequestBody 2000
            case japancar of
              Just jp -> do
                  liftIO $ do
                    execute_ dbcon migrationJp
                    execute dbcon "INSERT INTO japancar(marca, modelo, ano) VALUES (?,?,?)" (japancarMarca jp, japancarModelo jp, japancarAno jp)
                  modifyResponse $ setResponseStatus 200 "OK"
              _ -> modifyResponse $ setResponseStatus 500 "Erro"
         BackendRoute_EditarJp :/ pid -> method POST $ do
           jap <- A.decode <$> readRequestBody 2000
           case jap of
              Just japancar -> do
                  liftIO $ do
                    execute_ dbcon migrationJp
                    execute dbcon "UPDATE japancar SET marca = ?, \
                                \ modelo = ?, ano = ? WHERE id = ?"
                           (japancarMarca japancar,
                           japancarModelo japancar,
                           japancarAno japancar,
                           pid)
                  modifyResponse $ setResponseStatus 200 "OK"
              Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
         BackendRoute_ListarEu :/ () -> method GET $ do
             res :: [EuropeCar] <- liftIO $ do
                 execute_ dbcon migrationEu
                 query_ dbcon "SELECT * from europecar"
             modifyResponse $ setResponseStatus 200 "OK"
             writeLazyText (encodeToLazyText res)
         BackendRoute_BuscarEu :/ pid -> method GET $ do
            res :: [EuropeCar] <- liftIO $ do
              execute_ dbcon migrationEu
              query dbcon "SELECT * from europecar where id=?" (Only (pid :: Int))
            if res /= [] then do
              modifyResponse $ setResponseStatus 200 "OK"
              writeLazyText (encodeToLazyText (Prelude.head res))
            else
              modifyResponse $ setResponseStatus 404 "NOT FOUND"
         BackendRoute_EuropeCar :/ () -> method POST $ do
            europecar <- A.decode <$> readRequestBody 2000
            case europecar of
              Just eu -> do
                  liftIO $ do
                    execute_ dbcon migrationEu
                    execute dbcon "INSERT INTO europecar(marca, modelo, ano) VALUES (?,?,?)" (europecarMarca eu, europecarModelo eu, europecarAno eu)
                  modifyResponse $ setResponseStatus 200 "OK"
              _ -> modifyResponse $ setResponseStatus 500 "Erro"
         BackendRoute_EditarEu :/ pid -> method POST $ do
           eur <- A.decode <$> readRequestBody 2000
           case eur of
              Just europecar -> do
                  liftIO $ do
                    execute_ dbcon migrationEu
                    execute dbcon "UPDATE europecar SET marca = ?, \
                                \ modelo = ?, ano = ? WHERE id = ?"
                           (europecarMarca europecar,
                           europecarModelo europecar,
                           europecarAno europecar,
                           pid)
                  modifyResponse $ setResponseStatus 200 "OK"
              Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
         _ -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
