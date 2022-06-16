{-# LANGUAGE DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Common.Api where
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

import Database.PostgreSQL.Simple


data BrasilCar = BrasilCar {
    brasilcarId :: Int,
    brasilcarMarca :: Text,
    brasilcarModelo :: Text,
    brasilcarAno :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)


data JapanCar = JapanCar {
    japancarId :: Int,
    japancarMarca :: Text,
    japancarModelo :: Text,
    japancarAno :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data EuropeCar = EuropeCar {
    europecarId :: Int,
    europecarMarca :: Text,
    europecarModelo :: Text,
    europecarAno :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

commonStuff :: String
commonStuff = "Here is a string defined in Common.Api"
