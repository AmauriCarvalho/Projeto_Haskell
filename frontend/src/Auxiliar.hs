{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Auxiliar where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route


menu :: DomBuilder t m => m ()
menu = do
  elAttr "div" ("id" =: "menu") $ do   
    el "ul" $ do
        el "li" $ do
            elAttr "a" ("href" =: "#") (text "Home")
        el "li" $ do
            elAttr "a" ("href" =: "#") (text  "Japanese Cars")
        el "li" $ do
            elAttr "a" ("href" =: "#") (text  "Brazillian Cars")
        el "li" $ do
            elAttr "a" ("href" =: "#") (text  "European Cars")
