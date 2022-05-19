{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend where

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
    el "ul" $ do
        elAttr "a" ("href" =: "/") $ el "li" $ text "Home"
        elAttr "a" ("href" =: "https://www.w3schools.com") $ el "li" $ text "Pagina 2"
        elAttr "a" ("href" =: "#") $ el "li" $ text "Pagina 3"
        elAttr "a" ("href" =: "#") $ el "li" $ text "Pagina 4"


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Projeto - Haskell"
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" $ text "Projeto - Haskell"
      menu
      el "h2" $ text "Sobre"
      el "h3" $ text "Nome: Amauri Carvalho"
      el "h3" $ text "Nome: Marcos de Jesus"
      el "p" $ text "Página em desenvolvimento."

      return ()
  }