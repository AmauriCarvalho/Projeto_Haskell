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
import Data.Map.Strict

import Common.Api
import Common.Route
import Auxiliar

caixas :: (DomBuilder t m, PostBuild t m) => m ()
caixas = el "div" $ do
  t <- inputElement def -- m (Dynamic Text)
  --s <- inputElement def -- m (Dynamic Text)
  text " "

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "WMC"
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      menu
      el "h1" $ text "World Model Cars"
      --el "h2" $ text $ T.pack commonStuff
      el "h2" $ text "leave your suggestion to add to the list"
      --el "h3" $ text "HELLO WORLD!!!!!!"

      el "label" $ text "Car name/model"
      caixas
      elAttr "div" ("id" =: "sendButton") (text "Send")
        
      -- `prerender` and `prerender_` let you choose a widget to run on the server
      -- during prerendering and a different widget to run on the client with
      -- JavaScript. The following will generate a `blank` widget on the server and
      -- print "Hello, World!" on the client.
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)

      --elAttr "img" ("src" =: $(static "obelisk.jpg")) blank
      --el "div" $ do
        --exampleConfig <- getConfig "common/example"
        --case exampleConfig of
          --Nothing -> text "No config file found in config/common/example"
          --Just s -> text $ T.decodeUtf8 s
      --return ()
  }
