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

--menu old
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

caixas :: (DomBuilder t m, PostBuild t m) => m ()
caixas = el "div" $ do
  t <- inputElement def -- m (Dynamic Text)
  --s <- inputElement def -- m (Dynamic Text)
  text " "

home :: (DomBuilder t m, PostBuild t m) => m ()
home = do
    el "h1" $ text "World Model Cars"
      --el "h2" $ text $ T.pack commonStuff
    el "h2" $ text "leave your suggestion to add to the list"
      --el "h3" $ text "HELLO WORLD!!!!!!"
    el "label" $ text "Car name/model"
    caixas
    elAttr "div" ("id" =: "sendButton") (text "Send")

pagJap :: DomBuilder t m => m ()
pagJap = do
    el "h1" $ text "Japan car list"

    elAttr "table" ("class" =: "tabelaCar") $ do
      el "thead" $ do
        el "tr" $ do
        el "th" $ text "Id"
        el "th" $ text "Marca"
        el "th" $ text "Modelo"
        el "th" $ text "Ano"
        el "th" $ text "Preço"
      el "tbody" $ do 
        el "tr" $ do 
          el "td" $ text "--"
          el "td" $ text "--"
          el "td" $ text "--"
          el "td" $ text "--"
          el "td" $ text "--"


pagBra :: DomBuilder t m => m ()
pagBra = do
    el "h1" $ text "Brazil car list"

    elAttr "table" ("class" =: "tabelaCar") $ do
      el "thead" $ do
        el "tr" $ do
        el "th" $ text "Id"
        el "th" $ text "Marca"
        el "th" $ text "Modelo"
        el "th" $ text "Ano"
        el "th" $ text "Preço"
      el "tbody" $ do 
        el "tr" $ do 
          el "td" $ text "--"
          el "td" $ text "--"
          el "td" $ text "--"
          el "td" $ text "--"
          el "td" $ text "--"

pagEur :: DomBuilder t m => m ()
pagEur = do
    el "h1" $ text "Europe car list"

    elAttr "table" ("class" =: "tabelaCar") $ do
      el "thead" $ do
        el "tr" $ do
        el "th" $ text "Id"
        el "th" $ text "Marca"
        el "th" $ text "Modelo"
        el "th" $ text "Ano"
        el "th" $ text "Preço"
      el "tbody" $ do 
        el "tr" $ do 
          el "td" $ text "--"
          el "td" $ text "--"
          el "td" $ text "--"
          el "td" $ text "--"
          el "td" $ text "--"

pagSob :: DomBuilder t m => m ()
pagSob = do
  el "h1" $ text "About this web site"

  elAttr "p" ("class" =: "textoSobre") $ text "The purpose of this site is to gather a list of different car models around the world, forming an accessible repository of information about vehicle models."

footPg :: DomBuilder t m => m ()
footPg = do
  el "footer" $ do
    el "address" $ do 
      el "p" $ text "Website made using haskell by Amauri Carvalho e Marcos de Jesus"