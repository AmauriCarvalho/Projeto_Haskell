{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Menu where

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

import Text.Read
import Data.Maybe
import Control.Monad.Fix
import Auxiliar

data Pagina = Pagina0 | Pagina1 | Pagina2 | Pagina3 | Pagina4 | Pagina5 | Pagina6 | Pagina7 | Pagina8

clickLi :: DomBuilder t m => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
    (ev, _) <- el' "li" (elAttr "a" ("href" =: "#") (text t))
    return ((\_ -> p) <$> domEvent Click ev)

menuLi :: (DomBuilder t m, MonadHold t m, Prerender t m) => m (Dynamic t Pagina)
menuLi = do
    evs <- el "ul" $ do
        p1 <- clickLi Pagina1 "Home"
        p2 <- clickLi Pagina2 "Japanese Cars"
        p3 <- clickLi Pagina3 "Add JP Cars"
        p4 <- clickLi Pagina4 "Brazillian Cars"
        p5 <- clickLi Pagina5 "Add BR Cars"
        p6 <- clickLi Pagina6 "European Cars"
        p7 <- clickLi Pagina7 "Add EU Cars"
        p8 <- clickLi Pagina8 "About" 
        return (leftmost [p1,p2,p3,p4,p5,p6,p7,p8])
    holdDyn Pagina0 evs
    
currPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Prerender t m) => Pagina -> m ()
currPag p = case p of
    Pagina0 -> home
    Pagina1 -> home
    Pagina2 -> pagJap
    Pagina3 -> reqJpCar
    Pagina4 -> pagBra
    Pagina5 -> reqBrCar
    Pagina6 -> pagEur
    Pagina7 -> reqEuCar
    Pagina8 -> pagSob
    
mainPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Prerender t m )=> m ()
mainPag = do
    pag <- elAttr "div" ("id" =: "menu") menuLi
    dyn_ $ currPag <$> pag
    
    
    
    
    
