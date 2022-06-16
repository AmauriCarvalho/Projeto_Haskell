{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Auxiliar where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Data.Aeson
import Data.Maybe
import Data.Text
import Control.Monad.Fix
import Data.Map (Map)

import Text.Read

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

numberInputDyn :: (DomBuilder t m, Num a, Read a, Show a) =>
               Event t a -> m (Dynamic t a)
numberInputDyn p = do
      val <- return (fmap (T.pack . show) p)
      n <- inputElement $ def
        & inputElementConfig_setValue .~ val
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) 
                 (_inputElement_value n)

  
numberInput :: (DomBuilder t m, Num a, Read a) => m (Dynamic t a)
numberInput = do
    n <- inputElement $ def
      & inputElementConfig_initialValue .~ "0"
      & inputElementConfig_elementConfig
      . elementConfig_initialAttributes .~ ("type" =: "number")
    return $ fmap (fromMaybe 0 . readMaybe . unpack) (_inputElement_value n)

home :: (DomBuilder t m, PostBuild t m) => m ()
home = do
    el "h1" $ text "World Model Cars"
      --el "h2" $ text $ T.pack commonStuff
    el "h2" $ text "leave your suggestion to add to the list"
      --el "h3" $ text "HELLO WORLD!!!!!!"
    el "label" $ text "Car name/model"
    caixas
    elAttr "div" ("id" =: "sendButton") (text "Send")
    
    
sendRequest :: ToJSON a => R BackendRoute -> a -> XhrRequest T.Text
sendRequest r dados = postJson (getPath r) dados

getPath :: R BackendRoute -> T.Text
getPath p = renderBackendRoute checFullREnc p

    
-- Japan Page

getListReqJp :: XhrRequest ()
getListReqJp = xhrRequest "GET" (getPath (BackendRoute_ListarJp :/ ())) def
          

reqJpCar :: ( DomBuilder t m, Prerender t m) => m ()
reqJpCar = do
   elAttr "div" ("class" =: "addCon") $ do
	   elAttr "h1" ("class" =: "titleAdd") (text "Add Car")
	   elAttr "label" ("class" =: "labelTi") (text "Marca")
	   marca <- inputElement def
	   elAttr "label" ("class" =: "labelTi") (text "Modelo")
	   modelo <- inputElement def
	   elAttr "label" ("class" =: "labelTi") (text "Ano")
	   ano <- numberInput
	   let jp = fmap (\((m,md),a) -> JapanCar 0 md a m)  (zipDyn (zipDyn ano (_inputElement_value marca))(_inputElement_value modelo))
	   (submitBtn,_) <- el' "button" (text "Inserir")
	   let click = domEvent Click submitBtn
	   let jpcarEvt = tag (current jp) click
	   _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
	     (pure never)
	     (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_JapanCar :/ ()) <$> jpcarEvt))
	   return ()
   

-- Workflow BR

tabJapanCar :: (PostBuild t m, DomBuilder t m) => Dynamic t JapanCar
                                                -> m (Event t Acao)
tabJapanCar jp = do
    el "tr" $ do
      el "td" (dynText $ fmap (T.pack . show . japancarId) jp)
      el "td" (dynText $ fmap (T.pack . show .japancarMarca) jp)
      el "td" (dynText $ fmap (T.pack . show . japancarModelo) jp)
      el "td" (dynText $ fmap (T.pack . show . japancarAno) jp)
      evt1 <- elAttr "td" ("class" =: "perfilButton") $ fmap (fmap (const PerfilJp)) (button "perfil")
      evt2 <- elAttr "td" ("class" =: "editarButton") $ fmap (fmap (const EditarJp)) (button "editar")
      return (attachPromptlyDynWith (flip ($)) (fmap japancarId jp) (leftmost [evt1,evt2]))
        

pagPerfilJp :: ( DomBuilder t m
            , Prerender t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
pagPerfilJp pid = Workflow $ do
    el "h1" (text "Perfil")
    elAttr "div" ("class" =: "containerPerfil") $ do
	    btn <- button "Exibir"
	    jap :: Dynamic t (Event t (Maybe JapanCar)) <- prerender
		(pure never)
		(fmap decodeXhrResponse <$>
		performRequestAsync (const (getJpReq pid) <$> btn))
	    mdyn <- holdDyn Nothing (switchDyn jap)
	    dynP <- return ((fromMaybe (JapanCar 0 "" "" 0)) <$> mdyn)
	    el "div" $ do
		el "div" (dynText $ fmap japancarMarca dynP)
		el "div" (dynText $ fmap (T.pack . show . japancarModelo) dynP)
		el "div" (dynText $ fmap (T.pack . show . japancarAno) dynP)
	    ret <- button "voltar"
	    return ("Perfil: " <> (T.pack $ show pid), reqTabelaJp <$ ret)


getJpReq :: Int -> XhrRequest ()
getJpReq pid = xhrRequest "GET" (getPath (BackendRoute_BuscarJp :/ pid)) def


	    
reqTabelaJp :: ( DomBuilder t m
              , Prerender t m
              , MonadHold t m
              , MonadFix m
              , PostBuild t m) => Workflow t m T.Text
reqTabelaJp = Workflow $ do
    elAttr "div" ("class" =: "reqTabelaBr") $ do
	    btn <- button "Listar"
	    jap :: Dynamic t (Event t (Maybe [JapanCar])) <- prerender
		  (pure never)
		  (fmap decodeXhrResponse <$> performRequestAsync (const getListReqJp <$> btn))
	    evt <- return (fmap (fromMaybe []) $ switchDyn jap)
	    dynP <- foldDyn (++) [] evt
	    tb <- elAttr "table" ("class" =: "tabelaCar")$ do
	      el "thead" $ do
		el "tr" $ do
		  el "th" (text "Id")
		  el "th" (text "Marca")
		  el "th" (text "Modelo")
		  el "th" (text "Ano")
		  el "th" blank

	      el "tbody" $ do
		  simpleList dynP tabJapanCar
	    tb' <- return $ switchDyn $ fmap leftmost tb
	    return ("", escolherPagJp <$> tb')
	    where
		escolherPagJp (PerfilJp pid) = pagPerfilJp pid
		escolherPagJp (EditarJp pid) = editarPerfilJp pid

editarPerfilJp :: ( DomBuilder t m
                , Prerender t m
                , MonadHold t m
                , MonadFix m
                , PostBuild t m) => Int -> Workflow t m T.Text
editarPerfilJp pid = Workflow $ do
    elAttr "div" ("class" =: "addCon") $ do
      elAttr "h1" ("class" =: "titleAdd") (text "Edit Car")
      btn <- button "Exibir dados atuais"
      jap :: Dynamic t (Event t (Maybe JapanCar)) <- prerender
          (pure never)
          (fmap decodeXhrResponse <$> performRequestAsync
              (const (getJpReq pid) <$> btn))
      mdyn <- return (switchDyn jap)
      dynE <- return ((fromMaybe (JapanCar 0 "" "" 0)) <$> mdyn)

      elAttr "label" ("class" =: "labelTi") (text "Marca") 
      marca <- inputElement $
            def & inputElementConfig_setValue .~ (fmap japancarMarca dynE)
      elAttr "label" ("class" =: "labelTi") (text "Modelo") 
      modelo <- inputElement $
            def & inputElementConfig_setValue .~ (fmap japancarModelo dynE)
      elAttr "label" ("class" =: "labelTi") (text "Ano") 
      ano <- numberInputDyn (fmap japancarAno dynE)

      let jap = fmap (\((a,m),md) -> JapanCar 0 m md a) (zipDyn (zipDyn ano (_inputElement_value marca))(_inputElement_value modelo))    
      submitBtn <- button "Editar"
      let japEvt = tag (current jap) submitBtn
      _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
          (pure never)
          (fmap decodeXhrResponse <$>
              performRequestAsync (sendRequest (BackendRoute_EditarJp :/ pid)
              <$> japEvt))
      return ("Perfil: " <> (T.pack $ show pid), reqTabelaJp <$ submitBtn) 




reqListaJp :: ( DomBuilder t m
            , Prerender t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqListaJp = do
    r <- workflow reqTabelaJp
    el "div" (dynText r)   

pagJap :: ( DomBuilder t m, Prerender t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
pagJap = do
    el "h1" $ text "Japan car list"
    reqListaJp
          
-- END Japan Page

-- Brasil Page

getListReqBr :: XhrRequest ()
getListReqBr = xhrRequest "GET" (getPath (BackendRoute_ListarBr :/ ())) def
          

reqBrCar :: ( DomBuilder t m, Prerender t m) => m ()
reqBrCar = do
   elAttr "div" ("class" =: "addCon") $ do
	   elAttr "h1" ("class" =: "titleAdd") (text "Add Car")
	   elAttr "label" ("class" =: "labelTi") (text "Marca")
	   marca <- inputElement def
	   elAttr "label" ("class" =: "labelTi") (text "Modelo")
	   modelo <- inputElement def
	   elAttr "label" ("class" =: "labelTi") (text "Ano")
	   ano <- numberInput
	   let br = fmap (\((m,md),a) -> BrasilCar 0 md a m)  (zipDyn (zipDyn ano (_inputElement_value marca))(_inputElement_value modelo))
	   (submitBtn,_) <- el' "button" (text "Inserir")
	   let click = domEvent Click submitBtn
	   let brcarEvt = tag (current br) click
	   _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
	     (pure never)
	     (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_BrasilCar :/ ()) <$> brcarEvt))
	   return ()
   

-- Workflow BR

data Acao = Perfil Int | Editar Int | PerfilJp Int | EditarJp Int | PerfilEu Int | EditarEu Int

tabBrasilCar :: (PostBuild t m, DomBuilder t m) => Dynamic t BrasilCar
                                                -> m (Event t Acao)
tabBrasilCar br = do
    el "tr" $ do
      el "td" (dynText $ fmap (T.pack . show . brasilcarId) br)
      el "td" (dynText $ fmap (T.pack . show .brasilcarMarca) br)
      el "td" (dynText $ fmap (T.pack . show . brasilcarModelo) br)
      el "td" (dynText $ fmap (T.pack . show . brasilcarAno) br)
      evt1 <- elAttr "td" ("class" =: "perfilButton") $ fmap (fmap (const Perfil)) (button "perfil")
      evt2 <- elAttr "td" ("class" =: "editarButton") $ fmap (fmap (const Editar)) (button "editar")
      return (attachPromptlyDynWith (flip ($)) (fmap brasilcarId br) (leftmost [evt1,evt2]))
        

pagPerfil :: ( DomBuilder t m
            , Prerender t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
pagPerfil pid = Workflow $ do
    el "h1" (text "Perfil")
    elAttr "div" ("class" =: "containerPerfil") $ do
	    btn <- button "Exibir"
	    bra :: Dynamic t (Event t (Maybe BrasilCar)) <- prerender
		(pure never)
		(fmap decodeXhrResponse <$>
		performRequestAsync (const (getBrReq pid) <$> btn))
	    mdyn <- holdDyn Nothing (switchDyn bra)
	    dynP <- return ((fromMaybe (BrasilCar 0 "" "" 0)) <$> mdyn)
	    el "div" $ do
		el "div" (dynText $ fmap brasilcarMarca dynP)
		el "div" (dynText $ fmap (T.pack . show . brasilcarModelo) dynP)
		el "div" (dynText $ fmap (T.pack . show . brasilcarAno) dynP)
	    ret <- button "voltar"
	    return ("Perfil: " <> (T.pack $ show pid), reqTabelaBr <$ ret)


getBrReq :: Int -> XhrRequest ()
getBrReq pid = xhrRequest "GET" (getPath (BackendRoute_BuscarBr :/ pid)) def


	    
reqTabelaBr :: ( DomBuilder t m
              , Prerender t m
              , MonadHold t m
              , MonadFix m
              , PostBuild t m) => Workflow t m T.Text
reqTabelaBr = Workflow $ do
    elAttr "div" ("class" =: "reqTabelaBr") $ do
	    btn <- button "Listar"
	    prods :: Dynamic t (Event t (Maybe [BrasilCar])) <- prerender
		  (pure never)
		  (fmap decodeXhrResponse <$> performRequestAsync (const getListReqBr <$> btn))
	    evt <- return (fmap (fromMaybe []) $ switchDyn prods)
	    dynP <- foldDyn (++) [] evt
	    tb <- elAttr "table" ("class" =: "tabelaCar")$ do
	      el "thead" $ do
		el "tr" $ do
		  el "th" (text "Id")
		  el "th" (text "Marca")
		  el "th" (text "Modelo")
		  el "th" (text "Ano")
		  el "th" blank

	      el "tbody" $ do
		  simpleList dynP tabBrasilCar
	    tb' <- return $ switchDyn $ fmap leftmost tb
	    return ("", escolherPag <$> tb')
	    where
		escolherPag (Perfil pid) = pagPerfil pid
		escolherPag (Editar pid) = editarPerfilBr pid

editarPerfilBr :: ( DomBuilder t m
                , Prerender t m
                , MonadHold t m
                , MonadFix m
                , PostBuild t m) => Int -> Workflow t m T.Text
editarPerfilBr pid = Workflow $ do
    elAttr "div" ("class" =: "addCon") $ do
      elAttr "h1" ("class" =: "titleAdd") (text "Edit Car")
      btn <- button "Exibir dados atuais"
      prod :: Dynamic t (Event t (Maybe BrasilCar)) <- prerender
          (pure never)
          (fmap decodeXhrResponse <$> performRequestAsync
              (const (getBrReq pid) <$> btn))
      mdyn <- return (switchDyn prod)
      dynE <- return ((fromMaybe (BrasilCar 0 "" "" 0)) <$> mdyn)

      elAttr "label" ("class" =: "labelTi") (text "Marca") 
      marca <- inputElement $
            def & inputElementConfig_setValue .~ (fmap brasilcarMarca dynE)
      elAttr "label" ("class" =: "labelTi") (text "Modelo") 
      modelo <- inputElement $
            def & inputElementConfig_setValue .~ (fmap brasilcarModelo dynE)
      elAttr "label" ("class" =: "labelTi") (text "Ano") 
      ano <- numberInputDyn (fmap brasilcarAno dynE)

      let prod = fmap (\((a,m),md) -> BrasilCar 0 m md a) (zipDyn (zipDyn ano (_inputElement_value marca))(_inputElement_value modelo))    
      submitBtn <- button "Editar"
      let prodEvt = tag (current prod) submitBtn
      _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
          (pure never)
          (fmap decodeXhrResponse <$>
              performRequestAsync (sendRequest (BackendRoute_EditarBr :/ pid)
              <$> prodEvt))
      return ("Perfil: " <> (T.pack $ show pid), reqTabelaBr <$ submitBtn) 




reqListaBr :: ( DomBuilder t m
            , Prerender t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqListaBr = do
    r <- workflow reqTabelaBr
    el "div" (dynText r)   

    
	    
	    

pagBra :: ( DomBuilder t m, Prerender t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
pagBra = do
    
    el "h1" $ text "Brazil car list"
    reqListaBr
   -- reqListaBr
  
          
-- END Brasil Page

-- Page Europe

getListReqEu :: XhrRequest ()
getListReqEu = xhrRequest "GET" (getPath (BackendRoute_ListarEu :/ ())) def
          

reqEuCar :: ( DomBuilder t m, Prerender t m) => m ()
reqEuCar = do
   elAttr "div" ("class" =: "addCon") $ do
	   elAttr "h1" ("class" =: "titleAdd") (text "Add Car")
	   elAttr "label" ("class" =: "labelTi") (text "Marca")
	   marca <- inputElement def
	   elAttr "label" ("class" =: "labelTi") (text "Modelo")
	   modelo <- inputElement def
	   elAttr "label" ("class" =: "labelTi") (text "Ano")
	   ano <- numberInput
	   let eu = fmap (\((m,md),a) -> EuropeCar 0 md a m)  (zipDyn (zipDyn ano (_inputElement_value marca))(_inputElement_value modelo))
	   (submitBtn,_) <- el' "button" (text "Inserir")
	   let click = domEvent Click submitBtn
	   let eucarEvt = tag (current eu) click
	   _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
	     (pure never)
	     (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_EuropeCar :/ ()) <$> eucarEvt))
	   return ()
   

-- Workflow EUR

tabEuropeCar :: (PostBuild t m, DomBuilder t m) => Dynamic t EuropeCar
                                                -> m (Event t Acao)
tabEuropeCar eu = do
    el "tr" $ do
      el "td" (dynText $ fmap (T.pack . show . europecarId) eu)
      el "td" (dynText $ fmap (T.pack . show .europecarMarca) eu)
      el "td" (dynText $ fmap (T.pack . show . europecarModelo) eu)
      el "td" (dynText $ fmap (T.pack . show . europecarAno) eu)
      evt1 <- elAttr "td" ("class" =: "perfilButton") $ fmap (fmap (const PerfilEu)) (button "perfil")
      evt2 <- elAttr "td" ("class" =: "editarButton") $ fmap (fmap (const EditarEu)) (button "editar")
      return (attachPromptlyDynWith (flip ($)) (fmap europecarId eu) (leftmost [evt1,evt2]))
        

pagPerfilEu :: ( DomBuilder t m
            , Prerender t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
pagPerfilEu pid = Workflow $ do
    el "h1" (text "Perfil")
    elAttr "div" ("class" =: "containerPerfil") $ do
	    btn <- button "Exibir"
	    eur :: Dynamic t (Event t (Maybe EuropeCar)) <- prerender
		(pure never)
		(fmap decodeXhrResponse <$>
		performRequestAsync (const (getEuReq pid) <$> btn))
	    mdyn <- holdDyn Nothing (switchDyn eur)
	    dynP <- return ((fromMaybe (EuropeCar 0 "" "" 0)) <$> mdyn)
	    el "div" $ do
		el "div" (dynText $ fmap europecarMarca dynP)
		el "div" (dynText $ fmap (T.pack . show . europecarModelo) dynP)
		el "div" (dynText $ fmap (T.pack . show . europecarAno) dynP)
	    ret <- button "voltar"
	    return ("Perfil: " <> (T.pack $ show pid), reqTabelaEu <$ ret)


getEuReq :: Int -> XhrRequest ()
getEuReq pid = xhrRequest "GET" (getPath (BackendRoute_BuscarEu :/ pid)) def


	    
reqTabelaEu :: ( DomBuilder t m
              , Prerender t m
              , MonadHold t m
              , MonadFix m
              , PostBuild t m) => Workflow t m T.Text
reqTabelaEu = Workflow $ do
    elAttr "div" ("class" =: "reqTabelaBr") $ do
	    btn <- button "Listar"
	    eur :: Dynamic t (Event t (Maybe [EuropeCar])) <- prerender
		  (pure never)
		  (fmap decodeXhrResponse <$> performRequestAsync (const getListReqEu <$> btn))
	    evt <- return (fmap (fromMaybe []) $ switchDyn eur)
	    dynP <- foldDyn (++) [] evt
	    tb <- elAttr "table" ("class" =: "tabelaCar")$ do
	      el "thead" $ do
		el "tr" $ do
		  el "th" (text "Id")
		  el "th" (text "Marca")
		  el "th" (text "Modelo")
		  el "th" (text "Ano")
		  el "th" blank

	      el "tbody" $ do
		  simpleList dynP tabEuropeCar
	    tb' <- return $ switchDyn $ fmap leftmost tb
	    return ("", escolherPagEu <$> tb')
	    where
		escolherPagEu (PerfilEu pid) = pagPerfilEu pid
		escolherPagEu (EditarEu pid) = editarPerfilEu pid

editarPerfilEu :: ( DomBuilder t m
                , Prerender t m
                , MonadHold t m
                , MonadFix m
                , PostBuild t m) => Int -> Workflow t m T.Text
editarPerfilEu pid = Workflow $ do
    elAttr "div" ("class" =: "addCon") $ do
      elAttr "h1" ("class" =: "titleAdd") (text "Edit Car")
      btn <- button "Exibir dados atuais"
      eur :: Dynamic t (Event t (Maybe EuropeCar)) <- prerender
          (pure never)
          (fmap decodeXhrResponse <$> performRequestAsync
              (const (getEuReq pid) <$> btn))
      mdyn <- return (switchDyn eur)
      dynE <- return ((fromMaybe (EuropeCar 0 "" "" 0)) <$> mdyn)

      elAttr "label" ("class" =: "labelTi") (text "Marca") 
      marca <- inputElement $
            def & inputElementConfig_setValue .~ (fmap europecarMarca dynE)
      elAttr "label" ("class" =: "labelTi") (text "Modelo") 
      modelo <- inputElement $
            def & inputElementConfig_setValue .~ (fmap europecarModelo dynE)
      elAttr "label" ("class" =: "labelTi") (text "Ano") 
      ano <- numberInputDyn (fmap europecarAno dynE)

      let eur = fmap (\((a,m),md) -> EuropeCar 0 m md a) (zipDyn (zipDyn ano (_inputElement_value marca))(_inputElement_value modelo))    
      submitBtn <- button "Editar"
      let eurEvt = tag (current eur) submitBtn
      _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
          (pure never)
          (fmap decodeXhrResponse <$>
              performRequestAsync (sendRequest (BackendRoute_EditarEu :/ pid)
              <$> eurEvt))
      return ("Perfil: " <> (T.pack $ show pid), reqTabelaEu <$ submitBtn) 




reqListaEu :: ( DomBuilder t m
            , Prerender t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqListaEu = do
    r <- workflow reqTabelaEu
    el "div" (dynText r)   


pagEur :: ( DomBuilder t m, Prerender t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
pagEur = do
    el "h1" $ text "Europe car list"
    reqListaEu


-- END Page Europe

pagSob :: DomBuilder t m => m ()
pagSob = do
  el "h1" $ text "About this web site"

  elAttr "p" ("class" =: "textoSobre") $ text "The purpose of this site is to gather a list of different car models around the world, forming an accessible repository of information about vehicle models."

footPg :: DomBuilder t m => m ()
footPg = do
  el "footer" $ do
    el "address" $ do 
      el "p" $ text "Website made using haskell by Amauri Carvalho e Marcos de Jesus"
