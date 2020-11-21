module Main where

import Data.Maybe
import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (log)
import Geopsy.Component.Router as Router
import Geopsy.Data.Route (Route(..), routeCodec)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse, print)
import Routing.Hash (matchesWith, setHash)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  H.liftEffect $ log "the app started"
  halogenIO <- runUI Router.component {} body
  -- navigate to Home on boot
  H.liftEffect <<< setHash <<< print routeCodec $ Home
  void $ H.liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      log $ show new
      launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new

    
