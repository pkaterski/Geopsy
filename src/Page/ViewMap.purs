module Geopsy.Page.ViewMap where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Geopsy.Component.HTML.Header (header)
import Geopsy.Data.Route (Route(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

data Action
  = Initialize

type State
  = String


component :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
component = H.mkComponent
  { initialState: \s -> ""
  , render
  , eval: H.mkEval $ H.defaultEval
    { initialize = Just Initialize
    , handleAction = handleAction
    }
  }
  where 
    render :: forall a. State -> H.ComponentHTML a () m
    render s = HH.div_
      [ header ViewMap
      , HH.h1_ [ HH.text "View Map" ]
      , HH.h2_ [ HH.text s ]
      , HH.div
        [ HP.id_ "mapid"
        , HP.style "height: 700px; width: 99%; margin: auto;"
        ]
        []
      ]
    
    handleAction :: forall output.
      MonadEffect m =>
      Action ->
      H.HalogenM State Action () output m Unit
    handleAction Initialize = do
      H.liftEffect initMap
      pure unit


foreign import initMap :: Effect Unit

