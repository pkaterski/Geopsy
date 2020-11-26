module Geopsy.Page.ViewMap where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

data Action
  = Initialize


component :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
    { initialize = Just Initialize
    , handleAction = handleAction
    }
  }
  where 
    render :: forall a s. s -> H.ComponentHTML a () m
    render _ = HH.div_
      [ HH.h1_ [ HH.text "View Map" ]
      , HH.div
        [ HP.id_ "mapid"
        , HP.style "height: 700px; width: 99%; margin: auto;"
        ]
        []
      ]
    
    handleAction :: forall state output.
      MonadEffect m =>
      Action ->
      H.HalogenM state Action () output m Unit
    handleAction Initialize = do
      H.liftEffect $ initMap
      pure unit


foreign import initMap :: Effect Unit

