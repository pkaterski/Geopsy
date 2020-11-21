module Geopsy.Page.ViewMap where

import Prelude

import Halogen as H
import Halogen.HTML as HH


component :: forall q i o m. H.Component HH.HTML q i o m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
  }
  where 
    render :: forall a s. s -> H.ComponentHTML a () m
    render _ = HH.h1_ [ HH.text "View Map" ]
