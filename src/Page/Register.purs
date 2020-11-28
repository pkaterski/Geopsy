module Geopsy.Page.Register where

import Prelude

import Geopsy.Component.HTML.Header (header)
import Geopsy.Data.Route (Route(..))
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
    render _ =
      HH.div_
        [ header Register
        , HH.h1_ [ HH.text "Register" ]
        ]
