module Geopsy.Component.Router where

import Data.Maybe
import Prelude

import Data.Symbol (SProxy(..))
import Geopsy.Component.Utils (OpaqueSlot)
import Geopsy.Data.Route (Route(..))
import Geopsy.Page.Home as Home
import Geopsy.Page.Login as Login
import Geopsy.Page.Register as Register
import Geopsy.Page.ViewMap as ViewMap
import Halogen as H
import Halogen.HTML as HH
import Profile (Profile)

type State =
  { route :: Maybe Route
  , currentUser :: Maybe Profile
  }

data Query a
  = Navigate Route a

-- for now this is meaningless
data Action
  = Initialize

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , login :: OpaqueSlot Unit
  , register :: OpaqueSlot Unit
  , viewMap :: OpaqueSlot Unit
  )

component :: forall m. H.Component HH.HTML Query {} Void m
component = H.mkComponent
  { initialState: \_ -> { route: Nothing, currentUser: Nothing }
  , render
  , eval : H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , initialize = Just Initialize

      }
  }
  where
    handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
    handleAction = case _ of
      Initialize -> pure unit
  
    handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
    handleQuery = case _ of
      Navigate dest a -> do
        pure unit
        { route, currentUser } <- H.get
        when (route /= Just dest) do
          H.modify_ _ { route = Just dest } 
        pure $ Just a

    render :: State -> H.ComponentHTML Action ChildSlots m
    render { route, currentUser } = case route of
      Just r -> case r of
        Home ->
          HH.slot (SProxy :: _ "home") unit Home.component unit absurd
        Login ->
          HH.slot (SProxy :: _ "login") unit Login.component unit absurd
        Register ->
          HH.slot (SProxy :: _ "register") unit Register.component unit absurd
        ViewMap ->
          HH.slot (SProxy :: _ "viewMap") unit ViewMap.component unit absurd
      Nothing ->
        HH.div_ [ HH.text "Oh no! That page wasn't found." ]

          




