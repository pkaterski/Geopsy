module Geopsy.Component.Router where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
--import Geopsy.Component.Utils (OpaqueSlot)
import Geopsy.Data.Route (Route(..), routeCodec)
import Geopsy.Page.Home as Home
import Geopsy.Page.Login as Login
import Geopsy.Page.Register as Register
import Geopsy.Page.ViewMap as ViewMap
import Halogen as H
import Halogen.HTML as HH
import Profile (Profile)
import Routing.Duplex (print)
import Routing.Hash (setHash)

type State =
  { route :: Maybe Route
  , currentUser :: Maybe Profile
  }

data Query a
  = Navigate Route a

data Action
  = Initialize

-- type ChildSlots =
--   ( home :: OpaqueSlot Unit
--   , login :: OpaqueSlot Unit
--   , register :: OpaqueSlot Unit
--   , viewMap :: OpaqueSlot Unit
--   )
type OpaqueSlot = forall query. H.Slot query Void Unit

type ChildSlots =
  ( home     :: OpaqueSlot
  , login    :: OpaqueSlot
  , register :: OpaqueSlot
  , viewMap  :: OpaqueSlot
  )

component :: forall m. MonadEffect m => H.Component HH.HTML Query {} Void m
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
    handleAction :: MonadEffect m => Action -> H.HalogenM State Action ChildSlots Void m Unit
    handleAction = case _ of
      Initialize -> do
        H.liftEffect <<< setHash <<< print routeCodec $ ViewMap
        pure unit
  
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

          
