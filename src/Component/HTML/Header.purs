module Geopsy.Component.HTML.Header where

import Prelude

import Geopsy.Data.Route (Route(..), routeCodec)
import Halogen.HTML (AttrName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties (ButtonType(..))
import Halogen.HTML.Properties as HP
import Routing.Duplex (print)

header :: forall i w. Route -> HH.HTML i w
header route =
    HH.nav
        [ HP.classes
            [ HH.ClassName "navbar"
            , HH.ClassName "navbar-dark"
            , HH.ClassName "navbar-expand-lg"
            , HH.ClassName "bg-success"
            ]
        ]
        [ HH.a
            [ HP.classes [ HH.ClassName "navbar-brand" ]
            , HP.href "#" ]
            [ HH.text "Geopsy" ]
        , HH.button
            [ HP.classes [ HH.ClassName "navbar-toggler" ]
            , HP.type_ ButtonButton
            , HP.attr (AttrName "data-toggle") "collapse"
            , HP.attr (AttrName "data-target") "#navbarSupportedContent"
            , HP.attr (AttrName "aria-controls") "navbarSupportedContent"
            , HP.attr (AttrName "aria-expanded") "false"
            , HP.attr (AttrName "aria-label") "Toggle navigation"
            ]
            [ HH.span [ HP.classes [ HH.ClassName "navbar-toggler-icon" ] ] [] ]


        , HH.div
            [ HP.classes
                [ HH.ClassName "collapse"
                , HH.ClassName "navbar-collapse"
                ]
            , HP.id_ "navbarSupportedContent" ]
            [ HH.ul
                [ HP.classes
                    [ HH.ClassName "navbar-nav"
                    , HH.ClassName "mr-auto"
                    ]
                ]
                [ li Home "home"
                , li ViewMap "View Map"
                ]
            , HH.ul
                [ HP.classes
                    [ HH.ClassName "navbar-nav"
                    , HH.ClassName "ml-auto"
                    ]
                ]
                [ li Login "Login"
                , li Register "Register"
                ]
            ]
        ]

    where
        li :: forall i' w'. Route -> String -> HH.HTML i' w'
        li r content
            = HH.li
                [ HP.classes $
                    [ HH.ClassName "nav-item" ]
                    <> if r == route
                        then [ HH.ClassName "active"  ]
                        else []
                ]
                [ HH.a
                    [ HP.classes [ HH.ClassName "nav-link" ]
                    , HP.href $ "#" <> print routeCodec r ]
                    [ HH.text content ]
                ]
        
        