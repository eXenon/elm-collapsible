-- Small module of a material design collapsible component


module Main exposing (..)

import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)
import Collapsible exposing (Section, section, close, open, toggle, Style(Accordion, Collapsible), view)


type alias Model =
    { elements : List (Section Msg)
    }


type Msg
    = Toggle String


init =
    { elements =
        [ section "tab1" (text "hello") (div [] [ button [ onClick (Toggle "tab1") ] [ text "world !" ] ])
        , section "tab2" (text "foo") (text "bar")
        ]
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Toggle id ->
            { model | elements = toggle Collapsible id model.elements }


demo : Model -> Html Msg
demo model =
    div []
        [ view Toggle model.elements ]


main =
    Html.beginnerProgram
        { model = init
        , view = demo
        , update = update
        }
