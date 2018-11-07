-- Small module of a material design collapsible component


module Collapsible exposing (Section, section, close, open, toggle, view, Style(Accordion, Collapsible))

import Html exposing (Html, div, text, li, ul, label, input, p, node)
import Html.Attributes exposing (style, type_, class, name, for, id)
import Html.Events exposing (onClick)
import List exposing (map)
import Maybe


type Section msg
    = Section
        { header : Html msg
        , content : Html msg
        , id : String
        , open : Bool
        }


type Style
    = Accordion
    | Collapsible



-- Generic helpers


mapIfElse : (a -> Bool) -> (a -> a) -> (a -> a) -> List a -> List a
mapIfElse condition f1 f2 l =
    let
        real_f =
            \e ->
                if condition e then
                    f1 e
                else
                    f2 e
    in
        map real_f l


isId : String -> Section msg -> Bool
isId id (Section element) =
    element.id == id



-- Public functions


section : String -> Html msg -> Html msg -> Section msg
section id header content =
    Section { id = id, header = header, content = content, open = False }


open : Style -> String -> List (Section msg) -> List (Section msg)
open style id elements =
    let
        default =
            case style of
                Collapsible ->
                    identity

                Accordion ->
                    closeSection
    in
        mapIfElse (isId id) openSection default elements


close : Style -> String -> List (Section msg) -> List (Section msg)
close _ id elements =
    mapIfElse (isId id) closeSection identity elements


toggle : Style -> String -> List (Section msg) -> List (Section msg)
toggle style id elements =
    let
        operation =
            elements
                |> List.filter (isId id)
                |> List.head
                |> Maybe.map (\(Section e) -> e.open)
                |> Maybe.map
                    (\b ->
                        if b then
                            close style id
                        else
                            open style id
                    )
                |> Maybe.withDefault identity
    in
        operation elements



-- Private helper functions


openSection : Section msg -> Section msg
openSection (Section element) =
    Section { element | open = True }


closeSection : Section msg -> Section msg
closeSection (Section element) =
    Section { element | open = False }


toggleSection : Section msg -> Section msg
toggleSection (Section element) =
    Section { element | open = (not element.open) }



-- View functions


openStyle =
    [ ( "max-height", "100vh" ) ]


closedStyle =
    [ ( "max-height", "0" ) ]


styleNode =
    node "style" [] [ text "/* Acordeon styles */\n.tab {\n  position: relative;\n  margin-bottom: 1px;\n  width: 100%;\n  color: #fff;\n  overflow: hidden;\n}\n.tab input {\n  position: absolute;\n  opacity: 0;\n  z-index: -1;\n}\n.tab label {\n  position: relative;\n  display: block;\n  padding: 0 0 0 1em;\n  background: #16a085;\n  font-weight: bold;\n  line-height: 3;\n  cursor: pointer;\n}\n.tab-content {\n  max-height: 0;\n  overflow: hidden;\n  background: #1abc9c;\n  -webkit-transition: max-height .35s;\n  -o-transition: max-height .35s;\n  transition: max-height .35s;\n}\n.tab-content p {\n  margin: 1em;\n}\n/* Icon */\n.tab label::after {\n  position: absolute;\n  right: 0;\n  top: 0;\n  display: block;\n  width: 3em;\n  height: 3em;\n  line-height: 3;\n  text-align: center;\n  -webkit-transition: all .35s;\n  -o-transition: all .35s;\n  transition: all .35s;\n}\n.tab input[type=checkbox] + label::after {\n  content: \"+\";\n}\n.tab input[type=radio] + label::after {\n  content: \"ᦼ\";\n}\n.tab input[type=checkbox]:checked + label::after {\n  transform: rotate(45deg);\n}\n.tab input[type=radio]:checked + label::after {\n  transform: rotateX(180deg);\n}" ]


contentStyle : { a | open : Bool } -> List ( String, String )
contentStyle element =
    if element.open then
        openStyle
    else
        closedStyle


viewSection : (String -> msg) -> Section msg -> Html msg
viewSection msg (Section element) =
    div [ class "tab" ]
        [ input [ type_ "checkbox", name "tabs", id element.id ] []
        , label [ for element.id, onClick (msg element.id) ] [ element.header ]
        , div [ class "tab-content", style (contentStyle element) ] [ p [] [ element.content ] ]
        ]


view : (String -> msg) -> List (Section msg) -> Html msg
view msg elements =
    div []
        [ styleNode
        , div [] (map (viewSection msg) elements)
        ]
