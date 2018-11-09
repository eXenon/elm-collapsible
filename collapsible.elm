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


globalStyle =
    [ ( "border-top", "1px solid #ddd" )
    , ( "border-right", "1px solid #ddd" )
    , ( "border-left", "1px solid #ddd" )
    , ( "-webkit-box-shadow", "0 2px 2px 0 rgba(0,0,0,0.14), 0 3px 1px -2px rgba(0,0,0,0.12), 0 1px 5px 0 rgba(0,0,0,0.2)" )
    , ( "box-shadow", "0 2px 2px 0 rgba(0,0,0,0.14), 0 3px 1px -2px rgba(0,0,0,0.12), 0 1px 5px 0 rgba(0,0,0,0.2)" )
    ]


tabStyle =
    [ ( "position", "relative" )
    , ( "margin-bottom", "1px" )
    , ( "width", "100%" )
    , ( "color", "rgba(0,0,0,0.87)" )
    , ( "font-size", "1.6" )
    , ( "overflow", "hidden" )
    ]


inputStyle =
    [ ( "position", "absolute" )
    , ( "opacity", "0" )
    , ( "z-index", "-1" )
    ]


labelStyle =
    [ ( "position", "relative" )
    , ( "display", "block" )
    , ( "padding", "0 0 0 1em" )
    , ( "background", "white" )
    , ( "line-height", "3" )
    , ( "cursor", "pointer" )
    , ( "border-bottom", "1px solid #ddd" )
    ]


contentStyle : { a | open : Bool } -> List ( String, String )
contentStyle element =
    let
        height =
            if element.open then
                ( "max-height", "100vh" )
            else
                ( "max-height", "0" )
    in
        [ height
        , ( "overflow", "hidden" )
        , ( "background", "white" )
        , ( "-webkit-transition", "max-height .35s" )
        , ( "-o-transition", "max-height .35s" )
        , ( "transition", "max-height .35s" )
        , ( "border-bottom", "1px solid #ddd" )
        ]


pStyle =
    [ ( "margin", "1em" ) ]


styleNode =
    node "style" [] [ text ".tab label::after {\n  position: absolute;\n  right: 0;\n  top: 0;\n  display: block;\n  width: 3em;\n  height: 3em;\n  line-height: 3;\n  text-align: center;\n  -webkit-transition: all .35s;\n  -o-transition: all .35s;\n  transition: all .35s;\n}\n.tab input[type=checkbox] + label::after {\n  content: \"+\";\n}\n.tab input[type=radio] + label::after {\n  content: \"ᦼ\";\n}\n.tab input[type=checkbox]:checked + label::after {\n  transform: rotate(45deg);\n}\n.tab input[type=radio]:checked + label::after {\n  transform: rotateX(180deg);\n}" ]


viewSection : (String -> msg) -> Section msg -> Html msg
viewSection msg (Section element) =
    div [ class "tab", style tabStyle ]
        [ input [ type_ "checkbox", name "tabs", id element.id, style inputStyle ] []
        , label [ for element.id, style labelStyle, onClick (msg element.id) ] [ element.header ]
        , div [ class "tab-content", style (contentStyle element) ]
            [ p [ style pStyle ] [ element.content ]
            ]
        ]


view : (String -> msg) -> List (Section msg) -> Html msg
view msg elements =
    div []
        [ styleNode
        , div [ style globalStyle ] (map (viewSection msg) elements)
        ]
