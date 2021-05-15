module Theme exposing (..)

import Element exposing (Element, centerX, el, html, padding, rgb255, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlE


button : { onPress : Maybe msg, label : Element msg } -> Element msg
button btn =
    Input.button
        [ Background.color <|
            rgb255 210 210 210
        , padding 10
        , centerX
        , Border.rounded 9
        ]
        { onPress = btn.onPress
        , label = btn.label
        }


select :
    { onInput : String -> msg
    , value : String
    , options : List ( String, String )
    , addEmpty : Bool
    }
    -> Element msg
select props =
    let
        empty =
            if props.addEmpty then
                [ Html.option
                    [ HtmlA.value "" ]
                    [ Html.text "" ]
                ]

            else
                []
    in
    html <|
        Html.select
            [ HtmlE.onInput props.onInput
            , HtmlA.value props.value
            , HtmlA.style "margin-top" "2em"
            , HtmlA.style "margin-bottom" "2em"
            ]
        <|
            empty
                ++ List.map
                    (\( val, txt ) ->
                        Html.option
                            [ HtmlA.value val ]
                            [ Html.text txt ]
                    )
                    props.options


h1 : List (Element.Attribute msg) -> String -> Element msg
h1 attr str =
    el (Region.heading 1 :: Font.bold :: Font.size 46 :: attr) <|
        text str


h2 : List (Element.Attribute msg) -> String -> Element msg
h2 attr str =
    el (Region.heading 2 :: Font.bold :: Font.size 40 :: attr) <|
        text str


h3 : List (Element.Attribute msg) -> String -> Element msg
h3 attr str =
    el (Region.heading 3 :: Font.bold :: Font.size 32 :: attr) <|
        text str


h4 : List (Element.Attribute msg) -> String -> Element msg
h4 attr str =
    el (Region.heading 4 :: Font.bold :: Font.size 26 :: attr) <|
        text str


h5 : List (Element.Attribute msg) -> String -> Element msg
h5 attr str =
    el (Region.heading 5 :: Font.bold :: Font.size 20 :: attr) <|
        text str


yellow1 : Element.Color
yellow1 =
    rgb255 255 216 118


yellow2 : Element.Color
yellow2 =
    rgb255 252 191 1


blue1 : Element.Color
blue1 =
    rgb255 169 209 217


blue2 : Element.Color
blue2 =
    rgb255 5 149 173


foreground : Element.Color
foreground =
    rgb255 255 255 255


drawer : Element.Color
drawer =
    rgb255 200 200 200


background : Element.Color
background =
    rgb255 245 245 245
