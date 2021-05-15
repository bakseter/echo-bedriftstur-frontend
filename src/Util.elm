module Util exposing (edges, getPng, toHexColor)

import Element
import Hex


getPng : String -> String
getPng str =
    "/assets/" ++ str ++ ".png"


toHexColor : Element.Color -> String
toHexColor color =
    let
        { red, green, blue } =
            Element.toRgb color

        san c =
            let
                nc =
                    Hex.toString (round (c * 255))
            in
            if String.length nc /= 2 then
                "0" ++ nc

            else
                nc
    in
    "#" ++ san red ++ san green ++ san blue


edges : { top : Int, right : Int, bottom : Int, left : Int }
edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }
