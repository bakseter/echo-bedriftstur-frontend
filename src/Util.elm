module Util exposing (edges, getPng, sequenceM, sequenceR, toHexColor)

import Element
import Hex


getPng : String -> String
getPng str =
    "/assets/" ++ str ++ ".png"


toHexColor : Element.Color -> String
toHexColor color =
    let
        { red, green, blue, alpha } =
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


sequenceM : List (Maybe a) -> Maybe (List a)
sequenceM list =
    case list of
        x :: xs ->
            Maybe.andThen (\a -> Maybe.map ((::) a) <| sequenceM xs) x

        [] ->
            Just []


sequenceR : List (Result a b) -> Result a (List b)
sequenceR list =
    case list of
        x :: xs ->
            Result.andThen (\a -> Result.map ((::) a) <| sequenceR xs) x

        [] ->
            Ok []
