module Countdown exposing (countdownFromTo)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, id)
import Time

countdownFromTo timeNow timeThen =
    let time = timeThen - timeNow
        clock = List.map (\x -> div
                    [ class "clock-item", id ("clock" ++ Tuple.second x) ]
                    [ text <| fixNum <| String.fromInt <| Tuple.first x ])
                    (calcDate time)
    in
        (
            [ div [ id "days" ] [ text "D" ]
            , div [ id "hours" ] [ text "H" ]
            , div [ id "minutes" ] [ text "M" ]
            , div [ id "seconds" ] [ text "S" ]
            ] ++ clock
            , time <= 0
        )

fixNum : String -> String
fixNum str =
    if String.length str == 1
    then
        "0" ++ str
    else
        str

calcDate : Int -> List (Int, String)
calcDate diff =
    let day = diff // (86400 * 1000)
        dayMod = modBy (86400 * 1000) diff
        hour = dayMod // (3600 * 1000)
        hourMod = modBy (3600 * 1000) dayMod
        min = hourMod // (60 * 1000)
        minMod = modBy (60 * 1000) hourMod
        sec = minMod // 1000
    in
        if day <= 0 && hour <= 0 && min <= 0 && sec <= 0 then
            [(0,"D"),(0,"H"),(0,"M"),(0,"S")]
        else
            [(day,"D"), (hour,"H"), (min,"M"), (sec,"S")]
