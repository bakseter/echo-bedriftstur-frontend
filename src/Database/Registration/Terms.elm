module Database.Registration.Terms exposing (Terms(..), decoder, toBool)

import Json.Decode as Decode


type Terms
    = Terms Bool Bool Bool


decoder : Decode.Decoder Terms
decoder =
    Decode.map fromAbsolute <| Decode.field "formTerms" Decode.bool


fromAbsolute : Bool -> Terms
fromAbsolute bool =
    if bool then
        Terms True True True

    else
        Terms False False False


toBool : Terms -> Bool
toBool (Terms b1 b2 b3) =
    b1 && b2 && b3



{-
   fst : Terms -> Bool
   fst (Terms b _ _) =
       b


   snd : Terms -> Bool
   snd (Terms _ b _) =
       b


   thd : Terms -> Bool
   thd (Terms _ _ b) =
       b
-}
