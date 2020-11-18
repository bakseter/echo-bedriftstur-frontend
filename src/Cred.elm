module Cred exposing (Cred, IdToken(..), RefreshToken(..), credDecoder, isSignedIn)

import Json.Decode as Decode
import Json.Encode as Encode
import Uid exposing (Uid(..))


type IdToken
    = IdToken String Int


type RefreshToken
    = RefreshToken String


type alias Cred =
    { idToken : IdToken
    , refreshToken : RefreshToken
    , uid : Uid
    }


isSignedIn : Maybe Cred -> Bool
isSignedIn cred =
    case cred of
        Just _ ->
            True

        Nothing ->
            False


credDecoder : Decode.Decoder Cred
credDecoder =
    Decode.oneOf
        [ Decode.map3 Cred
            (Decode.map2 IdToken
                (Decode.field "idToken" Decode.string)
                (Decode.map (Maybe.withDefault 0 << String.toInt) <| Decode.field "expiresIn" Decode.string)
            )
            (Decode.map RefreshToken <| Decode.field "refreshToken" Decode.string)
            (Decode.map Uid <| Decode.field "localId" Decode.string)
        , Decode.map3 Cred
            (Decode.map2 IdToken
                (Decode.field "id_token" Decode.string)
                (Decode.map (Maybe.withDefault 0 << String.toInt) <| Decode.field "expires_in" Decode.string)
            )
            (Decode.map RefreshToken <| Decode.field "refresh_token" Decode.string)
            (Decode.map Uid <| Decode.field "user_id" Decode.string)
        ]
