module LocalStorage exposing (LocalStorageData, LocalStorageToken, encodeRetrieval, encodeSave)

import Json.Encode as Encode


type LocalStorageData
    = LocalStorageData String String


type LocalStorageToken
    = LocalStorageToken String Bool


encodeSave : LocalStorageData -> Encode.Value
encodeSave (LocalStorageData key val) =
    Encode.object
        [ ( key, Encode.string val ) ]


encodeRetrieval : LocalStorageToken -> Encode.Value
encodeRetrieval (LocalStorageToken key del) =
    Encode.object
        [ ( "key", Encode.string key )
        , ( "del", Encode.bool del )
        ]
