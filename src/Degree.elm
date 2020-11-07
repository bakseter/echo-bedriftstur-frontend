module Degree exposing (Degree(..), fromString, orNullDecoder, toString)

import Json.Decode as Decode



-- Type representing all the possible degrees a student can have


type Degree
    = DTEK
    | DSIK
    | DVIT
    | BINF
    | IMØ
    | IKT
    | KOGNI
    | INF
    | PROG
    | POST
    | MISC



-- List of valid degrees with their shorthand and long strings


toString : Bool -> Degree -> String
toString shorthand degree =
    case degree of
        DTEK ->
            if shorthand then
                "DTEK"

            else
                "Datateknologi"

        DVIT ->
            if shorthand then
                "DVIT"

            else
                "Datavitenskap"

        DSIK ->
            if shorthand then
                "DSIK"

            else
                "Datasikkerhet"

        BINF ->
            if shorthand then
                "BINF"

            else
                "Bioinformatikk"

        IMØ ->
            if shorthand then
                "IMØ"

            else
                "Informatikk-matematikk-økonomi"

        IKT ->
            if shorthand then
                "IKT"

            else
                "Informasjons- og kommunikasjonsteknologi"

        KOGNI ->
            if shorthand then
                "KOGNI"

            else
                "Kognitiv vitenskap med spesialisering i informatikk"

        INF ->
            if shorthand then
                "INF"

            else
                "Master i informatikk"

        PROG ->
            if shorthand then
                "PROG"

            else
                "Felles master i programutvikling"

        POST ->
            if shorthand then
                "POST"

            else
                "Postbachelor"

        MISC ->
            if shorthand then
                "MISC"

            else
                "Annet studieløp"


fromString : String -> Maybe Degree
fromString str =
    if str == "DTEK" || str == "Datateknologi" then
        Just DTEK

    else if str == "DSIK" || str == "Datasikkerhet" then
        Just DSIK

    else if str == "DVIT" || str == "Datavitenskap" then
        Just DVIT

    else if str == "BINF" || str == "Bioinformatikk" then
        Just BINF

    else if str == "IMØ" || str == "Informatikk-matematikk-økonomi" then
        Just IMØ

    else if str == "IKT" || str == "Informasjons- og kommunikasjonsteknologi" then
        Just IKT

    else if str == "KOGNI" || str == "Kognitiv vitenskap med spesialisering i informatikk" then
        Just KOGNI

    else if str == "INF" || str == "Master i informatikk" then
        Just INF

    else if str == "PROG" || str == "Felles master i programutvikling" then
        Just PROG

    else if str == "POST" || str == "Postbachelor" then
        Just POST

    else if str == "MISC" || str == "Annet studieløp" then
        Just MISC

    else
        Nothing


orNullDecoder : String -> Decode.Decoder Degree
orNullDecoder field =
    Decode.oneOf
        [ Decode.map (Maybe.withDefault DTEK << fromString) (Decode.at [ field ] Decode.string)
        , Decode.at [ field ] (Decode.null DTEK)
        , Decode.succeed DTEK
        ]
