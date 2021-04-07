module Database.UserInfo.Degree exposing (Degree(..), decoder, fromString, toString)

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


toString : Degree -> ( String, String )
toString degree =
    case degree of
        DTEK ->
            ( "DTEK", "Datateknologi" )

        DVIT ->
            ( "DVIT", "Datavitenskap" )

        DSIK ->
            ( "DSIK", "Datasikkerhet" )

        BINF ->
            ( "BINF", "Bioinformatikk" )

        IMØ ->
            ( "IMØ", "Informatikk-matematikk-økonomi" )

        IKT ->
            ( "IKT", "Informasjons- og kommunikasjonsteknologi" )

        KOGNI ->
            ( "KOGNI", "Kognitiv vitenskap med spesialisering i informatikk" )

        INF ->
            ( "INF", "Master i informatikk" )

        PROG ->
            ( "PROG", "Felles master i programutvikling" )

        POST ->
            ( "POST", "Postbachelor" )

        MISC ->
            ( "MISC", "Annet studieløp" )


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


decoder : Decode.Decoder Degree
decoder =
    -- TODO: is this field name correct ???
    Decode.field "userInfoDegree" Decode.string
        |> Decode.andThen
            (\str ->
                case fromString str of
                    Just degree ->
                        Decode.succeed degree

                    Nothing ->
                        Decode.fail "Degree.fromString returned Nothing."
            )
