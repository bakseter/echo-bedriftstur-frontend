module Password exposing (Password(..), isValid, toString)


type Password
    = Password String


toString : Password -> String
toString (Password str) =
    str


isValid : Password -> Bool
isValid (Password str) =
    String.toUpper str /= str && String.toLower str /= str && String.any Char.isDigit str && String.length str >= 9
