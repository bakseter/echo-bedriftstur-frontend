module Session exposing (Session)

import Browser.Navigation
import Cred exposing (Cred)


type alias Session =
    { navKey : Browser.Navigation.Key
    , cred : Maybe Cred
    }
