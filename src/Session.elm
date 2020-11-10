module Session exposing (Session)

import Api
import Browser.Navigation
import Cred exposing (Cred)


type alias Session =
    { navKey : Browser.Navigation.Key
    , apiKey : Api.Key
    , cred : Maybe Cred
    }
