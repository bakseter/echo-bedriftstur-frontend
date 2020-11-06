module Session exposing (Session)

import Api
import Browser.Navigation


type alias Session =
    { navKey : Browser.Navigation.Key
    , apiKey : Api.Key
    }
