
type Msg
    = PageFound Page
    | PageNotFound


type alias Model =
    { currentPage : Page
    , showNavBar : Bool
    , removeLineNavBtn : Bool
    }

init : (Model, Cmd Msg)
init =
   ({ currentPage = Hjem
    , showNavBar = False
    , removeLineNavBtn = False
    }, Cmd.none)

view : Model -> Html msg
view model =


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PageFound page ->
            ({ model | currentPage = page }, Cmd.none)
        PageNotFound ->
            ({ model | currentPage = NotFound }, Cmd.none)
