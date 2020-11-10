module Page.Info exposing (Model, init, route, title, toSession, updateSession, view)

import Element exposing (Element, centerX, padding, paddingEach, paragraph, spacing, text, textColumn)
import Session exposing (Session)


type Model
    = Model Session


init : Session -> Model
init =
    Model


view : Element msg
view =
    textColumn [ centerX, spacing 50, paddingEach { edges | bottom = 100 } ]
        [ paragraph [] [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Velit laoreet id donec ultrices tincidunt arcu non. Nibh cras pulvinar mattis nunc sed. Eu volutpat odio facilisis mauris sit. At urna condimentum mattis pellentesque id nibh. Elementum eu facilisis sed odio morbi quis commodo. Iaculis nunc sed augue lacus viverra vitae congue. Id ornare arcu odio ut sem. Lectus sit amet est placerat in egestas. Enim diam vulputate ut pharetra sit amet aliquam. Ante metus dictum at tempor commodo. Risus quis varius quam quisque id diam vel. Quis varius quam quisque id diam. Nulla pellentesque dignissim enim sit amet venenatis. Habitasse platea dictumst quisque sagittis. In hac habitasse platea dictumst. Cras sed felis eget velit aliquet. Lorem sed risus ultricies tristique nulla. Blandit cursus risus at ultrices mi tempus imperdiet nulla." ]
        , paragraph [] [ text "Leo duis ut diam quam nulla porttitor. Egestas sed tempus urna et pharetra. Arcu odio ut sem nulla pharetra. Bibendum est ultricies integer quis auctor elit. Eu volutpat odio facilisis mauris sit. Integer feugiat scelerisque varius morbi. Egestas erat imperdiet sed euismod nisi porta. Adipiscing elit pellentesque habitant morbi tristique senectus et. Senectus et netus et malesuada. Id cursus metus aliquam eleifend mi in nulla posuere sollicitudin. Magna etiam tempor orci eu lobortis elementum nibh tellus. Lacus vestibulum sed arcu non. Dictum fusce ut placerat orci nulla. Amet tellus cras adipiscing enim eu turpis egestas pretium aenean. Tristique sollicitudin nibh sit amet commodo nulla. Sit amet est placerat in egestas erat imperdiet. Interdum varius sit amet mattis vulputate enim. Feugiat nisl pretium fusce id velit ut tortor pretium. Urna nunc id cursus metus aliquam eleifend mi." ]
        , paragraph [] [ text "Quis hendrerit dolor magna eget est lorem ipsum dolor sit. Pretium viverra suspendisse potenti nullam. Dis parturient montes nascetur ridiculus. Est lorem ipsum dolor sit amet. Orci porta non pulvinar neque laoreet suspendisse interdum consectetur. Diam ut venenatis tellus in metus vulputate eu. Sed turpis tincidunt id aliquet risus feugiat in. Venenatis tellus in metus vulputate. Quisque egestas diam in arcu. Lorem donec massa sapien faucibus et molestie ac feugiat sed. Egestas maecenas pharetra convallis posuere morbi leo urna. Varius duis at consectetur lorem donec." ]
        , paragraph [] [ text "Augue neque gravida in fermentum et sollicitudin ac orci phasellus. Aenean et tortor at risus. Rhoncus dolor purus non enim praesent elementum facilisis leo vel. Sem viverra aliquet eget sit amet tellus cras. Sit amet dictum sit amet. At tempor commodo ullamcorper a lacus vestibulum sed arcu. Id diam vel quam elementum. Et ligula ullamcorper malesuada proin libero nunc. Lectus proin nibh nisl condimentum. Arcu risus quis varius quam quisque id. Vel pretium lectus quam id. Suspendisse sed nisi lacus sed viverra tellus in. Rhoncus mattis rhoncus urna neque viverra. Sed arcu non odio euismod." ]
        , paragraph [] [ text "Nec nam aliquam sem et tortor consequat id porta nibh. Enim blandit volutpat maecenas volutpat. Urna neque viverra justo nec. Convallis posuere morbi leo urna molestie at elementum eu facilisis. Id ornare arcu odio ut sem nulla pharetra diam sit. Dui faucibus in ornare quam viverra orci sagittis. Id donec ultrices tincidunt arcu non sodales neque. Bibendum enim facilisis gravida neque convallis a. Risus commodo viverra maecenas accumsan lacus vel facilisis volutpat est. Tincidunt arcu non sodales neque. Nec feugiat in fermentum posuere urna nec tincidunt praesent. Sed augue lacus viverra vitae congue eu consequat ac felis." ]
        ]


edges : { top : Int, right : Int, bottom : Int, left : Int }
edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


route : String
route =
    "info"


title : String
title =
    "Informasjon"


toSession : Model -> Session
toSession (Model session) =
    session


updateSession : Model -> Session -> Model
updateSession _ =
    Model
