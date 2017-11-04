module Test exposing (..)

import Elmi.Interface
import Elmi.Parser as Parser
import Html exposing (Html)
import Http
import String.UTF8


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    List Int


init : ( Model, Cmd Msg )
init =
    [] ! [ fetchFoo ]


fetchFoo : Cmd Msg
fetchFoo =
    Http.getString "foo.elmi" |> Http.send Loaded


type Msg
    = Loaded (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Loaded (Ok res) ->
            String.UTF8.toBytes res ! []

        _ ->
            [] ! []


view : Model -> Html Msg
view model =
    Html.text <| toString <| Parser.run Elmi.Interface.parseInterface model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
