module Main exposing (..)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Jsonp
import Task exposing (Task)
import Json.Decode as Decode
import Types exposing (..)
import Ports exposing (..)


type alias Model =
    { position : Position
    , error : Error
    , temperature : Float
    }


init : ( Model, Cmd Msg )
init =
    ( Model (Position 0.0 0.0) (Error 0 "") 0.0, Cmd.none )


type Msg
    = NewPositionMsg Position
    | NewErrorMsg Error
    | NewTemperature (Result Http.Error Float)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ position NewPositionMsg
        , error NewErrorMsg
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPositionMsg position ->
            ( { model | position = position }, Task.attempt NewTemperature (getTemperature position) )

        NewErrorMsg error ->
            ( { model | error = error }, Cmd.none )

        NewTemperature (Ok temperature) ->
            ( { model | temperature = temperature }, Cmd.none )

        NewTemperature (Err _) ->
            model ! []


view : Model -> Html a
view model =
    div []
        [ input [ placeholder "City", onInput Change ] []
        , div [] [ text ("City: " ++ "Chicago") ]
        , div [] [ text ("Latitude: " ++ toString model.position.latitude) ]
        , div [] [ text ("Longitude: " ++ toString model.position.longitude) ]
        , div [] [ text ("temperature: " ++ toString model.temperature) ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


getCoordinates: City -> String
getCoordinates: City -> String = 
    "Texas"

getTemperature : Position -> Task Http.Error Float
getTemperature position =
    let
        ( lat, lng ) =
            ( toString position.latitude, toString position.longitude )
    in
        Jsonp.get decodeTemperature ("https://api.darksky.net/forecast/4d1324a8471b7b00488db714ea24f1f7/" ++ lat ++ "," ++ lng)


decodeTemperature : Decode.Decoder Float
decodeTemperature =
    Decode.at [ "currently", "temperature" ] Decode.float
