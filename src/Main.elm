module Main exposing (..)

import Adequacy
import BoolImpl exposing (..)
import Browser
import Browser.Navigation as Nav
import Html exposing (a, div, nav, text)
import Html.Attributes exposing (..)
import Representations
import Url
import Url.Parser exposing ((<?>), Parser, oneOf, parse, s)
import Url.Parser.Query as Query



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { url : Url.Url
    , key : Nav.Key
    , route : Maybe Route
    }


type Route
    = Adequacy String Adequacy.Model
    | Representation String Representations.Model


type PrimitiveRoute
    = PrimitiveAdequacy (Maybe String)
    | PrimitiveRepresentation (Maybe String)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { url = url
      , key = key
      , route = getRoute url
      }
    , Cmd.none
    )


getRoute : Url.Url -> Maybe Route
getRoute url =
    case parse routeParser url of
        Just (PrimitiveRepresentation Nothing) ->
            Just (Representation "" (Representations.initModel ""))

        Just (PrimitiveRepresentation (Just a)) ->
            Just (Representation a (Representations.initModel a))

        Just (PrimitiveAdequacy Nothing) ->
            Just (Adequacy "" (Adequacy.initModel ""))

        Just (PrimitiveAdequacy (Just a)) ->
            Just (Adequacy a (Adequacy.initModel a))

        _ ->
            Nothing



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AdequacyMsg Adequacy.Msg
    | RepresentationMsg Representations.Msg


resultOk : Result a b -> Bool
resultOk result =
    case result of
        Ok _ ->
            Basics.True

        Err _ ->
            Basics.False


routeParser : Parser (PrimitiveRoute -> a) a
routeParser =
    oneOf
        [ Url.Parser.map PrimitiveAdequacy (s "adequacy" <?> Query.string "q")
        , Url.Parser.map PrimitiveRepresentation (s "representation" <?> Query.string "q")
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.route ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( { model | route = getRoute url }
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            ( { model | url = url }
            , Cmd.none
            )

        ( AdequacyMsg aMsg, Just (Adequacy _ aModel) ) ->
            ( { model | route = Just (Adequacy "" (Tuple.first (Adequacy.update aMsg aModel))) }, Cmd.map (\m -> AdequacyMsg m) (Tuple.second (Adequacy.update aMsg aModel)) )

        ( RepresentationMsg rMsg, Just (Representation _ rModel) ) ->
            ( { model | route = Just (Representation "" (Tuple.first (Representations.update rMsg rModel))) }, Cmd.map (\m -> RepresentationMsg m) (Tuple.second (Representations.update rMsg rModel)) )

        ( _, _ ) ->
            -- Ignore messages that come from a non active view
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "BoolTool Reloaded"
    , body =
        [ nav [ class "navbar" ]
            [ div [ class "navbar-menu is-active" ]
                [ div [ class "navbar-start" ]
                    [ a [ class "navbar-item", href "/representation" ]
                        [ text "Representations"
                        ]
                    , a [ class "navbar-item", href "/adequacy" ]
                        [ text "Adequacy"
                        ]
                    ]
                ]
            ]
        , case model.route of
            Just (Adequacy _ aModel) ->
                Html.map (\a -> AdequacyMsg a) (Adequacy.view aModel)

            Just (Representation _ rModel) ->
                Html.map (\r -> RepresentationMsg r) (Representations.view rModel)

            Nothing ->
                text "404"
        ]
    }
