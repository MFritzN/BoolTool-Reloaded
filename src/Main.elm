module Main exposing (..)

import Adequacy
import BoolImpl exposing (..)
import Browser
import Browser.Navigation as Nav
import Html exposing (a, div, h3, nav, text)
import Html.Attributes exposing (..)
import Representations
import Url
import Url.Parser exposing ((</>), Parser, fragment, oneOf, parse, s, top)



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
    , route : Route
    }


type Route
    = Adequacy String Adequacy.Model
    | Representation String Representations.Model
    | NotFound Nav.Key


type PrimitiveRoute
    = PrimitiveAdequacy (Maybe String)
    | PrimitiveRepresentation (Maybe String)
    | PrimitiveHome


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        routeTuple =
            getRoute url key
    in
    ( { url = url
      , route = Tuple.first routeTuple
      }
    , Tuple.second routeTuple
    )


getRoute : Url.Url -> Nav.Key -> ( Route, Cmd Msg )
getRoute url key =
    case parse routeParser url of
        Just (PrimitiveRepresentation Nothing) ->
            ( Representation "" (Representations.initModel "" key url), Cmd.none )

        Just (PrimitiveRepresentation (Just a)) ->
            ( Representation a (Representations.initModel a key url), Cmd.none )

        Just (PrimitiveAdequacy Nothing) ->
            ( Adequacy "" (Adequacy.initModel "" key url), Cmd.none )

        Just (PrimitiveAdequacy (Just a)) ->
            ( Adequacy a (Adequacy.initModel a key url), Cmd.none )

        Just PrimitiveHome ->
            let
                newUrl =
                    { url | path = "/representations" }
            in
            ( Representation "" (Representations.initModel "" key newUrl), Nav.replaceUrl key (Url.toString newUrl) )

        _ ->
            ( NotFound key, Cmd.none )



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
        [ Url.Parser.map PrimitiveHome top
        , Url.Parser.map PrimitiveAdequacy (s "adequacy" </> fragment identity)
        , Url.Parser.map PrimitiveRepresentation (s "representations" </> fragment identity)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        key =
            case model.route of
                Adequacy _ aModel ->
                    aModel.key

                Representation _ rModel ->
                    rModel.key

                NotFound k ->
                    k
    in
    case ( msg, model.route ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( { model | route = Tuple.first (getRoute url key) }
                    , Nav.pushUrl key (Url.toString url)
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            ( { model | url = url }
            , Cmd.none
            )

        ( AdequacyMsg aMsg, Adequacy _ aModel ) ->
            ( { model | route = Adequacy "" (Tuple.first (Adequacy.update aMsg aModel)) }, Cmd.map (\m -> AdequacyMsg m) (Tuple.second (Adequacy.update aMsg aModel)) )

        ( RepresentationMsg rMsg, Representation _ rModel ) ->
            ( { model | route = Representation "" (Tuple.first (Representations.update rMsg rModel)) }, Cmd.map (\m -> RepresentationMsg m) (Tuple.second (Representations.update rMsg rModel)) )

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
            [ div [ class "navbar-brand" ]
                [ a [ class "navbar-item", href "/" ]
                    [ h3 [] [ text "BoolTool Reloaded" ]
                    ]
                ]
            , div [ class "navbar-menu is-active" ]
                [ div [ class "navbar-start" ]
                    [ a [ class "navbar-item", href "/representations" ]
                        [ text "Representations"
                        ]
                    , a [ class "navbar-item", href "/adequacy" ]
                        [ text "Adequacy"
                        ]
                    ]
                ]
            ]
        , case model.route of
            Adequacy _ aModel ->
                Html.map (\a -> AdequacyMsg a) (Adequacy.view aModel)

            Representation _ rModel ->
                Html.map (\r -> RepresentationMsg r) (Representations.view rModel)

            NotFound _ ->
                text "404"
        ]
    }
