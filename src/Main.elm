module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, h1, h2, li, p, span, text, ul)
import Html.Attributes exposing (class)
import Http
import Json.Decode exposing (Decoder, field, int, list, map, map2, map4, string)
import Maybe exposing (withDefault)
import Random exposing (Generator, generate)
import Random.List exposing (choose)
import Tuple exposing (first)



-- MAIN


main : Program Env Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type RequestStatus a
    = Failure
    | Initial
    | Loading
    | Success a


type alias Model =
    { regions : RequestStatus (List Region)
    , selectedRegion : Maybe Region
    , mammals : RequestStatus (List Species)
    , criticallyEndangered : RequestStatus (List Species)
    , crMeasures : CrMeasures
    , env : Env
    }


type alias Region =
    { name : String, identifier : String }


type alias Species =
    { scientificName : String, taxonid : Int, className : String, category : String }


type alias Measures =
    String


type alias CrMeasures =
    Dict Int (Result String String)


type alias Env =
    { apiUrl : String, token : String, viewLimit : String }


init : Env -> ( Model, Cmd Msg )
init env =
    ( { env = env
      , regions = Loading
      , selectedRegion = Nothing
      , criticallyEndangered = Initial
      , mammals = Initial
      , crMeasures = Dict.empty
      }
    , getRegions env
    )



-- UPDATE


type Msg
    = GotRegions (Result Http.Error (List Region))
    | RandomRegion (Maybe Region)
    | GotSpecies (Result Http.Error (List Species))
    | GotMeasures Int (Result Http.Error Measures)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRegions result ->
            case result of
                Ok regions ->
                    ( { model | regions = Success regions }
                    , generate RandomRegion (randomRegion regions)
                    )

                Err _ ->
                    ( { model | regions = Failure }, Cmd.none )

        RandomRegion result ->
            case result of
                Just region ->
                    ( { model
                        | selectedRegion = result
                        , mammals = Loading
                        , criticallyEndangered = Loading
                      }
                    , getSpecies model.env region.identifier
                    )

                Nothing ->
                    ( { model | selectedRegion = result }, Cmd.none )

        GotSpecies result ->
            case result of
                Ok species ->
                    let
                        criticallyEndangered =
                            limitResults model.env (filterCR species)
                    in
                    ( { model
                        | mammals = Success (limitResults model.env (filterMammals species))
                        , criticallyEndangered = Success criticallyEndangered
                      }
                    , Cmd.batch (List.map (\s -> getMeasures model.env s.taxonid) criticallyEndangered)
                    )

                Err _ ->
                    ( { model
                        | mammals = Failure
                        , criticallyEndangered = Failure
                      }
                    , Cmd.none
                    )

        GotMeasures id result ->
            case result of
                Ok measures ->
                    ( { model | crMeasures = Dict.insert id (Ok measures) model.crMeasures }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | crMeasures = Dict.insert id (Err "Something happened.") model.crMeasures }
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "space-y-12 xl:grid grid-cols-2" ]
        [ viewRegion model.selectedRegion
        , viewMammals model.mammals
        , viewCr model.criticallyEndangered model.crMeasures
        ]


viewRegion : Maybe Region -> Html Msg
viewRegion selectedRegion =
    h1 [ class "text-5xl text-center font-bold col-span-2" ]
        [ case selectedRegion of
            Nothing ->
                text "Choosing random region..."

            Just region ->
                text region.name
        ]


viewMammals : RequestStatus (List Species) -> Html Msg
viewMammals ms =
    div [ class "mx-auto" ]
        [ h2 [ class "font-bold text-2xl mb-2" ] [ text "Mammals in the region:" ]
        , div []
            [ renderBasedOnReqStateWithDefaults ms
                (\mammals ->
                    case mammals of
                        [] ->
                            text "No mammals were found in the selected region."

                        _ ->
                            ul [] (List.map viewMammal mammals)
                )
            ]
        ]


viewMammal : Species -> Html Msg
viewMammal s =
    li [] [ text s.scientificName ]


viewCr : RequestStatus (List Species) -> CrMeasures -> Html Msg
viewCr crSpecies crMeasures =
    div [ class "mx-auto" ]
        [ h2 [ class "font-bold text-2xl mb-2" ] [ text "Critically endangered species in the selected region:" ]
        , div []
            [ renderBasedOnReqStateWithDefaults crSpecies
                (\cr ->
                    case cr of
                        [] ->
                            text "No critically endangered species were found in the region."

                        _ ->
                            ul [ class "space-y-2" ] (List.map (viewCrItem crMeasures) cr)
                )
            ]
        ]


viewCrItem : CrMeasures -> Species -> Html Msg
viewCrItem crMeasures cr =
    li []
        [ p [ class "font-semibold" ] [ text cr.scientificName ]
        , p []
            [ case Dict.get cr.taxonid crMeasures of
                Just (Ok "") ->
                    text "No conservation measures were found for this species."

                Just (Ok ms) ->
                    span [ class "text-sm" ] [ text ms ]

                Just (Err error) ->
                    span [ class "text-red-700" ] [ text error ]

                Nothing ->
                    text "Loading..."
            ]
        ]



-- HTTP


getRegions : Env -> Cmd Msg
getRegions env =
    Http.get
        { url = env.apiUrl ++ "region/list?token=" ++ env.token
        , expect = Http.expectJson GotRegions regionsDecoder
        }


getSpecies : Env -> String -> Cmd Msg
getSpecies env regionId =
    Http.get
        { url = env.apiUrl ++ "species/region/" ++ regionId ++ "/page/0?token=" ++ env.token
        , expect = Http.expectJson GotSpecies speciesListDecoder
        }


getMeasures : Env -> Int -> Cmd Msg
getMeasures env id =
    Http.get
        { url = env.apiUrl ++ "measures/species/id/" ++ String.fromInt id ++ "?token=" ++ env.token
        , expect = Http.expectJson (GotMeasures id) measuresRecordDecoder
        }



-- Decoders


regionDecoder : Decoder Region
regionDecoder =
    map2 Region
        (field "name" string)
        (field "identifier" string)


regionsDecoder : Decoder (List Region)
regionsDecoder =
    field "results" (list regionDecoder)


speciesDecoder : Decoder Species
speciesDecoder =
    map4 Species
        (field "scientific_name" string)
        (field "taxonid" int)
        (field "class_name" string)
        (field "category" string)


speciesListDecoder : Decoder (List Species)
speciesListDecoder =
    field "result" (list speciesDecoder)


measuresRecordDecoder : Decoder Measures
measuresRecordDecoder =
    list (field "title" string) |> map (String.join ", ") |> field "result"


randomRegion : List Region -> Generator (Maybe Region)
randomRegion regions =
    Random.map first (choose regions)



-- HELPERS


filterMammals : List Species -> List Species
filterMammals =
    List.filter (\s -> s.className == "MAMMALIA")


filterCR : List Species -> List Species
filterCR =
    List.filter (\s -> s.category == "CR")


limitResults : Env -> List a -> List a
limitResults env xs =
    let
        limit =
            String.toInt env.viewLimit |> withDefault 10
    in
    List.take limit xs


renderBasedOnReqState : RequestStatus a -> Html Msg -> Html Msg -> Html Msg -> (a -> Html Msg) -> Html Msg
renderBasedOnReqState reqState failure initial loading success =
    case reqState of
        Failure ->
            failure

        Initial ->
            initial

        Loading ->
            loading

        Success data ->
            success data


renderBasedOnReqStateWithDefaults : RequestStatus a -> (a -> Html Msg) -> Html Msg
renderBasedOnReqStateWithDefaults reqState =
    renderBasedOnReqState reqState (text "Ooops, something went wrong.") (text "") (text "Loading")
