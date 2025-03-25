module Main exposing (main)

{- This is a demo of the BarChart package -}

import Browser
import Html exposing (Html)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import SimpleGraph exposing (..)

import Svg exposing (Svg, g, line, rect, circle, svg, text, text_)
import Svg.Attributes as SA


--
-- DATA
--
 
-- example output for x-ray absorption spectroscopy
energies = [ [0.25, 3.0], [5.0, 45.0], [5.0, 50.0], [4.0, 44.0], [4.0, 44.0] ]
ids = [ "P04", "P06", "P23", "P64", "P65" ]

prep_x_data : List ( List Float ) -> List ( List Float )
prep_x_data raw_data =
    -- input: raw_data, e.g. energies or temperatures as received from sparql query
    -- output: print-ready data_set
    -- add dummy entries at beginning and end to add y margin (additional data lines) and to adjust x-range
    let
        min = raw_data -- Float, next number divisible by 5 below min
            |> List.concat
            |> List.minimum
            |> Maybe.withDefault 0.0
            |> ( \v -> toFloat ( ( Basics.floor ( v / 5 ) ) * 5 ) )
        max = raw_data -- Float , next number divisible by 5 above max
            |> List.concat
            |> List.maximum
            |> Maybe.withDefault 100.0
            |> ( \v -> toFloat ( ( Basics.floor ( v / 5 ) + 1 ) * 5 ) )
        d1list = List.reverse ( [ max ] :: List.reverse ( [ min ] :: raw_data ) )
    in
        List.map2 (
    \e_min_max idx -> List.map (
        \e -> ( e, toFloat ( idx-1 ) )
        ) e_min_max
    ) d1list ( List.range 1 ( List.length d1list ) )

prep_y_data : List String -> List String
prep_y_data id_list =
    -- add dummy entries at beginning and end to adapt to the changes from prep_x_data
    List.reverse ( "" :: List.reverse ( "" :: id_list ) )
        
es = prep_x_data energies 
bl_ids = prep_y_data ids
lgas = List.map ( \e -> { graphHeight = 200
                        , graphWidth = 800
                        , options = [ Color "blue"
                                    , YTickmarks ( List.length es )
                                    , XTickmarks (  1 + ( es
                                        |> List.concat
                                        |> List.maximum
                                        |> Maybe.withDefault 9.0
                                        |> ( \v -> v / 5.0 )
                                        |> round
                                        ) )
                                    , Scale 1.0 1.0 
                                    , YLabels bl_ids
                                    , LineWidth 10.0
                                    ]
                        } ) es

e_data = List.map2 (
    \e_min_max idx -> List.map (
        \e -> ( e, toFloat ( idx-1 ) )
        ) e_min_max
    ) es ( List.range 1 ( List.length es ) )
    


--
-- APP
--


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    {}


type Msg
    = NoOp


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( {}, Cmd.none )


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layout [] (mainColumn model)


{-| This paragraph is where all the action is
-}
mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ centerX, centerY, spacing 60, padding 40, Background.color (rgb255 240 240 240) ]
            --[ row [] [ lineChartsWithDataWindow lgaList lineDataList |> Element.html ] ]
            [ row [] [ lineChartsWithDataWindow lgas es |> Element.html ] ]
        ]


title : String -> Element msg
title str =
    row [ centerX, Font.bold ] [ Element.text str ]


mainColumnStyle =
    [ height fill
    , width fill
    , Background.color (rgb255 80 80 80)
    , paddingXY 20 20
    ]
