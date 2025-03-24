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
    in
        List.reverse ( [ max ] :: List.reverse ( [ min ] :: raw_data ) )

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


{-| This function is like lineChartWithDataWindow, but handling 
lists of datasets with corresponding lists of attributes. 
Except for color and linewidth, only the values of the first 
attribute record are evaluated here. However, 
lineChartAsSVGWithDataWindow is dependent on each attribute set 
again. Therefore, the attributes must be identical, except for 
color and linewidth. It also means that the axes and ticks are 
plotted for each dataset. The DataWindow is calculated from 
the minimal and maximal values of all datasets.
-}
lineChartsWithDataWindow : List ( GraphAttributes ) -> List ( List ( Float, Float ) ) -> Html msg
lineChartsWithDataWindow attr data =
    let
        h = Maybe.withDefault 100 ( List.head attr |> Maybe.map .graphHeight )
        w = Maybe.withDefault 400 ( List.head attr |> Maybe.map .graphWidth )
        xs = List.concatMap ( List.map fst ) data
        ys = List.concatMap ( List.map snd ) data
        x_min = Maybe.withDefault 0 ( List.minimum xs )
        x_max = Maybe.withDefault 0 ( List.maximum xs )
        y_min = Maybe.withDefault 0 ( List.minimum ys )
        y_max = Maybe.withDefault 0 ( List.maximum ys )
        x_margin = 0
        y_margin = 0
        dw = getDataWindow [ ( x_min - x_margin, y_min - y_margin ), ( x_max + x_margin, y_max + y_margin ) ]
        
        fst : ( a, b ) -> a
        fst ( x, _ ) = x
        snd : ( a, b ) -> b
        snd ( _, y ) = y
    in 
        svg
            [ SA.transform "scale(1,-1)"
            , SA.height <| String.fromFloat ( h + 60 )
            , SA.width <| String.fromFloat ( w + 70 )
            , SA.viewBox <| "-60 -50 " ++ String.fromFloat ( w + 70 ) ++ " " ++ String.fromFloat ( h + 60 )
            ]
            ( List.map2 ( \a d  -> lineChartAsSVGWithDataWindow dw a d ) attr data )


{-| This paragraph is where all the action is
-}
mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ centerX, centerY, spacing 60, padding 40, Background.color (rgb255 240 240 240) ]
            --[ row [] [ lineChartsWithDataWindow lgaList lineDataList |> Element.html ] ]
            [ row [] [ lineChartsWithDataWindow lgas e_data |> Element.html ] ]
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
