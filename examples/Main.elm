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


lineData1 : List ( Float, Float )
lineData1 =
    [ ( 3,1 ), ( 5,1 ) ]

scatterData : List (Float, Float)
scatterData = [(0,0), (10, 0), (0,10), (10, 10), (5,5), (5, 7)]
-- scatterData = [(0,0), (10,10)]

lineData2 : List ( Float, Float )
lineData2 =
    [ ( 5,2 ), ( 10,2 ) ]

lineData3 : List ( Float, Float )
lineData3 =
    [ ( 4,3 ), ( 7,3 ) ]

lineData4 : List ( Float, Float )
lineData4 =
    [ ( 6,4 ), ( 9,4 ) ]

lineDataList = [ lineData1, lineData2, lineData3, lineData4 ]


barData : List Float
barData =
    [ 5, 10, 20, 30, 20, 20, 5 ]


lineGraphAttributes1 =
    { graphHeight = 100
    , graphWidth = 400
    , options = [ Color "blue", YTickmarks 6, XTickmarks 10, Scale 1.0 1.0 ]
    }

lineGraphAttributes2 =
    { graphHeight = 100
    , graphWidth = 400
    , options = [ Color "red", YTickmarks 6, Scale 1.0 1.0 ]
    }

lineGraphAttributes3 =
    { graphHeight = 100
    , graphWidth = 400
    , options = [ Color "yellow", YTickmarks 6, Scale 1.0 1.0 ]
    }

lineGraphAttributes4 =
    { graphHeight = 100
    , graphWidth = 400
    , options = [ Color "green", YTickmarks 6, Scale 1.0 1.0 ]
    }

lgaList = [ lineGraphAttributes1, lineGraphAttributes2, lineGraphAttributes3, lineGraphAttributes4 ]

scatterPlotAttributes =
    { graphHeight = 200
    , graphWidth = 200
    , options = [ Color "blue", YTickmarks 4, Scale 1.0 1.0 ]
    }

barGraphAttributes =
    { graphHeight = 100
    , graphWidth = 400
    , options = [ Color "rgb(200,0,0)", DeltaX 15, YTickmarks 6, XTickmarks 2, Scale 1.0 1.0 ]
    }


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
attribute record are evaluated. The DataWindow is calculated from
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
        fact = 0.15
        x_margin = fact * ( x_max - x_min )
        y_margin = fact * ( y_max - y_min )
        dw = getDataWindow [ ( x_min - x_margin, y_min - y_margin ), ( x_max + x_margin, y_max + y_margin ) ]

        fst : ( a, b ) -> a
        fst ( x, _ ) = x
        snd : ( a, b ) -> b
        snd ( _, y ) = y
    in
        svg
            [ SA.transform "scale(1,-1)"
            , SA.height <| String.fromFloat ( h + 40 )
            , SA.width <| String.fromFloat ( w + 50 )
            , SA.viewBox <| "-40 -20 " ++ String.fromFloat ( w + 50 ) ++ " " ++ String.fromFloat ( h + 40 )
            ]
            ( List.map2 ( \a d  -> lineChartAsSVGWithDataWindow dw a d ) attr data )


{-| This paragraph is where all the action is
-}
mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ centerX, centerY, spacing 60, padding 40, Background.color (rgb255 240 240 240) ]
            [ title "SimpleGraph Demo"
            , row [] [ lineChartsWithDataWindow lgaList lineDataList |> Element.html ]
            , row [] [ barChart barGraphAttributes barData |> Element.html ]
            , row [] [ scatterPlot scatterPlotAttributes scatterData |> Element.html]
            ]
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
