module MN_test exposing (main)

{- This is a demo of the BarChart package -}

import Browser
import Html exposing (Html)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import SimpleGraph exposing (Option(..), barChart, lineChart, scatterPlot)


--
-- DATA
--


lineData : List ( Float, Float )
lineData =
    [ ( -20, 0 ), ( -10, -10 ), ( 0, 0 ), ( 10, 10 ), ( 20, 0 ), ( 30, 15 ), ( 40, 0 ), ( 50, -30 ), ( 60, 0 ) ]

lineData2 : List ( Float, Float )
lineData2 =
    [ ( 0, 0 ), ( 10, 10 ), ( 20, 0 ), ( 30, 15 ), ( 40, 0 ) ]

lineGraphAttributes =
    { graphHeight = 100
    , graphWidth = 400
    , xTickmarks  = 6
    , yTickmarks = 4 
    , scale = ( 1.0, 1.0 )
    , options = [ Color "#067DB4", StrokeWidth 3.0 ]
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


{-| This paragraph is where all the action is
-}
mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ centerX, centerY, spacing 60, padding 40, Background.color (rgb255 240 240 240) ]
            [ title "SimpleGraph Demo"
            , row [] [ lineChart lineGraphAttributes lineData |> Element.html ]
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
