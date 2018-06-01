module Main exposing (..)

import Format
import Html exposing (Html, div, img, text)
import Matrix exposing (Matrix, fromList)


---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


testMatrix =
    Matrix.fromList
        [ [ 2 / 3, -2 / 3, 1 / 3 ], [ 1 / 3, 2 / 3, 2 / 3 ], [ 2 / 3, 1 / 3, -2 / 3 ] ]


view : Model -> Html Msg
view model =
    case testMatrix of
        Nothing ->
            div [] [ text ":(" ]

        Just matrix ->
            let
                transposed =
                    Matrix.transpose matrix
            in
            case Matrix.multiply matrix transposed of
                Nothing ->
                    div [] [ text ":(" ]

                Just multiplied ->
                    div []
                        [ summaryView
                        , div []
                            [ Html.h3 [] [ text "Original" ]
                            , matrixView matrix
                            ]
                        , div []
                            [ Html.h3 [] [ text "Transposed" ]
                            , matrixView transposed
                            ]
                        , div []
                            [ Html.h3 [] [ text "Multiplied by Transpose" ]
                            , matrixView multiplied
                            ]
                        ]


summaryView : Html Msg
summaryView =
    div []
        [ text """A n×n matrix A is an orthogonal matrix if
            A multiplied by its transpose is equal to the identity matrix"""
        ]


matrixView : Matrix Float -> Html Msg
matrixView matrix =
    let
        lists =
            Matrix.toList matrix
    in
    lists
        |> List.map matrixRow
        |> Html.table []


matrixRow : List Float -> Html Msg
matrixRow items =
    items
        |> List.map matrixItem
        |> Html.tr []


matrixItem : Float -> Html Msg
matrixItem item =
    Html.td [] [ item |> Format.floatToString 2 |> text ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
