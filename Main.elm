module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (href, class, style)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Color as Color
import Material.Dialog as Dialog
import Material.Options as Options exposing (cs, css)
import Material.Scheme
import Material.Textfield as Textfield
import Random
import Time exposing (Time)


--import Time.Date as Date exposing (Date, date)
-- MODEL


type alias Cycle =
    { index : Int

    --, start : Date
    --, end : Date
    }


type DialogKind
    = DialogAddCycle


type alias UIModel =
    { dialogKind : Maybe DialogKind
    }



{- cycles are sorted by index, descending, so that the greatest index
   is first.
-}


type alias Model =
    { cycles : List Cycle
    , mdl : Material.Model
    , ui : UIModel
    }


initialModel : Model
initialModel =
    { cycles = []
    , mdl = Material.model
    , ui =
        { dialogKind = Nothing
        }
    }


nextIndex : Model -> Int
nextIndex model =
    let
        cycles =
            model.cycles
    in
        case cycles of
            [] ->
                1

            c :: _ ->
                c.index + 1



-- UPDATE


type Msg
    = AddCycle
    | CreateCycle
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddCycle ->
            let
                ui_ =
                    model.ui
            in
                ( { model | ui = { ui_ | dialogKind = Just DialogAddCycle } }, Cmd.none )

        CreateCycle ->
            let
                newCycle =
                    { index = nextIndex model }

                newCycles =
                    newCycle :: model.cycles
            in
                ( { model | cycles = newCycles }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.node "link"
            [ Html.Attributes.rel "stylesheet"
            , Html.Attributes.href "https://code.getmdl.io/1.3.0/material.teal-red.min.css"
            ]
            []
        , Button.render Mdl
            [ 0 ]
            model.mdl
            [ Button.raised
            , Options.onClick AddCycle
            , Dialog.openOn "click"
            ]
            [ text "Add cycle" ]
        , cycleCards model.cycles
        , dialog model
        ]


cycleCards : List Cycle -> Html Msg
cycleCards cycles =
    div [] (List.map cycleCard cycles)


cycleCard : Cycle -> Html Msg
cycleCard cycle =
    Card.view
        [ css "width" "128px"
        , Color.background (Color.color Color.Pink Color.S500)
        ]
        [ Card.title [] [ Card.head [ Color.text Color.white ] [ text <| toString cycle.index ] ]
        ]


dialog : Model -> Html Msg
dialog model =
    let
        dialogKind =
            model.ui.dialogKind
    in
        case dialogKind of
            Nothing ->
                dialogEmpty

            Just DialogAddCycle ->
                dialogAddCycle model


dialogEmpty : Html Msg
dialogEmpty =
    Dialog.view [] []


dialogAddCycle : Model -> Html Msg
dialogAddCycle model =
    Dialog.view
        []
        [ Dialog.title [] [ text "New Cycle" ]
        , Dialog.content []
            [ Textfield.render Mdl
                [ 1 ]
                model.mdl
                [ Textfield.label "Name" ]
                []
            ]
        , Dialog.actions []
            [ Button.render Mdl
                [ 0 ]
                model.mdl
                [ Dialog.closeOn "click"
                , Options.onClick CreateCycle
                ]
                [ text "Create" ]
            , Button.render Mdl
                [ 1 ]
                model.mdl
                [ Dialog.closeOn "click" ]
                [ text "Cancel" ]
            ]
        ]



-- MAIN


main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = Material.subscriptions Mdl
        }
