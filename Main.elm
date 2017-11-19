module Main exposing (..)

import Array exposing (Array)
import Html exposing (Html, text)
import Html.Events exposing (..)
import Html.Attributes as A exposing (class, href)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Color as Color
import Material.Dialog as Dialog
import Material.Options as Options exposing (cs, css, div, id)
import Material.Scheme
import Material.Textfield as Textfield
import Material.Typography as Typography
import Random
import Task exposing (Task)
import Time exposing (Time)
import Material.Layout as Layout


--import Time.Date as Date exposing (Date, date)
-- MODEL


type alias Cycle =
    { index : Int

    --, start : Date
    --, end : Date
    }


type DialogKind
    = DialogNone


type alias UIModel =
    { dialogKind : DialogKind
    , selectedTab : Int
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
        { dialogKind = DialogNone
        , selectedTab = 0
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
    = CreateCycle
    | UISelectTab Int
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ui_ =
            model.ui
    in
        case msg of
            CreateCycle ->
                let
                    newCycle =
                        { index = nextIndex model }

                    newCycles =
                        newCycle :: model.cycles
                in
                    ( { model | cycles = newCycles }, Cmd.none )

            UISelectTab k ->
                ( { model | ui = { ui_ | selectedTab = k } }, Cmd.none )

            Mdl msg_ ->
                Material.update Mdl msg_ model



-- VIEW


tabs : List ( String, String, Model -> Html Msg )
tabs =
    [ ( "Cycles", "cycles", cyclesTab )
    , ( "Projects", "projects", projectsTab )
    , ( "People", "people", peopleTab )
    ]


tabTitles : List (Html a)
tabTitles =
    List.map (\( x, _, _ ) -> text x) tabs


tabViews : Array (Model -> Html Msg)
tabViews =
    List.map (\( _, _, x ) -> x) tabs
        |> Array.fromList


e404 : Model -> Html Msg
e404 _ =
    div
        []
        [ Options.styled Html.h1
            [ Options.cs "mdl-typography--display-4"
            , Typography.center
            ]
            [ text "404" ]
        ]


view : Model -> Html Msg
view model =
    let
        selectedTab =
            (Array.get model.ui.selectedTab tabViews |> Maybe.withDefault e404) model
    in
        Html.div
            [ A.id "app"
            ]
            [ Html.node "link"
                [ A.rel "stylesheet"
                , A.href "https://fonts.googleapis.com/css?family=Roboto:400,300,500|Roboto+Mono|Roboto+Condensed:400,700&subset=latin,latin-ext"
                ]
                []
            , Html.node "link"
                [ A.rel "stylesheet"
                , A.href "https://fonts.googleapis.com/icon?family=Material+Icons"
                ]
                []
            , Html.node "link"
                [ A.rel "stylesheet"
                , A.href "https://code.getmdl.io/1.3.0/material.teal-pink.min.css"
                ]
                []
            , Layout.render Mdl
                model.mdl
                [ Layout.fixedHeader
                , Layout.onSelectTab UISelectTab
                ]
                { header = headerView
                , drawer = []
                , tabs = ( tabTitles, [] )
                , main = [ selectedTab ]
                }
            ]


headerView : List (Html Msg)
headerView =
    [ Layout.row
        []
        [ Layout.title [] [ text "Cycles" ]
        , Layout.spacer
        , Layout.navigation []
            [ Layout.link
                [ Layout.href "https://github.com/rchampourlier/cycles" ]
                [ Html.span [] [ text "github" ] ]
            ]
        ]
    ]


cyclesTab : Model -> Html Msg
cyclesTab model =
    div [ id "app-cycles-tab" ]
        [ Button.render Mdl
            [ 0 ]
            model.mdl
            [ Button.raised
            , Options.onClick CreateCycle
            ]
            [ text "Add cycle" ]
        , cycleColumns model.cycles
        , dialog model
        ]


cycleColumns : List Cycle -> Html Msg
cycleColumns cycles =
    div [ id "app-cycles-tab-columns" ]
        (cycles
            |> List.reverse
            |> List.map cycleColumn
        )


cycleColumn : Cycle -> Html Msg
cycleColumn cycle =
    Options.div
        [ cs "cycle-column" ]
        [ cycleCard cycle ]


cycleCard : Cycle -> Html Msg
cycleCard cycle =
    Card.view
        [ cs "cycle-card"
        , css "width" "128px"
        , Color.background (Color.color Color.Pink Color.S500)
        ]
        [ Card.title [] [ Card.head [ Color.text Color.white ] [ text <| toString cycle.index ] ]
        ]


projectsTab : Model -> Html Msg
projectsTab model =
    div [] [ text "Projects" ]


peopleTab : Model -> Html Msg
peopleTab model =
    div [] [ text "People" ]


dialog : Model -> Html Msg
dialog model =
    let
        dialogKind =
            model.ui.dialogKind
    in
        case dialogKind of
            DialogNone ->
                dialogEmpty model


dialogEmpty : Model -> Html Msg
dialogEmpty model =
    Dialog.view
        []
        [ Dialog.title [] [ text "Unexpected dialog" ]
        , Dialog.content [] []
        , Dialog.actions []
            [ Button.render Mdl
                [ 0 ]
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
