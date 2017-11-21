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
import Material.Grid as Grid
import Material.Icon as Icon
import Material.Options as Options exposing (cs, css, div, id)
import Material.Scheme
import Material.Textfield as Textfield
import Material.Typography as Typography
import Random
import Task exposing (Task)
import Time exposing (Time)
import Material.Layout as Layout


-- MODEL


type alias Cycle =
    { index : Int
    }


type alias Project =
    { name : String
    }


type DialogKind
    = DialogNone
    | DialogAddProject


type alias UIModel =
    { dialogKind : DialogKind
    , dialogFieldName : String
    , selectedTab : Int
    }



{- cycles are sorted by index, descending, so that the greatest index
   is first.
-}


type alias Model =
    { cycles : List Cycle
    , projects : List Project
    , mdl : Material.Model
    , ui : UIModel
    }


initialUIModel : UIModel
initialUIModel =
    { dialogKind = DialogNone
    , dialogFieldName = ""
    , selectedTab = 0
    }


initialModel : Model
initialModel =
    { cycles = []
    , projects = []
    , mdl = Material.model
    , ui = initialUIModel
    }



-- An initial model to ease development


initialModelForDev : Model
initialModelForDev =
    let
        cycles =
            [ { index = 2 }, { index = 1 } ]

        projects =
            [ { name = "Company Pages #1" }, { name = "Event Newsletter" } ]
    in
        { cycles = cycles
        , projects = projects
        , mdl = Material.model
        , ui = initialUIModel
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
    | AddProject
    | CreateProjectFromDialog
    | UISelectTab Int
    | UIDialogUpdateFieldForProjectName String
    | UIDialogReset
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

            AddProject ->
                ( updateForDialog DialogAddProject model, Cmd.none )

            CreateProjectFromDialog ->
                let
                    newProject =
                        { name = model.ui.dialogFieldName }
                in
                    ( model
                        |> addProject newProject
                        |> resetDialog
                    , Cmd.none
                    )

            UISelectTab k ->
                ( { model | ui = { ui_ | selectedTab = k } }, Cmd.none )

            UIDialogUpdateFieldForProjectName name ->
                ( { model | ui = { ui_ | dialogFieldName = name } }, Cmd.none )

            UIDialogReset ->
                ( updateForDialog DialogNone model, Cmd.none )

            Mdl msg_ ->
                Material.update Mdl msg_ model


addProject : Project -> Model -> Model
addProject project model =
    { model | projects = (project :: model.projects) }


resetDialog : Model -> Model
resetDialog model =
    let
        ui_ =
            model.ui
    in
        { model | ui = { ui_ | dialogFieldName = "" } }


updateForDialog : DialogKind -> Model -> Model
updateForDialog newKind model =
    let
        ui_ =
            model.ui

        newUI =
            { ui_ | dialogKind = newKind }
    in
        { model | ui = newUI }



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
    Html.div [ A.id "app" ] [ top, layout model ]



{- This is for prototyping in elm-reactor. For production,
   should be moved to HTML/CSS assets.
-}


top : Html Msg
top =
    div []
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
        , Html.node "style" [] [ text """
        #elm-mdl-layout-main {
          z-index: 0; /* to enable clicking the elm-reactor's overlay */
        }
        .cycles {
          display: flex;
        }
        .cycles--column {
          padding-right: 10px;
        }
        .projects .mdl-card {
          width: auto;
        }
        """ ]
        ]


layout : Model -> Html Msg
layout model =
    let
        selectedTab =
            (Array.get model.ui.selectedTab tabViews |> Maybe.withDefault e404) model
    in
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            , Layout.onSelectTab UISelectTab
            ]
            { header = headerView
            , drawer = []
            , tabs = ( tabTitles, [] )
            , main = [ selectedTab, dialog model ]
            }


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
    div []
        [ Button.render Mdl
            [ 0 ]
            model.mdl
            [ Button.raised
            , Options.onClick CreateCycle
            ]
            [ text "Add cycle" ]
        , cycleColumns model.cycles
        ]


cycleColumns : List Cycle -> Html Msg
cycleColumns cycles =
    div [ cs "cycles" ]
        (cycles
            |> List.reverse
            |> List.map cycleColumn
        )


cycleColumn : Cycle -> Html Msg
cycleColumn cycle =
    Options.div
        [ cs "cycles--column" ]
        [ cycleCard cycle ]


cycleCard : Cycle -> Html Msg
cycleCard cycle =
    Card.view
        [ css "width" "128px"
        , Color.background (Color.color Color.Pink Color.S500)
        ]
        [ Card.title [] [ Card.head [ Color.text Color.white ] [ text <| toString cycle.index ] ]
        ]


projectsTab : Model -> Html Msg
projectsTab model =
    div []
        [ Button.render Mdl
            [ 0 ]
            model.mdl
            [ Button.raised
            , Options.onClick AddProject
            , Dialog.openOn "click"
            ]
            [ text "Add project" ]
        , projectCards model
        ]


projectCards : Model -> Html Msg
projectCards model =
    div [ cs "projects" ]
        [ model.projects
            |> List.map
                (\project ->
                    Grid.cell
                        [ css "height" "200px", Grid.size Grid.All 4 ]
                        [ projectCard project model ]
                )
            |> Grid.grid []
        ]


projectCard : Project -> Model -> Html Msg
projectCard project model =
    Card.view
        [ Color.background (Color.color Color.DeepPurple Color.S300)
        ]
        [ Card.media
            [ css "background" "url('assets/table.jpg') center / cover"
            , css "height" "225px"
            ]
            []
        , Card.title []
            [ Card.head [ Color.text Color.white ] [ text project.name ]
            , Card.subhead [ Color.text Color.white ] [ text "(No project details for now)" ]
            ]
        , Card.menu []
            [ Button.render Mdl
                [ 0 ]
                model.mdl
                [ Button.icon, Button.ripple, Color.text Color.white ]
                [ Icon.i "delete" ]
            ]
        ]


projectRow : Project -> Html Msg
projectRow project =
    div [] [ text project.name ]


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

            DialogAddProject ->
                dialogAddProject model


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


dialogAddProject : Model -> Html Msg
dialogAddProject model =
    Dialog.view
        []
        [ Dialog.title [] [ text "Add project" ]
        , Dialog.content []
            [ Textfield.render Mdl
                [ 0 ]
                model.mdl
                [ Options.onInput UIDialogUpdateFieldForProjectName
                , Textfield.label "Name"
                , Textfield.floatingLabel
                , Textfield.text_
                ]
                []
            ]
        , Dialog.actions []
            [ Button.render Mdl
                [ 0 ]
                model.mdl
                [ Dialog.closeOn "click"
                , Options.onClick CreateProjectFromDialog
                ]
                [ text "Create" ]
            , Button.render Mdl
                [ 1 ]
                model.mdl
                [ Dialog.closeOn "click"
                , Options.onClick UIDialogReset
                ]
                [ text "Cancel" ]
            ]
        ]



-- MAIN


main =
    Html.program
        { init = ( initialModelForDev, Cmd.none )
        , view = view
        , update = update
        , subscriptions = Material.subscriptions Mdl
        }
