module Main exposing (..)

import Array exposing (Array)
import Html exposing (Html, text)
import Html.Attributes as A exposing (class, href)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Color as Color
import Material.Dialog as Dialog
import Material.Grid as Grid
import Material.Icon as Icon
import Material.List as List
import Material.Dropdown.Item as Item
import Material.Options as Options exposing (cs, css, div, id)
import Material.Select as Select
import Material.Textfield as Textfield
import Material.Typography as Typography
import Material.Layout as Layout


-- MODEL


type alias Cycle =
    { index : Int
    }


type alias Project =
    { name : String
    }


type alias Person =
    { name : String
    , roleId : String
    }


type alias Role =
    { id : String
    , read : String
    }


roles : List Role
roles =
    [ { id = "product_manager", read = "Product Manager" }
    , { id = "backend_developer", read = "Backend Developer" }
    , { id = "frontend_developer", read = "Frontend Developer" }
    ]


roleForId : String -> Maybe Role
roleForId id =
    roleForId_ id roles


roleForId_ : String -> List Role -> Maybe Role
roleForId_ id remainingMap =
    case remainingMap of
        [] ->
            Nothing

        h :: t ->
            if h.id == id then
                Just h
            else
                roleForId_ id t


roleReadForId : String -> String
roleReadForId id =
    case roleForId id of
        Nothing ->
            "Unexpected role ID"

        Just role ->
            role.read


allRoleIds : List String
allRoleIds =
    List.map .id roles


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
    , people : List Person
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
    , people = []
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

        people =
            [ { name = "Raphaëlle", roleId = "product_manager" } ]
    in
        { cycles = cycles
        , people = people
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
    | AddNewPerson
    | UISelectTab Int
    | UIDialogUpdateFieldForProjectName String
    | UIDialogReset
    | UIUpdatePersonName String String -- 1st matches person's name, 2nd is update value
    | UISelectPersonRole String String -- 1st matches person's name, 2nd is update value
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

            AddNewPerson ->
                ( addNewPerson model, Cmd.none )

            UISelectTab k ->
                ( { model | ui = { ui_ | selectedTab = k } }, Cmd.none )

            UIDialogUpdateFieldForProjectName name ->
                ( { model | ui = { ui_ | dialogFieldName = name } }, Cmd.none )

            UIDialogReset ->
                ( updateForDialog DialogNone model, Cmd.none )

            UIUpdatePersonName personName newName ->
                let
                    updatedPeople =
                        model.people
                            |> List.map
                                (\p ->
                                    if p.name == personName then
                                        { p | name = newName }
                                    else
                                        p
                                )
                in
                    ( { model | people = updatedPeople }, Cmd.none )

            UISelectPersonRole personName newRoleId ->
                case roleForId newRoleId of
                    Nothing ->
                        ( model, Cmd.none )

                    Just role ->
                        let
                            updatedPeople =
                                model.people
                                    |> List.map
                                        (\p ->
                                            if p.name == personName then
                                                { p | roleId = role.id }
                                            else
                                                p
                                        )
                        in
                            ( { model | people = updatedPeople }, Cmd.none )

            Mdl msg_ ->
                Material.update Mdl msg_ model


addProject : Project -> Model -> Model
addProject project model =
    { model | projects = (project :: model.projects) }


addNewPerson : Model -> Model
addNewPerson model =
    let
        newPerson =
            { name = "New person", roleId = "product_manager" }
    in
        { model | people = (newPerson :: model.people) }


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
    Html.div [ A.id "app" ] [ layout model ]


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
            [ 1 ]
            model.mdl
            [ Button.raised
            , Options.onClick AddProject
            , Dialog.openOn "click"
            ]
            [ text "Add project" ]
        , div [ cs "projects" ]
            [ model.projects
                |> List.indexedMap (projectCard model)
                |> Grid.grid []
            ]
        ]


projectCard : Model -> Int -> Project -> Grid.Cell Msg
projectCard model index project =
    Grid.cell
        [ css "height" "200px", Grid.size Grid.All 4 ]
        [ Card.view
            [ Color.background (Color.color Color.DeepPurple Color.S300)
            ]
            [ Card.media
                [ css "height" "225px"
                ]
                []
            , Card.title []
                [ Card.head [ Color.text Color.white ] [ text project.name ]
                , Card.subhead [ Color.text Color.white ] [ text "(No project details for now)" ]
                ]
            , Card.menu []
                [ Button.render Mdl
                    [ 1, index ]
                    model.mdl
                    [ Button.icon, Button.ripple, Color.text Color.white ]
                    [ Icon.i "delete" ]
                ]
            ]
        ]


peopleTab : Model -> Html Msg
peopleTab model =
    div []
        [ addNewPersonButton model
        , peopleTable model
        ]


addNewPersonButton : Model -> Html Msg
addNewPersonButton model =
    Button.render Mdl
        [ 2 ]
        model.mdl
        [ Button.raised
        , Options.onClick AddNewPerson
        ]
        [ text "Add person" ]


peopleTable : Model -> Html Msg
peopleTable model =
    let
        roleSelect index person =
            Select.render Mdl
                [ 4, index ]
                model.mdl
                [ Select.label "Role"
                , Select.floatingLabel
                , Select.ripple
                , Select.value <| roleReadForId person.roleId
                ]
                (allRoleIds
                    |> List.map
                        (\roleId ->
                            Select.item
                                [ Item.onSelect (UISelectPersonRole person.name roleId)
                                ]
                                [ text <| roleReadForId roleId
                                ]
                        )
                )

        nameTextfield index person =
            Textfield.render Mdl
                [ 3, index ]
                model.mdl
                [ Textfield.value <| person.name
                , Options.onInput <| UIUpdatePersonName person.name
                ]
                []

        personRow index person =
            List.li []
                [ List.content []
                    [ List.avatarIcon "format_paint" []
                    , nameTextfield index person
                    , roleSelect index person
                    ]
                ]
    in
        List.ul [ cs "people" ]
            (model.people
                |> List.indexedMap personRow
            )


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
                [ 5 ]
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
                [ 6 ]
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
                [ 7 ]
                model.mdl
                [ Dialog.closeOn "click"
                , Options.onClick CreateProjectFromDialog
                ]
                [ text "Create" ]
            , Button.render Mdl
                [ 8 ]
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
