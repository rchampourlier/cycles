module Main exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html, text)
import Html.Attributes as A exposing (class, href)
import List as L
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


type alias Model =
    { cycles : List Cycle -- sorted by index, desc
    , people : List Person
    , projects : List Project
    , mdl : Material.Model
    , ui : UIModel
    }



{- MDL key references

   - 3xx serie: people tab
     310: person row delete button

-}


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
      -- projects
    | AddProject
    | CreateProjectFromDialog
      -- people
    | AddNewPerson
    | UpdatePersonName Int String -- personIndex, newValue
    | LeavePersonNameEdition Int -- personIndex
    | UpdatePersonRole Int String -- personIndex, newValue
    | DeletePerson Int -- personIndex
      -- ui
    | UISelectTab Int
    | UIDialogUpdateFieldForProjectName String
    | UIDialogReset
      -- mdl
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

            -- PROJECTS
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

            -- PEOPLE
            AddNewPerson ->
                ( model
                    |> addNewPerson
                , Cmd.none
                )

            UpdatePersonName personIndex newName ->
                ( model |> updatePersonName personIndex newName, Cmd.none )

            LeavePersonNameEdition personIndex ->
                let
                    newModel =
                        model
                            |> removeInvalidPersonAtIndex personIndex
                            |> sortPeople
                in
                    ( newModel, Cmd.none )

            UpdatePersonRole personIndex newRoleId ->
                ( model |> updatePersonRole personIndex newRoleId, Cmd.none )

            DeletePerson pIndex ->
                ( model |> removePersonAtIndex pIndex, Cmd.none )

            -- UI
            UISelectTab k ->
                ( { model | ui = { ui_ | selectedTab = k } }, Cmd.none )

            UIDialogUpdateFieldForProjectName name ->
                ( { model | ui = { ui_ | dialogFieldName = name } }, Cmd.none )

            UIDialogReset ->
                ( updateForDialog DialogNone model, Cmd.none )

            Mdl msg_ ->
                Material.update Mdl msg_ model


updatePersonRole : Int -> String -> Model -> Model
updatePersonRole personIndex newRoleId model =
    case roleForId newRoleId of
        Nothing ->
            model

        Just role ->
            let
                updatePerson index person =
                    if index == personIndex then
                        { person | roleId = role.id }
                    else
                        person
            in
                { model | people = model.people |> List.indexedMap updatePerson }


updatePersonName : Int -> String -> Model -> Model
updatePersonName personIndex newName model =
    let
        updatePerson index person =
            if index == personIndex then
                { person | name = newName }
            else
                person
    in
        { model | people = model.people |> List.indexedMap updatePerson }


isPersonNameAlreadyTakenAtIndex : Int -> Model -> Bool
isPersonNameAlreadyTakenAtIndex pIndex model =
    case personAtIndex pIndex model of
        Nothing ->
            False

        Just personAtIndex_ ->
            (model.people
                |> List.filter (\p -> p.name == personAtIndex_.name)
                |> List.length
            )
                > 1


isPersonNameEmptyAtIndex : Int -> Model -> Bool
isPersonNameEmptyAtIndex pIndex model =
    let
        personAtIndex_ =
            personAtIndex pIndex model
    in
        case personAtIndex_ of
            Nothing ->
                False

            Just p ->
                String.isEmpty p.name


personAtIndex : Int -> Model -> Maybe Person
personAtIndex pIndex model =
    model.people
        |> L.drop pIndex
        |> L.head


personNameAtIndex : Int -> Model -> Maybe String
personNameAtIndex pIndex model =
    case personAtIndex pIndex model of
        Nothing ->
            Nothing

        Just person ->
            Just person.name


removeInvalidPersonAtIndex : Int -> Model -> Model
removeInvalidPersonAtIndex pIndex model =
    {- If the person at the specified index is invalid (duplicate or
       empty name, removes it.
    -}
    let
        personNameAtIndex_ =
            personNameAtIndex pIndex model
    in
        if errorForPersonAtIndex pIndex model then
            removePersonAtIndex pIndex model
        else
            model


removePersonAtIndex : Int -> Model -> Model
removePersonAtIndex pIndex model =
    let
        people =
            model.people
    in
        { model | people = (List.take pIndex people) ++ (List.drop (pIndex + 1) people) }


errorForPersonAtIndex : Int -> Model -> Bool
errorForPersonAtIndex pIndex model =
    case errorMessageForPersonAtIndex pIndex model of
        Nothing ->
            False

        Just m ->
            True


errorMessageForPersonAtIndex : Int -> Model -> Maybe String
errorMessageForPersonAtIndex pIndex model =
    if isPersonNameAlreadyTakenAtIndex pIndex model then
        Just "Name already taken"
    else if isPersonNameEmptyAtIndex pIndex model then
        Just "Name cannot be empty"
    else
        Nothing


addProject : Project -> Model -> Model
addProject project model =
    { model | projects = (project :: model.projects) }


sortPeople : Model -> Model
sortPeople model =
    { model | people = model.people |> List.sortBy .name }


addNewPerson : Model -> Model
addNewPerson model =
    let
        newPerson =
            { name = "New person", roleId = "product_manager" }

        newModel =
            { model | people = (newPerson :: model.people) }
    in
        newModel |> removeInvalidPersonAtIndex 0


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
        roleSelect : Int -> Person -> Html Msg
        roleSelect pIndex person =
            Select.render Mdl
                [ 4, pIndex ]
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
                                [ Item.onSelect (UpdatePersonRole pIndex roleId)
                                ]
                                [ text <| roleReadForId roleId
                                ]
                        )
                )

        nameTextfield pIndex person =
            Textfield.render Mdl
                [ 3, pIndex ]
                model.mdl
                [ Textfield.value <| person.name
                , Options.onInput <| UpdatePersonName pIndex
                , Options.onBlur <| LeavePersonNameEdition pIndex
                , Textfield.error (Maybe.withDefault "" (errorMessageForPersonAtIndex pIndex model))
                    |> Options.when (errorForPersonAtIndex pIndex model && isTextfieldFocused [ 3, pIndex ] model)
                ]
                []

        deleteButton model pIndex =
            Button.render Mdl
                [ 310, pIndex ]
                model.mdl
                [ Button.icon
                , Options.onClick (DeletePerson pIndex)
                ]
                [ Icon.i "delete" ]

        personRow pIndex person =
            List.li []
                [ List.content []
                    [ List.avatarIcon "format_paint" []
                    , nameTextfield pIndex person
                    , roleSelect pIndex person
                    ]
                , deleteButton model pIndex
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


isTextfieldFocused : List Int -> Model -> Bool
isTextfieldFocused key model =
    case Dict.get key model.mdl.textfield of
        Nothing ->
            False

        Just tf ->
            tf.isFocused



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModelForDev, Cmd.none )
        , view = view
        , update = update
        , subscriptions = Material.subscriptions Mdl
        }
