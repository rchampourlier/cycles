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
import Material.Elevation as Elevation
import Material.Grid as Grid
import Material.Icon as Icon
import Material.List as List
import Material.Dropdown.Item as Item
import Material.Options as Options exposing (cs, css, div, id)
import Material.Select as Select
import Material.Table as Table
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Typography as Typography
import Material.Layout as Layout
import Set exposing (Set)


toggle : comparable -> Set comparable -> Set comparable
toggle x set =
    if Set.member x set then
        Set.remove x set
    else
        Set.insert x set



-- MODEL


type alias Cycle =
    { index : CycleIndex
    }


type CycleIndex
    = CycleIndex Int


cycleIndexToInt : CycleIndex -> Int
cycleIndexToInt cycleIdx =
    case cycleIdx of
        CycleIndex i ->
            i


type alias Project =
    { name : ProjectName
    }


type ProjectName
    = ProjectName String


type RoleID
    = RoleID String


type alias Plan =
    { cycleIndex : CycleIndex
    , projectName : ProjectName
    , assignments : List Assignment
    }


type alias Person =
    { name : String
    , roleId : RoleID
    }


type alias Role =
    { id : RoleID
    , read : String
    }


type alias Assignment =
    { personName : String
    }


roles : List Role
roles =
    [ { id = RoleID "product_manager", read = "Product Manager" }
    , { id = RoleID "backend_developer", read = "Backend Developer" }
    , { id = RoleID "frontend_developer", read = "Frontend Developer" }
    ]


roleForId : RoleID -> Maybe Role
roleForId id =
    roleForId_ id roles


roleForId_ : RoleID -> List Role -> Maybe Role
roleForId_ id remainingMap =
    case remainingMap of
        [] ->
            Nothing

        h :: t ->
            if h.id == id then
                Just h
            else
                roleForId_ id t



-- TODO: use Result instead of Maybe


roleReadForId : RoleID -> String
roleReadForId id =
    case roleForId id of
        Nothing ->
            "Unexpected role ID"

        Just role ->
            role.read


allRoleIds : List RoleID
allRoleIds =
    List.map .id roles


type DialogKind
    = DialogNone
    | DialogAddProject
    | DialogAddProjectsToCycle CycleIndex


type alias Model =
    { cycles : List Cycle -- sorted by index, desc
    , people : List Person
    , plans : List Plan
    , projects : List Project
    , mdl : Material.Model
    , ui : UIModel
    }


type alias UIModel =
    { dialogKind : DialogKind
    , dialogFieldName : String
    , dialogAddPlanSelected : Set String -- project names as String
    , selectedTab : Int
    }



{- MDL key references

   # 1xx serie: cycles tab
     110: cycle column, plan project button

   # 3xx serie: people tab
     310: person row delete button

   # 9xx serie: dialog
     901: cancel button
     902: action button

-}


initialUIModel : UIModel
initialUIModel =
    { dialogKind = DialogNone
    , dialogFieldName = ""
    , dialogAddPlanSelected = Set.empty
    , selectedTab = 0
    }


initialModel : Model
initialModel =
    { cycles = []
    , people = []
    , plans = []
    , projects = []
    , mdl = Material.model
    , ui = initialUIModel
    }



-- An initial model to ease development
-- TODO: extract module for RoleID to enforce it using
--   the single constructor union type


initialModelForDev : Model
initialModelForDev =
    let
        cycles =
            [ { index = CycleIndex 2 }
            , { index = CycleIndex 1 }
            ]

        people =
            [ { name = "Raphaëlle", roleId = RoleID "product_manager" }
            , { name = "Christophe", roleId = RoleID "product_manager" }
            , { name = "Ludovic", roleId = RoleID "backend_developer" }
            ]

        plans =
            [ { cycleIndex = CycleIndex 1
              , projectName = ProjectName "Company Pages #1"
              , assignments = []
              }
            ]

        projects =
            [ { name = ProjectName "Company Pages #1" }, { name = ProjectName "Event Newsletter" } ]
    in
        { cycles = cycles
        , people = people
        , plans = plans
        , projects = projects
        , mdl = Material.model
        , ui = initialUIModel
        }


nextIndex : Model -> CycleIndex
nextIndex model =
    let
        cycles =
            model.cycles
    in
        case cycles of
            [] ->
                CycleIndex 1

            c :: _ ->
                case c.index of
                    CycleIndex i ->
                        CycleIndex (i + 1)



-- UPDATE


type Msg
    = CreateCycle
      -- projects
    | AddProject
    | CreateProjectFromDialog
      -- people
    | AddNewPerson
    | UpdatePersonName Int String -- personIndex newValue
    | LeavePersonNameEdition Int -- personIndex
    | UpdatePersonRole Int RoleID -- personIndex...
    | DeletePerson Int -- personIndex
      -- plans
    | AddPlan ProjectName CycleIndex -- projectName cycleIndex
    | AddProjectsToCycle CycleIndex
    | AddProjectsToCycleFromDialog CycleIndex
      -- ui
    | UISelectTab Int
    | UIDialogAddProjectUpdateFieldName String
    | UIDialogAddProjectsToCycleToggleAll
    | UIDialogAddProjectsToCycleToggle String
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
                ( updateUIDialogKind DialogAddProject model, Cmd.none )

            CreateProjectFromDialog ->
                let
                    newProject =
                        { name = ProjectName model.ui.dialogFieldName }
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

            -- PLANS
            AddPlan projectName cycleIndex ->
                -- TODO
                ( model, Cmd.none )

            AddProjectsToCycle cycleIdx ->
                ( model |> updateUIDialogKind (DialogAddProjectsToCycle cycleIdx), Cmd.none )

            AddProjectsToCycleFromDialog cycleIdx ->
                ( model
                    |> addPlanFromDialog cycleIdx
                    |> updateUIDialogReset
                , Cmd.none
                )

            -- UI
            UISelectTab k ->
                ( { model | ui = { ui_ | selectedTab = k } }, Cmd.none )

            UIDialogAddProjectUpdateFieldName name ->
                ( { model | ui = { ui_ | dialogFieldName = name } }, Cmd.none )

            UIDialogAddProjectsToCycleToggleAll ->
                ( model |> updateUIDialogAddProjectsToCycleToggleAll, Cmd.none )

            UIDialogAddProjectsToCycleToggle projectNameStr ->
                ( model |> updateUIDialogAddProjectsToCycleToggle projectNameStr, Cmd.none )

            UIDialogReset ->
                ( model |> updateUIDialogReset, Cmd.none )

            Mdl msg_ ->
                Material.update Mdl msg_ model



-- UPDATE/CYCLES


cycleAtIndex : CycleIndex -> Model -> Maybe Cycle
cycleAtIndex cycleIdx model =
    model.cycles
        |> List.filter (\c -> c.index == cycleIdx)
        |> List.head


updateCycleAtIndex : CycleIndex -> Cycle -> Model -> Model
updateCycleAtIndex cycleIdx newCycle model =
    let
        newCycles =
            model.cycles
                |> List.map
                    (\c ->
                        if c.index == cycleIdx then
                            newCycle
                        else
                            c
                    )
    in
        { model | cycles = newCycles }



-- UPDATE/PEOPLE


updatePersonRole : Int -> RoleID -> Model -> Model
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


sortPeople : Model -> Model
sortPeople model =
    { model | people = model.people |> List.sortBy .name }


addNewPerson : Model -> Model
addNewPerson model =
    let
        newPerson =
            { name = "New person", roleId = RoleID "product_manager" }

        newModel =
            { model | people = (newPerson :: model.people) }
    in
        newModel |> removeInvalidPersonAtIndex 0



-- UPDATE/PLANS


plannedProjectNamesForCycle : CycleIndex -> Model -> List ProjectName
plannedProjectNamesForCycle cycleIdx model =
    model.plans
        |> List.filter (\p -> p.cycleIndex == cycleIdx)
        |> List.map .projectName



-- UPDATE/PROJECTS


addPlanFromDialog : CycleIndex -> Model -> Model
addPlanFromDialog cycleIdx model =
    let
        cycle_ =
            cycleAtIndex cycleIdx model

        selectedProjects : List ProjectName
        selectedProjects =
            model.ui.dialogAddPlanSelected
                |> Set.toList
                |> List.map (\s -> ProjectName s)
    in
        case cycle_ of
            Nothing ->
                model

            Just cycle ->
                let
                    newPlans =
                        selectedProjects
                            |> List.map (\pn -> { cycleIndex = cycleIdx, projectName = pn, assignments = [] })
                in
                    model |> addPlans newPlans


addPlans : List Plan -> Model -> Model
addPlans plans model =
    { model | plans = model.plans ++ plans }


addProject : Project -> Model -> Model
addProject project model =
    { model | projects = (project :: model.projects) }


projectNameToString : ProjectName -> String
projectNameToString projectNameStr =
    case projectNameStr of
        ProjectName s ->
            s


projectForName : Model -> ProjectName -> Maybe Project
projectForName model projectName =
    model.projects
        |> List.filter (\p -> p.name == projectName)
        |> List.head



-- UPDATE/DIALOGS


resetDialog : Model -> Model
resetDialog model =
    let
        ui_ =
            model.ui
    in
        { model | ui = { ui_ | dialogFieldName = "" } }


updateUIDialogReset : Model -> Model
updateUIDialogReset model =
    model
        |> updateUIDialogKind DialogNone
        |> updateUIDialogAddProjectsToCycleToggleNone


updateUIDialogKind : DialogKind -> Model -> Model
updateUIDialogKind newKind model =
    let
        ui_ =
            model.ui

        newUI =
            { ui_ | dialogKind = newKind }
    in
        { model | ui = newUI }


updateUIDialogAddProjectsToCycleToggleNone : Model -> Model
updateUIDialogAddProjectsToCycleToggleNone model =
    let
        ui_ =
            model.ui
    in
        { model | ui = { ui_ | dialogAddPlanSelected = Set.empty } }


updateUIDialogAddProjectsToCycleToggleAll : Model -> Model
updateUIDialogAddProjectsToCycleToggleAll model =
    let
        ui_ =
            model.ui

        allProjectNames =
            model.projects
                |> List.map (.name >> projectNameToString)
                |> Set.fromList

        alreadySelected =
            areDialogAddProjectsToCycleAllSelected model
    in
        { model
            | ui =
                { ui_
                    | dialogAddPlanSelected =
                        if alreadySelected then
                            Set.empty
                        else
                            allProjectNames
                }
        }


updateUIDialogAddProjectsToCycleToggle : String -> Model -> Model
updateUIDialogAddProjectsToCycleToggle projectNameStr model =
    let
        ui_ =
            model.ui
    in
        { model
            | ui =
                { ui_
                    | dialogAddPlanSelected = toggle projectNameStr ui_.dialogAddPlanSelected
                }
        }


areDialogAddProjectsToCycleAllSelected : Model -> Bool
areDialogAddProjectsToCycleAllSelected model =
    Set.size model.ui.dialogAddPlanSelected == List.length model.projects



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
        , cycleColumns model
        ]


cycleColumns : Model -> Html Msg
cycleColumns model =
    div [ cs "cycles" ]
        (model.cycles
            |> List.reverse
            |> List.map (cycleColumn model)
        )


cycleColumn : Model -> Cycle -> Html Msg
cycleColumn model cycle =
    div
        [ cs "cycles--column" ]
        (cycleColumnHeader model cycle :: cycleColumnProjects model cycle)


cycleColumnHeader : Model -> Cycle -> Html Msg
cycleColumnHeader model cycle =
    let
        cycleIdx =
            cycleIndexToInt cycle.index
    in
        Card.view
            [ cs "cycles--column--header"
            , css "width" "256px"
            , Elevation.e2
            ]
            [ Card.title []
                [ Card.head []
                    [ text <| toString cycle.index ]
                ]
            , Card.actions
                [ Card.border

                -- Modify flexbox to accomodate small text in action block
                , css "display" "flex"
                , css "justify-content" "space-between"
                , css "align-items" "center"
                , css "padding" "8px 16px 8px 16px"
                , css "color" "white"
                ]
                [ Button.render Mdl
                    [ 110, cycleIdx ]
                    model.mdl
                    [ Button.ripple
                    , Options.onClick (AddProjectsToCycle cycle.index)
                    , Dialog.openOn "click"
                    ]
                    [ text "Plan projects" ]
                ]
            ]


cycleColumnProjects : Model -> Cycle -> List (Html Msg)
cycleColumnProjects model cycle =
    plannedProjectNamesForCycle cycle.index model
        |> List.filterMap (\n -> projectForName model n)
        |> List.map cycleColumnProjectCard


cycleColumnProjectCard : Project -> Html Msg
cycleColumnProjectCard project =
    let
        assignmentsTable =
            Table.table []
                [ Table.thead []
                    [ Table.tr []
                        [ Table.th [] [ Icon.i "format_paint" ]
                        , Table.th [] [ Icon.i "format_paint" ]
                        , Table.th [] [ Icon.i "format_paint" ]
                        ]
                    ]
                , Table.tbody []
                    [ Table.tr []
                        [ Table.td [] [ text "2" ]
                        , Table.td [ Table.numeric ] [ text "1" ]
                        , Table.td [ Table.numeric ] [ text "0" ]
                        ]
                    ]
                ]

        {-
           [ Options.span [ css "width" "64px", css "text-align" "center" ]
               [ Icon.i "format_paint" ]
           , Options.span [ css "width" "64px", css "text-align" "right" ]
               [ text <| toString "Other text"
               , Options.span
                   [ css "color" "rgba(0,0,0,0.37)" ]
                   [ text "Another one" ]
               ]
           ]
        -}
    in
        Card.view
            [ css "width" "256px"
            , Elevation.e2
            ]
            [ Card.title
                [ css "flex-direction" "column" ]
                [ Card.head [] [ text <| toString (projectNameToString project.name) ]
                , Card.subhead [] [ text "No description for now" ]
                ]
            , Card.actions []
                [ div
                    [ css "display" "flex"
                    , css "flex-direction" "column"
                    , css "padding" "1rem 0"
                    , css "color" "rgba(0, 0, 0, 0.54)"
                    ]
                    [ assignmentsTable ]
                ]
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
                [ Card.head [ Color.text Color.white ] [ text <| projectNameToString project.name ]
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
                dialogError "Unexpected dialog" model

            DialogAddProject ->
                dialogAddProject model

            DialogAddProjectsToCycle cycleIdx ->
                let
                    cycle_ =
                        cycleAtIndex cycleIdx model
                in
                    case cycle_ of
                        Nothing ->
                            dialogError "Unknown cycle" model

                        Just cycle ->
                            dialogAddPlan cycle model


dialogError : String -> Model -> Html Msg
dialogError message model =
    Dialog.view
        []
        [ Dialog.title [] [ text message ]
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
                [ Options.onInput UIDialogAddProjectUpdateFieldName
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


dialogAddPlan : Cycle -> Model -> Html Msg
dialogAddPlan cycle model =
    let
        cycleIdxInt =
            cycleIndexToInt cycle.index

        addableProjectNameStrings : List String
        addableProjectNameStrings =
            model.projects
                |> List.map .name
                |> List.filter (\pn -> not (List.member pn (plannedProjectNamesForCycle cycle.index model)))
                |> List.map projectNameToString

        dialogTitle =
            Dialog.title [] [ text ("Plan projects on cycle" ++ (toString cycleIdxInt)) ]

        cancelButton =
            Button.render Mdl
                [ 901 ]
                model.mdl
                [ Dialog.closeOn "click"
                , Options.onClick UIDialogReset
                ]
                [ text "Cancel" ]

        noAddableProjectsDialog =
            Dialog.view []
                [ dialogTitle
                , Dialog.content []
                    [ Html.p [] [ text "All available projects have already been added to this cycle." ]
                    ]
                , Dialog.actions [] [ cancelButton ]
                ]
    in
        if List.isEmpty addableProjectNameStrings then
            noAddableProjectsDialog
        else
            Dialog.view []
                [ dialogTitle
                , Dialog.content []
                    [ Table.table []
                        [ Table.thead []
                            [ Table.tr []
                                [ Table.th []
                                    [ Toggles.checkbox Mdl
                                        [ -1 ]
                                        model.mdl
                                        [ Options.onToggle UIDialogAddProjectsToCycleToggleAll
                                        , Toggles.value (areDialogAddProjectsToCycleAllSelected model)
                                        ]
                                        []
                                    ]
                                , Table.th [] [ text "Projects" ]
                                ]
                            ]
                        , Table.tbody []
                            (addableProjectNameStrings
                                |> List.indexedMap
                                    (\idx projectNameStr ->
                                        Table.tr
                                            [ Table.selected |> Options.when (Set.member projectNameStr model.ui.dialogAddPlanSelected) ]
                                            [ Table.td []
                                                [ Toggles.checkbox Mdl
                                                    [ idx ]
                                                    model.mdl
                                                    [ Options.onToggle (UIDialogAddProjectsToCycleToggle projectNameStr)
                                                    , Toggles.value <| Set.member projectNameStr model.ui.dialogAddPlanSelected
                                                    ]
                                                    []
                                                ]
                                            , Table.td [] [ text projectNameStr ]
                                            ]
                                    )
                            )
                        ]
                    ]
                , Dialog.actions []
                    [ Button.render Mdl
                        [ 902 ]
                        model.mdl
                        [ Dialog.closeOn "click"
                        , Options.onClick (AddProjectsToCycleFromDialog cycle.index)
                        ]
                        [ text "Plan" ]
                    , cancelButton
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
