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


type alias Plan =
    { cycleIndex : CycleIndex
    , projectName : ProjectName
    , assignments : List Assignment
    }


type alias Person =
    { name : String
    , roleID : RoleID
    }


type alias Role =
    { id : RoleID
    , read : String
    , icon : String
    }


type RoleID
    = RoleID String


roles : List Role
roles =
    [ { id = RoleID "product_manager", read = "Product Manager", icon = "directions" }
    , { id = RoleID "frontend_developer", read = "Frontend Developer", icon = "palette" }
    , { id = RoleID "backend_developer", read = "Backend Developer", icon = "settings" }
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


type alias Assignment =
    { personName : String
    }


type DialogKind
    = DialogNone
    | DialogAddProject
    | DialogAddPlan CycleIndex
    | DialogEditAssignments CycleIndex ProjectName RoleID


type alias Model =
    { cycles : List Cycle -- sorted by index, desc
    , persons : List Person
    , plans : List Plan
    , projects : List Project
    , mdl : Material.Model
    , ui : UIModel
    }


type alias UIModel =
    { dialogKind : DialogKind
    , dialogFieldName : String
    , dialogAddPlansSelected : Set String -- project names as String
    , dialogEditAssignmentsSelected : Set String -- person names
    , selectedTab : Int
    }



{- MDL key references

   # 1xx serie: cycles tab
     110: cycle column, plan project button
     120: cycle column, assignments (x changes for each role)

   # 3xx serie: persons tab
     310: person row delete button

   # 9xx serie: dialog
     901: cancel button
     90o: action button

-}


initialUIModel : UIModel
initialUIModel =
    { dialogKind = DialogNone
    , dialogFieldName = ""
    , dialogAddPlansSelected = Set.empty
    , dialogEditAssignmentsSelected = Set.empty
    , selectedTab = 0
    }


initialModel : Model
initialModel =
    { cycles = []
    , persons = []
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

        persons =
            [ { name = "Raphaëlle", roleID = RoleID "product_manager" }
            , { name = "Christophe", roleID = RoleID "product_manager" }
            , { name = "Ludovic", roleID = RoleID "backend_developer" }
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
        , persons = persons
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
      -- persons
    | AddNewPerson
    | UpdatePersonName Int String -- personIndex newValue
    | LeavePersonNameEdition Int -- personIndex
    | UpdatePersonRole Int RoleID -- personIndex...
    | DeletePerson Int -- personIndex
      -- plans
    | AddPlan ProjectName CycleIndex -- projectName cycleIndex
    | AddPlans CycleIndex
    | AddPlansFromDialog CycleIndex
      -- assignments
    | EditAssignments CycleIndex ProjectName RoleID
    | UpdateAssignmentsFromDialog CycleIndex ProjectName
      -- ui
    | UISelectTab Int
    | UIDialogAddProjectUpdateFieldName String
    | UIDialogAddPlansToggleAll
    | UIDialogAddPlansToggle String
    | UIDialogEditAssignmentsToggleAll
    | UIDialogEditAssignmentsToggle String -- personName
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

            -- PERSONS
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
                            |> sortPersons
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

            AddPlans cycleIdx ->
                ( model |> updateUIDialogKind (DialogAddPlan cycleIdx), Cmd.none )

            AddPlansFromDialog cycleIdx ->
                ( model
                    |> addPlansFromDialog cycleIdx
                    |> updateUIDialogReset
                , Cmd.none
                )

            -- ASSIGNMENTS
            EditAssignments cycleIdx projectName roleID ->
                ( model |> updateUIDialogKind (DialogEditAssignments cycleIdx projectName roleID), Cmd.none )

            UpdateAssignmentsFromDialog cycleIdx projectName ->
                ( model
                    |> addAssignmentsFromDialog cycleIdx projectName
                    |> updateUIDialogReset
                , Cmd.none
                )

            -- UI
            UISelectTab k ->
                ( { model | ui = { ui_ | selectedTab = k } }, Cmd.none )

            UIDialogAddProjectUpdateFieldName name ->
                ( { model | ui = { ui_ | dialogFieldName = name } }, Cmd.none )

            UIDialogAddPlansToggleAll ->
                ( model |> updateUIDialogAddPlansToggleAll, Cmd.none )

            UIDialogAddPlansToggle projectNameStr ->
                ( model |> updateUIDialogAddPlansToggle projectNameStr, Cmd.none )

            UIDialogEditAssignmentsToggleAll ->
                ( model |> updateUIDialogEditAssignmentsToggleAll, Cmd.none )

            UIDialogEditAssignmentsToggle personName ->
                ( model |> updateUIDialogEditAssignmentsToggle personName, Cmd.none )

            UIDialogReset ->
                ( model |> updateUIDialogReset, Cmd.none )

            Mdl msg_ ->
                Material.update Mdl msg_ model



-- UPDATE/CYCLES


cycleForIndex : CycleIndex -> Model -> Maybe Cycle
cycleForIndex cycleIdx model =
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



-- UPDATE/PERSONS


updatePersonRole : Int -> RoleID -> Model -> Model
updatePersonRole personIndex newRoleId model =
    case roleForId newRoleId of
        Nothing ->
            model

        Just role ->
            let
                updatePerson index person =
                    if index == personIndex then
                        { person | roleID = role.id }
                    else
                        person
            in
                { model | persons = model.persons |> List.indexedMap updatePerson }


updatePersonName : Int -> String -> Model -> Model
updatePersonName personIndex newName model =
    let
        updatePerson index person =
            if index == personIndex then
                { person | name = newName }
            else
                person
    in
        { model | persons = model.persons |> List.indexedMap updatePerson }


isPersonNameAlreadyTakenAtIndex : Int -> Model -> Bool
isPersonNameAlreadyTakenAtIndex pIndex model =
    case personAtIndex pIndex model of
        Nothing ->
            False

        Just personAtIndex_ ->
            (model.persons
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
    model.persons
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
        persons =
            model.persons
    in
        { model | persons = (List.take pIndex persons) ++ (List.drop (pIndex + 1) persons) }


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


sortPersons : Model -> Model
sortPersons model =
    { model | persons = model.persons |> List.sortBy .name }


addNewPerson : Model -> Model
addNewPerson model =
    let
        newPerson =
            { name = "New person", roleID = RoleID "product_manager" }

        newModel =
            { model | persons = (newPerson :: model.persons) }
    in
        newModel |> removeInvalidPersonAtIndex 0


personForName : Model -> String -> Maybe Person
personForName model name =
    model.persons
        |> List.filter (\p -> p.name == name)
        |> List.head


personNamesAvailableForCycle : CycleIndex -> Model -> List String
personNamesAvailableForCycle cycleIdx model =
    let
        personNames =
            model.persons |> List.map .name

        assignedPersonNames =
            plansForCycleIndex cycleIdx model
                |> List.concatMap .assignments
                |> List.map .personName
    in
        personNames
            |> List.filter (\pn -> not (List.member pn assignedPersonNames))


personsWithRole : RoleID -> Model -> List Person
personsWithRole roleID model =
    model.persons
        |> List.filter (\p -> p.roleID == roleID)



-- UPDATE/PLANS


plannedProjectNamesForCycle : CycleIndex -> Model -> List ProjectName
plannedProjectNamesForCycle cycleIdx model =
    model.plans
        |> List.filter (\p -> p.cycleIndex == cycleIdx)
        |> List.map .projectName


addPlansFromDialog : CycleIndex -> Model -> Model
addPlansFromDialog cycleIdx model =
    let
        cycle_ =
            cycleForIndex cycleIdx model

        selectedProjects : List ProjectName
        selectedProjects =
            model.ui.dialogAddPlansSelected
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


replacePlan : Plan -> Model -> Model
replacePlan newPlan model =
    let
        newPlans =
            model.plans
                |> List.map
                    (\p ->
                        if (samePlans p newPlan) then
                            newPlan
                        else
                            p
                    )
    in
        { model | plans = newPlans }


plansForCycleIndex : CycleIndex -> Model -> List Plan
plansForCycleIndex cycleIdx model =
    model.plans
        |> List.filter (\p -> p.cycleIndex == cycleIdx)


planForCycleIndexAndProjectName : CycleIndex -> ProjectName -> Model -> Maybe Plan
planForCycleIndexAndProjectName cycleIdx projectName model =
    model.plans
        |> List.filter (\p -> p.cycleIndex == cycleIdx && p.projectName == projectName)
        |> List.head


samePlans : Plan -> Plan -> Bool
samePlans planA planB =
    planA.cycleIndex == planB.cycleIndex && planA.projectName == planB.projectName



-- UPDATE/PROJECTS


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



-- UPDATE/ASSIGNMENTS


addAssignmentsFromDialog : CycleIndex -> ProjectName -> Model -> Model
addAssignmentsFromDialog cycleIdx projectName model =
    let
        plan_ =
            planForCycleIndexAndProjectName cycleIdx projectName model

        selectedPersonNames : List String
        selectedPersonNames =
            model.ui.dialogEditAssignmentsSelected
                |> Set.toList
    in
        case plan_ of
            Nothing ->
                model

            Just plan ->
                let
                    newAssignments =
                        selectedPersonNames
                            |> List.map (\pn -> { personName = pn })

                    newPlan =
                        { plan | assignments = plan.assignments ++ newAssignments }
                in
                    model |> replacePlan newPlan


countPersonsAssigned : RoleID -> CycleIndex -> ProjectName -> Model -> Int
countPersonsAssigned roleID cycleIdx pjName model =
    let
        plan_ =
            planForCycleIndexAndProjectName cycleIdx pjName model

        assignedPersons =
            case plan_ of
                Just plan ->
                    plan.assignments
                        |> List.map .personName
                        |> List.filterMap (personForName model)
                        |> List.filter (\p -> p.roleID == roleID)

                Nothing ->
                    []
    in
        List.length assignedPersons



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
        |> updateUIDialogAddPlansToggleNone
        |> updateUIDialogEditAssignmentsToggleNone


updateUIDialogKind : DialogKind -> Model -> Model
updateUIDialogKind newKind model =
    let
        ui_ =
            model.ui

        newUI =
            { ui_ | dialogKind = newKind }
    in
        { model | ui = newUI }


updateUIDialogAddPlansToggleNone : Model -> Model
updateUIDialogAddPlansToggleNone model =
    let
        ui_ =
            model.ui
    in
        { model | ui = { ui_ | dialogAddPlansSelected = Set.empty } }


updateUIDialogAddPlansToggleAll : Model -> Model
updateUIDialogAddPlansToggleAll model =
    let
        ui_ =
            model.ui

        allProjectNames =
            model.projects
                |> List.map (.name >> projectNameToString)
                |> Set.fromList

        alreadySelected =
            areDialogAddPlansAllSelected model
    in
        { model
            | ui =
                { ui_
                    | dialogAddPlansSelected =
                        if alreadySelected then
                            Set.empty
                        else
                            allProjectNames
                }
        }


updateUIDialogAddPlansToggle : String -> Model -> Model
updateUIDialogAddPlansToggle projectNameStr model =
    let
        ui_ =
            model.ui
    in
        { model
            | ui =
                { ui_
                    | dialogAddPlansSelected = toggle projectNameStr ui_.dialogAddPlansSelected
                }
        }


areDialogAddPlansAllSelected : Model -> Bool
areDialogAddPlansAllSelected model =
    Set.size model.ui.dialogAddPlansSelected == List.length model.projects


updateUIDialogEditAssignmentsToggleNone : Model -> Model
updateUIDialogEditAssignmentsToggleNone model =
    let
        ui_ =
            model.ui
    in
        { model | ui = { ui_ | dialogEditAssignmentsSelected = Set.empty } }



-- BUG: this assign all persons, even the ones that are not displayed


updateUIDialogEditAssignmentsToggleAll : Model -> Model
updateUIDialogEditAssignmentsToggleAll model =
    let
        ui_ =
            model.ui

        allPersonsNames =
            model.persons
                |> List.map .name
                |> Set.fromList

        alreadySelected =
            areDialogEditAssignmentsAllSelected model
    in
        { model
            | ui =
                { ui_
                    | dialogEditAssignmentsSelected =
                        if alreadySelected then
                            Set.empty
                        else
                            allPersonsNames
                }
        }


updateUIDialogEditAssignmentsToggle : String -> Model -> Model
updateUIDialogEditAssignmentsToggle personName model =
    let
        ui_ =
            model.ui
    in
        { model
            | ui =
                { ui_
                    | dialogEditAssignmentsSelected = toggle personName ui_.dialogEditAssignmentsSelected
                }
        }


areDialogEditAssignmentsAllSelected : Model -> Bool
areDialogEditAssignmentsAllSelected model =
    Set.size model.ui.dialogEditAssignmentsSelected == List.length model.persons



-- VIEW


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


tabs : List ( String, String, Model -> Html Msg )
tabs =
    [ ( "Cycles", "cycles", cyclesTab )
    , ( "Projects", "projects", projectsTab )
    , ( "Persons", "persons", personsTab )
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
                    , Options.onClick (AddPlans cycle.index)
                    , Dialog.openOn "click"
                    ]
                    [ text "Plan projects" ]
                ]
            ]


cycleColumnProjects : Model -> Cycle -> List (Html Msg)
cycleColumnProjects model cycle =
    plannedProjectNamesForCycle cycle.index model
        |> List.filterMap (\n -> projectForName model n)
        |> List.indexedMap (cycleColumnProjectCard model cycle.index)


cycleColumnProjectCard : Model -> CycleIndex -> Int -> Project -> Html Msg
cycleColumnProjectCard model cycleIdx projectIdx project =
    let
        assignments =
            roles
                |> List.indexedMap
                    (\i r ->
                        Button.render Mdl
                            [ 120, cycleIndexToInt cycleIdx, projectIdx, i ]
                            model.mdl
                            [ Button.ripple
                            , Options.onClick (EditAssignments cycleIdx project.name r.id)
                            , Dialog.openOn "click"
                            ]
                            [ div []
                                [ Icon.i <| r.icon
                                , text <| toString (countPersonsAssigned r.id cycleIdx project.name model)
                                ]
                            ]
                    )
    in
        Card.view
            [ cs "cycles--project-card"
            , css "width" "256px"
            , Elevation.e2
            ]
            [ Card.title
                [ css "flex-direction" "column" ]
                [ Card.head [] [ text <| toString (projectNameToString project.name) ]
                , Card.subhead [] [ text "No description for now" ]
                ]
            , Card.actions
                [ cs "cycles--project-card__assignments" ]
                [ div [] assignments ]
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


personsTab : Model -> Html Msg
personsTab model =
    div []
        [ addNewPersonButton model
        , personsTable model
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


personsTable : Model -> Html Msg
personsTable model =
    let
        roleSelect : Int -> Person -> Html Msg
        roleSelect pIndex person =
            Select.render Mdl
                [ 4, pIndex ]
                model.mdl
                [ Select.label "Role"
                , Select.floatingLabel
                , Select.ripple
                , Select.value <| roleReadForId person.roleID
                ]
                (allRoleIds
                    |> List.map
                        (\roleID ->
                            Select.item
                                [ Item.onSelect (UpdatePersonRole pIndex roleID)
                                ]
                                [ text <| roleReadForId roleID
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
        List.ul [ cs "persons" ]
            (model.persons
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

            DialogAddPlan cycleIdx ->
                let
                    cycle_ =
                        cycleForIndex cycleIdx model
                in
                    case cycle_ of
                        Nothing ->
                            dialogError "Unknown cycle" model

                        Just cycle ->
                            dialogAddPlan cycle model

            DialogEditAssignments cycleIdx projectName roleID ->
                let
                    cycle_ =
                        cycleForIndex cycleIdx model

                    project_ =
                        projectForName model projectName
                in
                    case ( cycle_, project_ ) of
                        ( Just cycle, Just project ) ->
                            dialogEditAssignments cycle project roleID model

                        _ ->
                            dialogError "Unknown cycle or project" model


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
                                        [ Options.onToggle UIDialogAddPlansToggleAll
                                        , Toggles.value (areDialogAddPlansAllSelected model)
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
                                            [ Table.selected |> Options.when (Set.member projectNameStr model.ui.dialogAddPlansSelected) ]
                                            [ Table.td []
                                                [ Toggles.checkbox Mdl
                                                    [ idx ]
                                                    model.mdl
                                                    [ Options.onToggle (UIDialogAddPlansToggle projectNameStr)
                                                    , Toggles.value <| Set.member projectNameStr model.ui.dialogAddPlansSelected
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
                        , Options.onClick (AddPlansFromDialog cycle.index)
                        ]
                        [ text "Plan" ]
                    , cancelButton
                    ]
                ]


dialogEditAssignments : Cycle -> Project -> RoleID -> Model -> Html Msg
dialogEditAssignments cycle project roleID model =
    let
        cycleIdxInt =
            cycleIndexToInt cycle.index

        projectNameStr =
            projectNameToString project.name

        personsWithRoleNames =
            personsWithRole roleID model
                |> List.map .name

        addablePersonNames : List String
        addablePersonNames =
            personNamesAvailableForCycle cycle.index model
                |> List.filter (\pn -> List.member pn personsWithRoleNames)

        dialogTitle =
            Dialog.title [] [ text "Assign persons" ]

        cancelButton =
            Button.render Mdl
                [ 901 ]
                model.mdl
                [ Dialog.closeOn "click"
                , Options.onClick UIDialogReset
                ]
                [ text "Cancel" ]

        noAddablePersonsDialog =
            Dialog.view []
                [ dialogTitle
                , Dialog.content []
                    [ Html.p [] [ text "All available persons have already been assigned on this cycle." ]
                    ]
                , Dialog.actions [] [ cancelButton ]
                ]
    in
        if List.isEmpty addablePersonNames then
            noAddablePersonsDialog
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
                                        [ Options.onToggle UIDialogEditAssignmentsToggleAll
                                        , Toggles.value (areDialogEditAssignmentsAllSelected model)
                                        ]
                                        []
                                    ]
                                , Table.th [] [ text "Projects" ]
                                ]
                            ]
                        , Table.tbody []
                            (addablePersonNames
                                |> List.indexedMap
                                    (\idx personName ->
                                        Table.tr
                                            [ Table.selected |> Options.when (Set.member personName model.ui.dialogEditAssignmentsSelected) ]
                                            [ Table.td []
                                                [ Toggles.checkbox Mdl
                                                    [ idx ]
                                                    model.mdl
                                                    [ Options.onToggle (UIDialogEditAssignmentsToggle personName)
                                                    , Toggles.value <| Set.member personName model.ui.dialogEditAssignmentsSelected
                                                    ]
                                                    []
                                                ]
                                            , Table.td [] [ text personName ]
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
                        , Options.onClick (UpdateAssignmentsFromDialog cycle.index project.name)
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
