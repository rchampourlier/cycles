module Main exposing (..)

import Array exposing (Array)
import Date exposing (Date)
import Date.Extra as Date
import Dict exposing (Dict)
import Html exposing (Html, text)
import Html.Attributes as A exposing (class, href)
import Http
import Json.Decode
import Json.Decode.Extra


--import Json.Encode

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
import Material.Options as Options exposing (cs, css, div, id, span)
import Material.Select as Select
import Material.Table as Table
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Tooltip as Tooltip
import Material.Typography as Typography
import Material.Layout as Layout
import Set exposing (Set)
import Task


toggle : comparable -> Set comparable -> Set comparable
toggle x set =
    if Set.member x set then
        Set.remove x set
    else
        Set.insert x set



-- MODEL


type alias Model =
    { state : State
    , mdl : Material.Model
    , ui : UIModel
    }


type alias State =
    { roles : List Role
    , statuses : List Status
    , cycles : List Cycle -- sorted by index, desc
    , persons : List Person
    , plans : List Plan
    , projects : List Project
    }



-- MODEL:CYCLE


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



-- MODEL:PROJECT


type alias Project =
    { name : ProjectName
    }


type ProjectName
    = ProjectName String



-- MODEL:PLAN


type alias Plan =
    { id : PlanID
    , assignments : List Assignment
    , statusRecords : List StatusRecord
    }


type alias PlanID =
    { cycleIndex : CycleIndex
    , projectName : ProjectName
    }



-- MODEL:PERSON


type alias Person =
    { name : String
    , roleID : RoleID
    }



-- MODEL:ROLE


type alias Role =
    { id : RoleID
    , read : String
    , icon : String
    }


type RoleID
    = RoleID String



-- MODEL:ASSIGNMENT


type alias Assignment =
    { personName : String
    }



-- MODEL:STATUS


type alias StatusRecord =
    { id : StatusID
    , description : String
    , date : Date
    }


type StatusID
    = StatusID String


type alias Status =
    { id : StatusID
    , read : String
    , icon : String
    }



-- MODEL:UI


type alias UIModel =
    { selectedTab : Int
    , dialog : UI_DialogModel
    }



-- MODEL:UI:DIALOG
{- MDL key references

   # 1xx serie: cycles tab
     110: cycle column, plan project button
     111: cycle column > header > assignment box > buttons
     112: cycle column > header > assignment box > tooltips
     120: cycle column > plan > actions > assignments
     130: cycle column > plan > actions > add status button

   # 3xx serie: persons tab
     310: person row delete button

   # 9xx serie: dialog
     901: cancel button
     902: action button
     911: dialog addStatusRecord -> description field
     912: dialog addStatusRecord -> date field
     913: dialog addStatusRecord -> status select

-}


type alias UI_DialogModel =
    { kind : DialogKind
    , addProject_ProjectNameField_Value : String
    , addPlans_Projects_SelectedProjectNames : Set String -- project names as String
    , editAssignments_Persons_PersonNameSelected : List ( String, Bool ) -- personName, isSelected
    , addStatusRecord_Status_SelectedID : StatusID
    , addStatusRecord_DateField_Value : String
    , addStatusRecord_DescriptionField_Value : String
    , addStatusRecord_DescriptionField_Touched : Bool
    , addStatusRecord_ParsedDate : Result String Date
    }


type DialogKind
    = DialogNone
    | DialogAddProject
    | DialogAddPlans CycleIndex
    | DialogEditAssignments PlanID RoleID
    | DialogAddStatusRecord PlanID



-- MODEL:DEFAULT


defaultModel : Model
defaultModel =
    { state = defaultState
    , mdl = Material.model
    , ui = defaultUIModel Nothing
    }


defaultState : State
defaultState =
    { roles = []
    , statuses = []
    , cycles = []
    , persons = []
    , plans = []
    , projects = []
    }


defaultUIModel : Maybe Date -> UIModel
defaultUIModel date =
    { selectedTab = 0
    , dialog = defaultUIDialogModel
    }


defaultUIDialogModel : UI_DialogModel
defaultUIDialogModel =
    { kind = DialogNone
    , addProject_ProjectNameField_Value = ""
    , addPlans_Projects_SelectedProjectNames = Set.empty
    , editAssignments_Persons_PersonNameSelected = []
    , addStatusRecord_Status_SelectedID = StatusID "on-track"
    , addStatusRecord_DateField_Value = ""
    , addStatusRecord_DescriptionField_Value = "Everything's ok!"
    , addStatusRecord_DescriptionField_Touched = False
    , addStatusRecord_ParsedDate = Err "Date not initialized yet!"
    }


dateAsString : Maybe Date -> String
dateAsString maybeDate =
    case maybeDate of
        Nothing ->
            ""

        Just date ->
            [ Date.year >> toString, Date.month >> monthAsString, Date.day >> toString ]
                |> List.map (\f -> f date)
                |> List.intersperse "-"
                |> List.foldr (++) ""


defaultModelForDev : Model
defaultModelForDev =
    -- A default model to ease development
    let
        roles =
            [ { id = RoleID "product_manager", read = "Product Manager", icon = "directions" }
            , { id = RoleID "frontend_developer", read = "Frontend Developer", icon = "palette" }
            , { id = RoleID "backend_developer", read = "Backend Developer", icon = "settings" }
            ]

        statuses =
            [ { id = StatusID "on-track", read = "On track", icon = "thumb_up" }
            , { id = StatusID "at-risk", read = "At risk", icon = "warning" }
            , { id = StatusID "critical", read = "Critical", icon = "error" }
            ]

        cycles =
            [ { index = CycleIndex 2 }
            , { index = CycleIndex 1 }
            ]

        persons =
            [ { name = "Raphaëlle", roleID = RoleID "product_manager" }
            , { name = "Christophe", roleID = RoleID "product_manager" }
            , { name = "Ludovic", roleID = RoleID "backend_developer" }
            ]

        defaultDate =
            Date.fromString "2017-12-24"

        defaultMaybeDate =
            case defaultDate of
                Ok date ->
                    Just date

                Err _ ->
                    Nothing

        statusRecords =
            case defaultDate of
                Ok date ->
                    [ { id = StatusID "on-track", description = "Milestone #1 delivered 1 day earlier than expected", date = date } ]

                Err _ ->
                    []

        plans =
            [ { id = (PlanID (CycleIndex 1) (ProjectName "Company Pages #1"))
              , assignments = [ { personName = "Raphaëlle" } ]
              , statusRecords = statusRecords
              }
            ]

        projects =
            [ { name = ProjectName "Company Pages #1" }, { name = ProjectName "Event Newsletter" } ]

        state =
            { roles = roles
            , statuses = statuses
            , cycles = cycles
            , persons = persons
            , plans = plans
            , projects = projects
            }
    in
        { state = state
        , mdl = Material.model
        , ui = defaultUIModel defaultMaybeDate
        }


nextIndex : Model -> CycleIndex
nextIndex model =
    let
        cycles =
            model.state.cycles
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
    = SetDate Date
      -- cycles
    | CreateCycle
      -- projects
    | AddProject
    | CreateProjectFromDialog
      -- persons
    | AddNewPerson
    | UpdatePersonName Int String -- personIndex newValue
    | LeavePersonNameEdition Int -- personIndex
    | UpdatePersonRole Int RoleID -- personIndex...
    | DeletePerson Int -- personIndex
      -- plan
    | AddPlans CycleIndex
    | AddPlansFromDialog CycleIndex
      -- assignments
    | EditAssignments PlanID RoleID
    | UpdateAssignmentsFromDialog PlanID RoleID
      -- status records
    | AddStatusRecord PlanID
    | CreateStatusRecordFromDialog PlanID
      -- commands
    | FetchLatestStateDone (Result Http.Error State)
      -- ui
    | UI_SelectTab Int
    | UI_Dialog_AddProjectUpdateFieldName String
    | UI_Dialog_AddPlans_ToggleAll
    | UI_Dialog_AddPlans_Toggle String
    | UI_Dialog_EditAssignmentsToggleAll
    | UI_Dialog_EditAssignmentsToggle String -- personName
    | UI_Dialog_AddStatusRecord_PrepareAndOpen PlanID Date
    | UI_Dialog_AddStatusRecord_SelectStatus StatusID
    | UI_Dialog_AddStatusRecord_UpdateDate PlanID String
    | UI_Dialog_AddStatusRecord_UpdateDescription PlanID String
    | UI_Dialog_Reset
      -- mdl
    | Mdl (Material.Msg Msg)


monthAsString : Date.Month -> String
monthAsString month =
    case month of
        Date.Jan ->
            "01"

        Date.Feb ->
            "02"

        Date.Mar ->
            "03"

        Date.Apr ->
            "04"

        Date.May ->
            "05"

        Date.Jun ->
            "06"

        Date.Jul ->
            "07"

        Date.Aug ->
            "08"

        Date.Sep ->
            "09"

        Date.Oct ->
            "10"

        Date.Nov ->
            "11"

        Date.Dec ->
            "12"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ui_ =
            model.ui
    in
        case msg of
            -- Tasks
            SetDate date ->
                let
                    ui_ =
                        model.ui
                in
                    ( { model | ui = defaultUIModel (Just date) }, Cmd.none )

            -- Events
            CreateCycle ->
                let
                    state_ =
                        model.state

                    newCycle =
                        { index = nextIndex model }

                    newCycles =
                        newCycle :: model.state.cycles

                    newState =
                        { state_ | cycles = newCycles }
                in
                    ( { model | state = newState }, Cmd.none )

            -- PROJECTS
            AddProject ->
                prepareForDialog DialogAddProject model

            CreateProjectFromDialog ->
                let
                    newProject =
                        { name = ProjectName model.ui.dialog.addProject_ProjectNameField_Value }
                in
                    ( model
                        |> addProject newProject
                        |> update_UI_Dialog_Reset
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
            AddPlans cycleIdx ->
                model |> prepareForDialog (DialogAddPlans cycleIdx)

            AddPlansFromDialog cycleIdx ->
                ( model
                    |> addPlansFromDialog cycleIdx
                    |> update_UI_Dialog_Reset
                , Cmd.none
                )

            -- ASSIGNMENTS
            EditAssignments planID roleID ->
                model |> prepareForDialog (DialogEditAssignments planID roleID)

            UpdateAssignmentsFromDialog planID roleID ->
                ( model
                    |> updateAssignmentsFromDialog planID roleID
                    |> update_UI_Dialog_Reset
                , Cmd.none
                )

            -- STATUS RECORD
            AddStatusRecord planID ->
                ( model, Task.perform (UI_Dialog_AddStatusRecord_PrepareAndOpen planID) Date.now )

            CreateStatusRecordFromDialog planID ->
                ( model
                    |> updateStatusRecordsFromDialog planID
                    |> update_UI_Dialog_Reset
                , Cmd.none
                )

            -- COMMANDS
            FetchLatestStateDone res ->
                -- TODO
                ( model, Cmd.none )

            -- UI
            UI_SelectTab k ->
                ( { model | ui = { ui_ | selectedTab = k } }, Cmd.none )

            UI_Dialog_AddProjectUpdateFieldName name ->
                let
                    dialog_ =
                        model.ui.dialog
                in
                    ( model |> update_UI_Dialog { dialog_ | addProject_ProjectNameField_Value = name }, Cmd.none )

            UI_Dialog_AddPlans_ToggleAll ->
                ( model |> update_UI_Dialog_AddPlans_ToggleAll, Cmd.none )

            UI_Dialog_AddPlans_Toggle projectNameStr ->
                ( model |> update_UI_Dialog_AddPlans_Toggle projectNameStr, Cmd.none )

            UI_Dialog_EditAssignmentsToggleAll ->
                ( model |> update_UI_Dialog_EditAssignmentsToggleAll, Cmd.none )

            UI_Dialog_EditAssignmentsToggle personName ->
                ( model |> update_UI_Dialog_EditAssignmentsToggle personName, Cmd.none )

            UI_Dialog_AddStatusRecord_PrepareAndOpen planID date ->
                prepareForDialogAddStatusRecord planID date model

            UI_Dialog_AddStatusRecord_SelectStatus newStatusID ->
                ( model |> update_UI_Dialog_AddStatusRecord_SelectedStatusID newStatusID, Cmd.none )

            UI_Dialog_AddStatusRecord_UpdateDate planID newDate ->
                ( model |> update_UI_Dialog_AddStatusRecord_UpdateDate newDate, Cmd.none )

            UI_Dialog_AddStatusRecord_UpdateDescription planID newDescription ->
                ( model |> update_UI_Dialog_AddStatusRecord_Description newDescription, Cmd.none )

            UI_Dialog_Reset ->
                ( model |> update_UI_Dialog_Reset, Cmd.none )

            Mdl msg_ ->
                Material.update Mdl msg_ model



-- UPDATE:CYCLES


cycleForIndex : CycleIndex -> Model -> Maybe Cycle
cycleForIndex cycleIdx model =
    model.state.cycles
        |> List.filter (\c -> c.index == cycleIdx)
        |> List.head


updateCycleAtIndex : CycleIndex -> Cycle -> Model -> Model
updateCycleAtIndex cycleIdx newCycle model =
    let
        state_ =
            model.state

        newCycles =
            state_.cycles
                |> List.map
                    (\c ->
                        if c.index == cycleIdx then
                            newCycle
                        else
                            c
                    )

        newState =
            { state_ | cycles = newCycles }
    in
        { model | state = newState }



-- UPDATE:PERSONS


updatePersonRole : Int -> RoleID -> Model -> Model
updatePersonRole personIndex newRoleId model =
    case roleForID newRoleId model of
        Nothing ->
            model

        Just role ->
            let
                state_ =
                    model.state

                updatePerson index person =
                    if index == personIndex then
                        { person | roleID = role.id }
                    else
                        person

                newState =
                    { state_ | persons = state_.persons |> List.indexedMap updatePerson }
            in
                { model | state = newState }


updatePersonName : Int -> String -> Model -> Model
updatePersonName personIndex newName model =
    let
        state_ =
            model.state

        updatePerson index person =
            if index == personIndex then
                { person | name = newName }
            else
                person

        newState =
            { state_ | persons = state_.persons |> List.indexedMap updatePerson }
    in
        { model | state = newState }


isPersonNameAlreadyTakenAtIndex : Int -> Model -> Bool
isPersonNameAlreadyTakenAtIndex pIndex model =
    case personAtIndex pIndex model of
        Nothing ->
            False

        Just personAtIndex_ ->
            (model.state.persons
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
    model.state.persons
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
        state_ =
            model.state

        persons =
            state_.persons

        newState =
            { state_ | persons = (List.take pIndex persons) ++ (List.drop (pIndex + 1) persons) }
    in
        { model | state = newState }


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
    let
        state_ =
            model.state

        newState =
            { state_ | persons = state_.persons |> List.sortBy .name }
    in
        { model | state = newState }


addNewPerson : Model -> Model
addNewPerson model =
    let
        state_ =
            model.state

        newPerson =
            { name = "New person", roleID = RoleID "product_manager" }

        newState =
            { state_ | persons = (newPerson :: state_.persons) }

        newModel =
            { model | state = newState }
    in
        newModel |> removeInvalidPersonAtIndex 0


personForName : Model -> String -> Maybe Person
personForName model name =
    model.state.persons
        |> List.filter (\p -> p.name == name)
        |> List.head


personNamesAvailableForCycle : CycleIndex -> Model -> List String
personNamesAvailableForCycle cycleIdx model =
    let
        personNames =
            model.state.persons |> List.map .name

        assignedPersonNames =
            plansForCycleIndex cycleIdx model
                |> List.concatMap .assignments
                |> List.map .personName
    in
        personNames
            |> List.filter (\pn -> not (List.member pn assignedPersonNames))


personsForRole : Model -> RoleID -> List Person
personsForRole model roleID =
    model.state.persons
        |> List.filter (\p -> p.roleID == roleID)


roleForID : RoleID -> Model -> Maybe Role
roleForID id model =
    model.state.roles
        |> List.filter (\r -> r.id == id)
        |> List.head


roleReadForID : RoleID -> Model -> String
roleReadForID id model =
    case roleForID id model of
        Nothing ->
            "Unexpected role ID"

        Just role ->
            role.read



-- UPDATE:PLANS


cycleIndexForPlanID : PlanID -> CycleIndex
cycleIndexForPlanID planID =
    planID.cycleIndex


projectNameForPlanID : PlanID -> ProjectName
projectNameForPlanID planID =
    planID.projectName


plansForCycle : CycleIndex -> Model -> List Plan
plansForCycle cycleIdx model =
    model.state.plans
        |> List.filter (\p -> cycleIndexForPlanID p.id == cycleIdx)


plannedProjectNamesForCycle : CycleIndex -> Model -> List ProjectName
plannedProjectNamesForCycle cycleIdx model =
    plansForCycle cycleIdx model
        |> List.map .id
        |> List.map projectNameForPlanID


addPlansFromDialog : CycleIndex -> Model -> Model
addPlansFromDialog cycleIdx model =
    let
        cycle_ =
            cycleForIndex cycleIdx model

        selectedProjects : List ProjectName
        selectedProjects =
            model.ui.dialog.addPlans_Projects_SelectedProjectNames
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
                            |> List.map (\pn -> { id = (PlanID cycleIdx pn), assignments = [], statusRecords = [] })
                in
                    model |> addPlans newPlans


addPlans : List Plan -> Model -> Model
addPlans plans model =
    let
        state_ =
            model.state

        newState =
            { state_ | plans = state_.plans ++ plans }
    in
        { model | state = newState }


updatePlan : Plan -> Model -> Model
updatePlan newPlan model =
    let
        state_ =
            model.state

        newPlans =
            state_.plans
                |> List.map
                    (\p ->
                        if (samePlans p newPlan) then
                            newPlan
                        else
                            p
                    )

        newState =
            { state_ | plans = newPlans }
    in
        { model | state = newState }


plansForCycleIndex : CycleIndex -> Model -> List Plan
plansForCycleIndex cycleIdx model =
    model.state.plans
        |> List.filter (\p -> cycleIndexForPlanID p.id == cycleIdx)


planForID : PlanID -> Model -> Maybe Plan
planForID planID model =
    model.state.plans
        |> List.filter (\p -> p.id == planID)
        |> List.head


samePlans : Plan -> Plan -> Bool
samePlans planA planB =
    planA.id == planB.id



-- UPDATE:PROJECTS


addProject : Project -> Model -> Model
addProject project model =
    let
        state_ =
            model.state

        newState =
            { state_ | projects = (project :: state_.projects) }
    in
        { model | state = newState }


projectNameToString : ProjectName -> String
projectNameToString projectNameStr =
    case projectNameStr of
        ProjectName s ->
            s


projectForName : Model -> ProjectName -> Maybe Project
projectForName model projectName =
    model.state.projects
        |> List.filter (\p -> p.name == projectName)
        |> List.head



-- UPDATE:ASSIGNMENTS


updateAssignmentsFromDialog : PlanID -> RoleID -> Model -> Model
updateAssignmentsFromDialog planID roleID model =
    let
        selectedPersonNames : List String
        selectedPersonNames =
            model.ui.dialog.editAssignments_Persons_PersonNameSelected
                |> List.filter Tuple.second
                |> List.map Tuple.first
    in
        case planForID planID model of
            Nothing ->
                model

            Just plan ->
                let
                    allPersonNames =
                        model.state.persons |> List.map .name

                    rolePersonNames =
                        personsForRole model roleID |> List.map .name

                    otherRolesAssignments =
                        plan.assignments
                            |> List.filter (\a -> not (List.member a.personName rolePersonNames))

                    newRoleAssignments =
                        selectedPersonNames
                            |> List.map (\pn -> { personName = pn })

                    newPlan =
                        { plan | assignments = otherRolesAssignments ++ newRoleAssignments }
                in
                    model |> updatePlan newPlan


countPersonsAssignedForRoleAndCycle : RoleID -> CycleIndex -> Model -> Int
countPersonsAssignedForRoleAndCycle roleID cycleIdx model =
    plansForCycleIndex cycleIdx model
        |> List.map (\p -> countPersonsAssignedForRoleAndPlan roleID p.id model)
        |> List.sum


countPersonsAssignedForRoleAndPlan : RoleID -> PlanID -> Model -> Int
countPersonsAssignedForRoleAndPlan roleID planID model =
    personsAssignedForPlan model planID
        |> List.filter (\p -> p.roleID == roleID)
        |> List.length


personsAssignedForPlan : Model -> PlanID -> List Person
personsAssignedForPlan model planID =
    case planForID planID model of
        Just plan ->
            plan.assignments
                |> List.map .personName
                |> List.filterMap (personForName model)

        Nothing ->
            []


personsAssignedForCycle : Model -> CycleIndex -> List Person
personsAssignedForCycle model cycleIdx =
    model.state.plans
        |> List.filter (\p -> cycleIndexForPlanID p.id == cycleIdx)
        |> List.map .id
        |> List.concatMap (personsAssignedForPlan model)


personsNotAssignedForCycle : Model -> CycleIndex -> List Person
personsNotAssignedForCycle model cycleIdx =
    let
        assignedPersons =
            personsAssignedForCycle model cycleIdx
    in
        model.state.persons
            |> List.filter (\p -> not (List.member p assignedPersons))



-- UPDATE:STATUS_RECORD


updateStatusRecordsFromDialog : PlanID -> Model -> Model
updateStatusRecordsFromDialog planID model =
    let
        plan_ =
            planForID planID model

        parsedDate_ =
            model.ui.dialog.addStatusRecord_ParsedDate

        newStatusRecord date =
            { id = model.ui.dialog.addStatusRecord_Status_SelectedID
            , date = date
            , description = model.ui.dialog.addStatusRecord_DescriptionField_Value
            }

        newPlan : Plan -> Date -> Plan
        newPlan plan date =
            { plan | statusRecords = plan.statusRecords ++ [ newStatusRecord date ] }
    in
        -- If plan is not found (plan_ == Nothing) or date can't be parsed
        -- (parsedDate of Err..), then abort and return unchanged model.
        case ( plan_, parsedDate_ ) of
            ( Just plan, Ok parsedDate ) ->
                model |> updatePlan (newPlan plan parsedDate)

            _ ->
                model


statusReadForID : StatusID -> Model -> String
statusReadForID id model =
    case statusForID id model of
        Nothing ->
            "Unexpected status ID"

        Just status ->
            status.read


statusForID : StatusID -> Model -> Maybe Status
statusForID id model =
    model.state.statuses
        |> List.filter (\s -> s.id == id)
        |> List.head



-- UPDATE:UI:DIALOG


prepareForDialog : DialogKind -> Model -> ( Model, Cmd Msg )
prepareForDialog kind model =
    let
        model_ =
            update_UI_DialogKind kind model
    in
        case kind of
            DialogEditAssignments planID roleID ->
                prepareForDialogEditAssignments planID roleID model_

            _ ->
                ( model_, Cmd.none )


prepareForDialogEditAssignments : PlanID -> RoleID -> Model -> ( Model, Cmd Msg )
prepareForDialogEditAssignments planID roleID model =
    {- This dialog requires to identify the list of persons that may be
       assigned to the project. This list will be displayed in the dialog
       and the selection will serve to update the assignments.
    -}
    let
        ui_ =
            model.ui

        dialog_ =
            model.ui.dialog

        plan_ =
            planForID planID model

        availablePersonsNames =
            personNamesAvailableForCycle (cycleIndexForPlanID planID) model

        rolePersonsNames =
            personsForRole model roleID
                |> List.map .name

        newPersons : List ( String, Bool )
        newPersons =
            let
                assignedPersons =
                    case plan_ of
                        Nothing ->
                            []

                        Just plan ->
                            plan.assignments
                                |> List.map (\a -> ( a.personName, True ))

                availablePersons =
                    availablePersonsNames
                        |> List.map (\pn -> ( pn, False ))
            in
                (assignedPersons ++ availablePersons)
                    |> List.filter (\( pn, _ ) -> List.member pn rolePersonsNames)

        newDialog =
            { dialog_ | editAssignments_Persons_PersonNameSelected = newPersons }

        newUI =
            { ui_ | dialog = newDialog }
    in
        ( model |> update_UI_Dialog newDialog, Cmd.none )


prepareForDialogAddStatusRecord : PlanID -> Date -> Model -> ( Model, Cmd Msg )
prepareForDialogAddStatusRecord planID date model =
    let
        dialog_ =
            model.ui.dialog

        newDialog =
            { dialog_
                | kind = DialogAddStatusRecord planID
                , addStatusRecord_DateField_Value = dateAsString (Just date)
                , addStatusRecord_ParsedDate = Ok date
            }
    in
        ( model |> update_UI_Dialog newDialog, Cmd.none )


update_UI_Dialog : UI_DialogModel -> Model -> Model
update_UI_Dialog newDialog model =
    let
        ui_ =
            model.ui

        newUI =
            { ui_ | dialog = newDialog }
    in
        { model | ui = newUI }


update_UI_Dialog_Reset : Model -> Model
update_UI_Dialog_Reset model =
    let
        ui_ =
            model.ui

        newUI =
            { ui_ | dialog = defaultUIDialogModel }
    in
        { model | ui = newUI }


update_UI_DialogKind : DialogKind -> Model -> Model
update_UI_DialogKind kind model =
    let
        dialog_ =
            model.ui.dialog
    in
        model |> update_UI_Dialog { dialog_ | kind = kind }



-- UPDATE:UI:DIALOG:PLANS


update_UI_Dialog_AddPlans_Selected : Model -> Set String -> Model
update_UI_Dialog_AddPlans_Selected model newPlansSelected =
    let
        dialog_ =
            model.ui.dialog
    in
        model |> update_UI_Dialog { dialog_ | addPlans_Projects_SelectedProjectNames = newPlansSelected }


update_UI_Dialog_AddPlans_ToggleNone : Model -> Model
update_UI_Dialog_AddPlans_ToggleNone model =
    update_UI_Dialog_AddPlans_Selected model Set.empty


update_UI_Dialog_AddPlans_ToggleAll : Model -> Model
update_UI_Dialog_AddPlans_ToggleAll model =
    let
        allProjectNames =
            model.state.projects
                |> List.map (.name >> projectNameToString)
                |> Set.fromList

        newPlansSelected =
            if areDialogAddPlansAllSelected model then
                Set.empty
            else
                allProjectNames
    in
        update_UI_Dialog_AddPlans_Selected model newPlansSelected


update_UI_Dialog_AddPlans_Toggle : String -> Model -> Model
update_UI_Dialog_AddPlans_Toggle projectNameStr model =
    let
        dialog_ =
            model.ui.dialog

        newPlansSelected =
            toggle projectNameStr dialog_.addPlans_Projects_SelectedProjectNames
    in
        update_UI_Dialog_AddPlans_Selected model newPlansSelected


areDialogAddPlansAllSelected : Model -> Bool
areDialogAddPlansAllSelected model =
    Set.size model.ui.dialog.addPlans_Projects_SelectedProjectNames == List.length model.state.projects



-- UPDATE:UI:DIALOG:ASSIGNMENTS


update_UI_Dialog_EditAssignmentsReset : Model -> Model
update_UI_Dialog_EditAssignmentsReset model =
    let
        ui_ =
            model.ui

        dialog_ =
            ui_.dialog

        newDialogEditAssignmentsPersons =
            ui_.dialog.editAssignments_Persons_PersonNameSelected
                |> List.map (\( pn, _ ) -> ( pn, False ))

        newDialog =
            { dialog_ | editAssignments_Persons_PersonNameSelected = newDialogEditAssignmentsPersons }

        newUI =
            { ui_ | dialog = newDialog }
    in
        { model | ui = newUI }


update_UI_Dialog_EditAssignmentsToggleAll : Model -> Model
update_UI_Dialog_EditAssignmentsToggleAll model =
    let
        ui_ =
            model.ui

        dialog_ =
            ui_.dialog

        alreadySelected =
            List.all Tuple.second ui_.dialog.editAssignments_Persons_PersonNameSelected

        newDialogEditAssignmentsPersons =
            dialog_.editAssignments_Persons_PersonNameSelected
                |> List.map (\( pn, _ ) -> ( pn, not alreadySelected ))

        newDialog =
            { dialog_ | editAssignments_Persons_PersonNameSelected = newDialogEditAssignmentsPersons }

        newUI =
            { ui_ | dialog = newDialog }
    in
        { model | ui = newUI }


update_UI_Dialog_EditAssignmentsToggle : String -> Model -> Model
update_UI_Dialog_EditAssignmentsToggle personName model =
    let
        ui_ =
            model.ui

        dialog_ =
            ui_.dialog

        newDialogEditAssignmentsPersons =
            ui_.dialog.editAssignments_Persons_PersonNameSelected
                |> List.map
                    (\( pn, selected ) ->
                        if pn == personName then
                            ( pn, not selected )
                        else
                            ( pn, selected )
                    )

        newDialog =
            { dialog_ | editAssignments_Persons_PersonNameSelected = newDialogEditAssignmentsPersons }

        newUi =
            { ui_ | dialog = newDialog }
    in
        { model | ui = newUi }


areDialogEditAssignmentsAllSelected : Model -> Bool
areDialogEditAssignmentsAllSelected model =
    model.ui.dialog.editAssignments_Persons_PersonNameSelected
        |> List.all Tuple.second



-- UPDATE:UI:DIALOG:ADD_STATUS_RECORD


update_UI_Dialog_AddStatusRecord_SelectedStatusID : StatusID -> Model -> Model
update_UI_Dialog_AddStatusRecord_SelectedStatusID newStatusID model =
    let
        dialog_ =
            model.ui.dialog

        newDialog =
            { dialog_ | addStatusRecord_Status_SelectedID = newStatusID }
    in
        model |> update_UI_Dialog newDialog


update_UI_Dialog_AddStatusRecord_UpdateDate : String -> Model -> Model
update_UI_Dialog_AddStatusRecord_UpdateDate newDate model =
    let
        dialog_ =
            model.ui.dialog

        newDialog =
            { dialog_
                | addStatusRecord_DateField_Value = newDate
                , addStatusRecord_ParsedDate = Date.fromString newDate
            }
    in
        model |> update_UI_Dialog newDialog


update_UI_Dialog_AddStatusRecord_Description : String -> Model -> Model
update_UI_Dialog_AddStatusRecord_Description newDescription model =
    let
        dialog_ =
            model.ui.dialog

        newDialog =
            { dialog_
                | addStatusRecord_DescriptionField_Value = newDescription
                , addStatusRecord_DescriptionField_Touched = True
            }
    in
        model |> update_UI_Dialog newDialog



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
            , Layout.onSelectTab UI_SelectTab
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



-- VIEW:CYCLE


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
        (model.state.cycles
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
        nonAssignments =
            personsNotAssignedForCycle model cycle.index
                |> List.map (\p -> { personName = p.name })
    in
        Card.view
            [ cs "cycles--column--header"
            , css "width" "256px"
            , Elevation.e2
            ]
            [ Card.title []
                [ Card.head []
                    [ text <| toString cycle.index
                    ]
                ]
            , Card.text []
                [ assignmentsBox
                    [ 112, cycleIndexToInt cycle.index ]
                    model
                    Nothing
                    nonAssignments
                    "Not assigned: "
                ]
            , Card.actions
                [ Card.border

                -- Modify flexbox to accomodate small text in action block
                , css "display" "flex"
                , css "justify-content" "space-between"
                , css "align-items" "center"
                , css "padding" "8px 16px 8px 16px"
                ]
                [ Button.render Mdl
                    [ 110, cycleIndexToInt cycle.index ]
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
    plansForCycle cycle.index model
        |> List.indexedMap (cycleColumnPlanCard model)


cycleColumnPlanCard : Model -> Int -> Plan -> Html Msg
cycleColumnPlanCard model i plan =
    let
        cycleIdx =
            cycleIndexForPlanID plan.id

        project_ =
            projectForName model (projectNameForPlanID plan.id)

        latestStatusRecord_ =
            plan.statusRecords
                |> List.sortWith (\srA srB -> Date.compare srB.date srA.date)
                |> List.head

        latestStatusText =
            case latestStatusRecord_ of
                Nothing ->
                    div [] [ text <| "No status yet" ]

                Just sr ->
                    div [] [ text <| sr.description ]
    in
        case project_ of
            Nothing ->
                div [] []

            Just project ->
                Card.view
                    [ cs "cycles--project-card"
                    , css "width" "256px"
                    , Elevation.e2
                    ]
                    [ Card.title
                        [ css "flex-direction" "column" ]
                        [ Card.head [] [ text <| toString (projectNameToString project.name) ]
                        ]
                    , Card.text []
                        [ latestStatusText ]
                    , Card.actions
                        [ cs "cycles--project-card__assignments" ]
                        [ div []
                            [ assignmentsBox
                                [ 120, cycleIndexToInt cycleIdx, i ]
                                model
                                (Just (EditAssignments (PlanID cycleIdx project.name)))
                                plan.assignments
                                "Assigned: "
                            , Button.render Mdl
                                [ 130, cycleIndexToInt cycleIdx, i ]
                                model.mdl
                                [ Button.ripple
                                , Options.onClick (AddStatusRecord plan.id)
                                , Dialog.openOn "click"
                                ]
                                [ div [] [ text "Record status" ] ]
                            ]
                        ]
                    ]


assignmentsBox : List Int -> Model -> Maybe (RoleID -> Msg) -> List Assignment -> String -> Html Msg
assignmentsBox keyPrefix model buttonAction assignments tooltipTextPrefix =
    let
        tooltipAttachment i =
            Tooltip.attach Mdl (keyPrefix ++ [ i ])

        tooltipRender i personNames =
            Tooltip.render Mdl
                (keyPrefix ++ [ i ])
                model.mdl
                [ Tooltip.right ]
                [ text <| tooltipTextPrefix ++ List.foldl (++) "" (List.intersperse ", " personNames) ]

        itemElement : Int -> Role -> List String -> List (Html Msg)
        itemElement i role personNames =
            case buttonAction of
                Nothing ->
                    [ span
                        [ tooltipAttachment i ]
                        [ Icon.i role.icon
                        , text <| toString (List.length personNames)
                        ]
                    , tooltipRender i personNames
                    ]

                Just action ->
                    [ Button.render Mdl
                        (keyPrefix ++ [ i ])
                        model.mdl
                        [ Button.ripple
                        , Options.onClick (action role.id)
                        , Dialog.openOn "click"
                        , tooltipAttachment i
                        ]
                        [ div []
                            [ Icon.i role.icon
                            , text <| toString (List.length personNames)
                            ]
                        ]
                    , tooltipRender i personNames
                    ]
    in
        div []
            (model.state.roles
                |> List.indexedMap
                    (\i role ->
                        let
                            personNames =
                                assignments
                                    |> List.filterMap (\a -> personForName model a.personName)
                                    |> List.filter (\p -> p.roleID == role.id)
                                    |> List.map .name
                        in
                            itemElement i role personNames
                    )
                |> List.foldr (++) []
            )



-- VIEW:PROJECTS


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
            [ model.state.projects
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



-- VIEW:PERSONS


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
                , Select.value <| roleReadForID person.roleID model
                ]
                (model.state.roles
                    |> List.map .id
                    |> List.map
                        (\roleID ->
                            Select.item
                                [ Item.onSelect (UpdatePersonRole pIndex roleID)
                                ]
                                [ text <| roleReadForID roleID model
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
            (model.state.persons
                |> List.indexedMap personRow
            )



-- VIEW:UI:DIALOG


dialog : Model -> Html Msg
dialog model =
    let
        dialogKind =
            model.ui.dialog.kind
    in
        case dialogKind of
            DialogNone ->
                dialogError "Unexpected dialog" model

            DialogAddProject ->
                dialogAddProject model

            DialogAddPlans cycleIdx ->
                let
                    cycle_ =
                        cycleForIndex cycleIdx model
                in
                    case cycle_ of
                        Nothing ->
                            dialogError "Unknown cycle" model

                        Just cycle ->
                            dialogAddPlan cycle model

            DialogEditAssignments planID roleID ->
                dialogEditAssignments planID roleID model

            DialogAddStatusRecord planID ->
                view_Dialog_AddStatusRecord planID model


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



-- VIEW:UI:DIALOG:PROJECT


dialogAddProject : Model -> Html Msg
dialogAddProject model =
    Dialog.view
        []
        [ Dialog.title [] [ text "Add project" ]
        , Dialog.content []
            [ Textfield.render Mdl
                [ 6 ]
                model.mdl
                [ Options.onInput UI_Dialog_AddProjectUpdateFieldName
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
                , Options.onClick UI_Dialog_Reset
                ]
                [ text "Cancel" ]
            ]
        ]



-- VIEW:UI:DIALOG:PLAN


dialogAddPlan : Cycle -> Model -> Html Msg
dialogAddPlan cycle model =
    let
        cycleIdxInt =
            cycleIndexToInt cycle.index

        addableProjectNameStrings : List String
        addableProjectNameStrings =
            model.state.projects
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
                , Options.onClick UI_Dialog_Reset
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
                                        [ Options.onToggle UI_Dialog_AddPlans_ToggleAll
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
                                            [ Table.selected |> Options.when (Set.member projectNameStr model.ui.dialog.addPlans_Projects_SelectedProjectNames) ]
                                            [ Table.td []
                                                [ Toggles.checkbox Mdl
                                                    [ idx ]
                                                    model.mdl
                                                    [ Options.onToggle (UI_Dialog_AddPlans_Toggle projectNameStr)
                                                    , Toggles.value <| Set.member projectNameStr model.ui.dialog.addPlans_Projects_SelectedProjectNames
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



-- VIEW:UI:DIALOG:ASSIGNMENTS


dialogEditAssignments : PlanID -> RoleID -> Model -> Html Msg
dialogEditAssignments planID roleID model =
    let
        personsForRoleNames =
            personsForRole model roleID
                |> List.map .name

        addablePersonNames : List String
        addablePersonNames =
            personNamesAvailableForCycle (cycleIndexForPlanID planID) model
                |> List.filter (\pn -> List.member pn personsForRoleNames)

        dialogTitle =
            Dialog.title [] [ text "Assign persons" ]

        cancelButton =
            Button.render Mdl
                [ 901 ]
                model.mdl
                [ Dialog.closeOn "click"
                , Options.onClick UI_Dialog_Reset
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
        if List.isEmpty model.ui.dialog.editAssignments_Persons_PersonNameSelected then
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
                                        [ Options.onToggle UI_Dialog_EditAssignmentsToggleAll
                                        , Toggles.value (areDialogEditAssignmentsAllSelected model)
                                        ]
                                        []
                                    ]
                                , Table.th [] [ text <| roleReadForID roleID model ]
                                ]
                            ]
                        , Table.tbody []
                            (model.ui.dialog.editAssignments_Persons_PersonNameSelected
                                |> List.indexedMap
                                    (\idx ( pn, isSelected ) ->
                                        Table.tr
                                            [ Table.selected
                                                |> Options.when isSelected
                                            ]
                                            [ Table.td []
                                                [ Toggles.checkbox Mdl
                                                    [ idx ]
                                                    model.mdl
                                                    [ Options.onToggle (UI_Dialog_EditAssignmentsToggle pn)
                                                    , Toggles.value isSelected
                                                    ]
                                                    []
                                                ]
                                            , Table.td [] [ text pn ]
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
                        , Options.onClick (UpdateAssignmentsFromDialog planID roleID)
                        ]
                        [ text "Plan" ]
                    , cancelButton
                    ]
                ]



-- VIEW:UI:DIALOG:STATUS_RECORD


view_Dialog_AddStatusRecord : PlanID -> Model -> Html Msg
view_Dialog_AddStatusRecord planID model =
    let
        statusSelect : Html Msg
        statusSelect =
            Select.render Mdl
                [ 903 ]
                model.mdl
                [ Select.label "Status"
                , Select.floatingLabel
                , Select.ripple
                , Select.value <| statusReadForID model.ui.dialog.addStatusRecord_Status_SelectedID model
                ]
                (model.state.statuses
                    |> List.map .id
                    |> List.map
                        (\statusID ->
                            Select.item
                                [ Item.onSelect (UI_Dialog_AddStatusRecord_SelectStatus statusID)
                                ]
                                [ text <| statusReadForID statusID model
                                ]
                        )
                )

        parsedDate_ =
            model.ui.dialog.addStatusRecord_ParsedDate

        dateErrorIfApplicable =
            case parsedDate_ of
                Ok parsedDate ->
                    Options.nop

                Err error ->
                    Textfield.error error

        descriptionErrorIfApplicable =
            let
                value =
                    model.ui.dialog.addStatusRecord_DescriptionField_Value

                touched =
                    model.ui.dialog.addStatusRecord_DescriptionField_Touched
            in
                if touched && String.length value == 0 then
                    Textfield.error "The description can't be empty"
                else
                    Options.nop

        dialogHasError =
            not (dateErrorIfApplicable == Options.nop) || not (descriptionErrorIfApplicable == Options.nop)
    in
        Dialog.view
            []
            [ Dialog.title [] [ text "Record status" ]
            , Dialog.content []
                [ statusSelect
                , Textfield.render Mdl
                    [ 911 ]
                    model.mdl
                    [ Options.onInput (UI_Dialog_AddStatusRecord_UpdateDescription planID)
                    , Textfield.label "Description"
                    , Textfield.floatingLabel
                    , Textfield.text_
                    , Textfield.value model.ui.dialog.addStatusRecord_DescriptionField_Value
                    , descriptionErrorIfApplicable
                    ]
                    []
                , Textfield.render Mdl
                    [ 912 ]
                    model.mdl
                    [ Options.onInput (UI_Dialog_AddStatusRecord_UpdateDate planID)
                    , Textfield.label "Date"
                    , Textfield.floatingLabel
                    , Textfield.text_
                    , Textfield.value model.ui.dialog.addStatusRecord_DateField_Value
                    , dateErrorIfApplicable
                    ]
                    []
                ]
            , Dialog.actions []
                [ Button.render Mdl
                    [ 7 ]
                    model.mdl
                    [ Dialog.closeOn "click"
                    , Options.onClick (CreateStatusRecordFromDialog planID)
                    , Button.disabled |> Options.when dialogHasError
                    ]
                    [ text "Create" ]
                , Button.render Mdl
                    [ 8 ]
                    model.mdl
                    [ Dialog.closeOn "click"
                    , Options.onClick UI_Dialog_Reset
                    ]
                    [ text "Cancel" ]
                ]
            ]



-- DIALOG:UTIL


isTextfieldFocused : List Int -> Model -> Bool
isTextfieldFocused key model =
    case Dict.get key model.mdl.textfield of
        Nothing ->
            False

        Just tf ->
            tf.isFocused



-- COMMANDS


stateDecoder : Json.Decode.Decoder State
stateDecoder =
    Json.Decode.map6 State
        (Json.Decode.list roleDecoder)
        (Json.Decode.list statusDecoder)
        (Json.Decode.list cycleDecoder)
        (Json.Decode.list personDecoder)
        (Json.Decode.list planDecoder)
        (Json.Decode.list projectDecoder)


roleDecoder : Json.Decode.Decoder Role
roleDecoder =
    Json.Decode.map3 Role
        (Json.Decode.field "id" (Json.Decode.map RoleID Json.Decode.string))
        (Json.Decode.field "read" Json.Decode.string)
        (Json.Decode.field "icon" Json.Decode.string)


statusDecoder : Json.Decode.Decoder Status
statusDecoder =
    Json.Decode.map3 Status
        (Json.Decode.field "id" (Json.Decode.map StatusID Json.Decode.string))
        (Json.Decode.field "read" Json.Decode.string)
        (Json.Decode.field "icon" Json.Decode.string)


cycleDecoder : Json.Decode.Decoder Cycle
cycleDecoder =
    Json.Decode.map Cycle
        (Json.Decode.field "index" (Json.Decode.map CycleIndex Json.Decode.int))


personDecoder : Json.Decode.Decoder Person
personDecoder =
    Json.Decode.map2 Person
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "roleID" (Json.Decode.map RoleID Json.Decode.string))


planDecoder : Json.Decode.Decoder Plan
planDecoder =
    Json.Decode.map3 Plan
        (Json.Decode.field "id" planIDDecoder)
        (Json.Decode.field "assignments" (Json.Decode.list assignmentDecoder))
        (Json.Decode.field "statusRecords" (Json.Decode.list statusRecordDecoder))


assignmentDecoder : Json.Decode.Decoder Assignment
assignmentDecoder =
    Json.Decode.map Assignment
        (Json.Decode.field "personName" Json.Decode.string)


statusRecordDecoder : Json.Decode.Decoder StatusRecord
statusRecordDecoder =
    Json.Decode.map3 StatusRecord
        (Json.Decode.field "id" (Json.Decode.map StatusID Json.Decode.string))
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.field "date" Json.Decode.Extra.date)


planIDDecoder : Json.Decode.Decoder PlanID
planIDDecoder =
    Json.Decode.map2 PlanID
        (Json.Decode.field "cycleIndex" (Json.Decode.map CycleIndex Json.Decode.int))
        (Json.Decode.field "projectName" (Json.Decode.map ProjectName Json.Decode.string))


projectDecoder : Json.Decode.Decoder Project
projectDecoder =
    Json.Decode.map Project
        (Json.Decode.field "name" (Json.Decode.map ProjectName Json.Decode.string))


resourceUrl : String
resourceUrl =
    "localhost:8081"


fetchLatestState : Cmd Msg
fetchLatestState =
    let
        request =
            Http.get resourceUrl stateDecoder
    in
        Http.send FetchLatestStateDone request



-- MAIN


initModel : Model
initModel =
    defaultModelForDev


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Task.perform SetDate Date.now )
        , view = view
        , update = update
        , subscriptions = Material.subscriptions Mdl
        }
