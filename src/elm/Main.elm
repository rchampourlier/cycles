module Main exposing (..)

import Array exposing (Array)
import Date exposing (Date)
import Date.Extra as Date
import Dict exposing (Dict)
import Html exposing (Html, text)
import Html.Attributes exposing (class, href)
import Http
import Json.Decode
import Json.Decode.Extra
import Json.Encode
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
import Material.Menu as Menu
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
    , ui : UI_Model
    , lastError : Maybe String
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


projectNameToString : ProjectName -> String
projectNameToString pn =
    case pn of
        ProjectName n ->
            n



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


roleIDToString : RoleID -> String
roleIDToString roleID =
    case roleID of
        RoleID rid ->
            rid



-- MODEL:ASSIGNMENT


type alias Assignment =
    { personName : String
    }



-- MODEL:STATUS


type alias StatusRecord =
    -- TODO: `id` should be `statusID`
    { statusID : StatusID
    , description : String
    , date : Date
    }


type StatusID
    = StatusID String


statusIDToString : StatusID -> String
statusIDToString statusID =
    case statusID of
        StatusID sid ->
            sid


type alias Status =
    { id : StatusID
    , read : String
    , icon : String
    }



-- MODEL:UI


type alias UI_Model =
    { selectedTab : Int
    , dialog : UI_Dialog_Model
    }



-- MODEL:UI:DIALOG
{- MDL key references

   # 1xx serie: cycles tab
     110: cycle column, plan project button
     111: cycle column > header > assignment box > buttons
     112: cycle column > header > assignment box > tooltips
     120: cycle column > plan > actions > assignments
     130: cycle column > plan > actions > add status button
     131: cycle column > plan > actions > add status button > tooltip
     132: cycle column > plan > actions > menu

   # 3xx serie: persons tab
     310: person row delete button
     321: dialog -> name field
     322: dialog -> role select
     327: dialog -> button delete
     328: dialog -> button create or update
     329: dialog -> button cancel

   # 9xx serie: dialog
     901: cancel button
     902: action button
     911: dialog addStatusRecord -> description field
     912: dialog addStatusRecord -> date field
     913: dialog addStatusRecord -> status select

-}


type alias UI_Dialog_Model =
    { kind : UI_Dialog_Kind

    -- PERSONS
    , person_EditedPerson : Person

    -- PROJECTS
    , addProject_ProjectNameField_Value : String

    -- PLANS
    , addPlans_Projects_SelectedProjectNames : Set String -- project names as String

    -- ASSIGNMENTS
    , editAssignments_Persons_PersonNameSelected : List ( String, Bool ) -- personName, isSelected

    -- STATUS_RECORDS
    , addStatusRecord_SelectedStatusID : StatusID
    , addStatusRecord_DateField_Value : String
    , addStatusRecord_DescriptionField_Value : String
    , addStatusRecord_DescriptionField_Touched : Bool
    , addStatusRecord_ParsedDate : Result String Date
    }


type UI_Dialog_Kind
    = UI_Dialog_None
      -- ASSIGNMENTS
    | UI_Dialog_EditAssignments PlanID RoleID
      -- PERSONS
    | UI_Dialog_Person (Maybe Person)
      -- PLANS
    | UI_Dialog_AddPlans CycleIndex
    | UI_Dialog_RemovePlanConfirmation PlanID
      -- PROJECTS
    | UI_Dialog_AddProject
      -- STATUS_RECORDS
    | UI_Dialog_AddStatusRecord PlanID



-- MODEL:DEFAULT


initModel : Model
initModel =
    { state = initState
    , mdl = Material.model
    , ui = init_UI_Model Nothing
    , lastError = Nothing
    }


initState : State
initState =
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
    in
        { roles = roles
        , statuses = statuses
        , cycles = []
        , persons = []
        , plans = []
        , projects = []
        }


init_UI_Model : Maybe Date -> UI_Model
init_UI_Model date =
    { selectedTab = 0
    , dialog = init_UI_Dialog_Model
    }


init_UI_Dialog_Model : UI_Dialog_Model
init_UI_Dialog_Model =
    { kind = UI_Dialog_None
    , person_EditedPerson = Person "New person" (RoleID "product_manager")
    , addProject_ProjectNameField_Value = ""
    , addPlans_Projects_SelectedProjectNames = Set.empty
    , editAssignments_Persons_PersonNameSelected = []
    , addStatusRecord_SelectedStatusID = StatusID "on-track"
    , addStatusRecord_DateField_Value = ""
    , addStatusRecord_DescriptionField_Value = "Everything's ok!"
    , addStatusRecord_DescriptionField_Touched = False
    , addStatusRecord_ParsedDate = Err "Date not initialized yet!"
    }



-- HELPERS


maybeDateAsString : Maybe Date -> String
maybeDateAsString maybeDate =
    case maybeDate of
        Nothing ->
            ""

        Just date ->
            [ Date.year >> toString, Date.month >> monthAsString, Date.day >> toString ]
                |> List.map (\f -> f date)
                |> List.intersperse "-"
                |> List.foldr (++) ""



-- UPDATE


type
    Msg
    -- ASSIGNMENTS
    = EditAssignments PlanID RoleID
    | UpdateAssignmentsFromDialog PlanID RoleID
      -- CYCLES
    | CreateCycle
      -- PERSONS
    | AddPerson
    | CreatePersonFromDialog
    | EditPerson Person
    | UpdatePersonFromDialog Person
    | DeletePersonFromDialog Person
      -- PROJECTS
    | AddProject
    | CreateProjectFromDialog
      -- PLANS
    | AddPlans CycleIndex
    | AddPlansFromDialog CycleIndex
    | RemovePlan PlanID
    | RemovePlanWithConfirmation PlanID
      -- STATUS_RECORDS
    | AddStatusRecord PlanID
    | CreateStatusRecordFromDialog PlanID
      -- COMMANDS
    | SetDate Date
    | FetchLatestStateDone (Result Http.Error State)
    | StoreStateDone (Result Http.Error ())
      -- UI
    | UI_SelectTab Int
      -- UI:DIALOG
    | UI_Dialog_Reset
      -- UI:DIALOG:PERSONS
    | UI_Dialog_Person_UpdateName String
    | UI_Dialog_Person_SelectRole RoleID
      -- UI:DIALOG:PROJECTS
    | UI_Dialog_AddProject_UpdateName String
      -- UI:DIALOG:PLANS
    | UI_Dialog_AddPlans_ToggleProjectByNameAllProjects
    | UI_Dialog_AddPlans_ToggleProjectByName String
      -- UI:DIALOG:ASSIGNMENTS
    | UI_Dialog_EditAssignments_ToggleAllPersons
    | UI_Dialog_EditAssignments_TogglePersonByName String -- personName
      -- UI:DIALOG:STATUS_RECORDS
    | UI_Dialog_AddStatusRecord_PrepareAndOpen PlanID Date
    | UI_Dialog_AddStatusRecord_SelectStatus StatusID
    | UI_Dialog_AddStatusRecord_UpdateDate PlanID String
    | UI_Dialog_AddStatusRecord_UpdateDescription PlanID String
      -- MDL
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ui_ =
            model.ui
    in
        case msg of
            -- EVENTS
            CreateCycle ->
                model
                    |> createCycle
                    |> updateModel

            -- PERSONS
            AddPerson ->
                update_UI_Dialog_prepare (UI_Dialog_Person Nothing) model

            CreatePersonFromDialog ->
                model
                    |> addPerson model.ui.dialog.person_EditedPerson
                    |> sortPersons
                    |> update_UI_Dialog_Reset
                    |> updateModel

            EditPerson person ->
                update_UI_Dialog_prepare (UI_Dialog_Person (Just person)) model

            UpdatePersonFromDialog person ->
                model
                    |> deletePerson person
                    |> addPerson model.ui.dialog.person_EditedPerson
                    |> sortPersons
                    |> update_UI_Dialog_Reset
                    |> updateModel

            DeletePersonFromDialog person ->
                model
                    |> deletePerson person
                    |> update_UI_Dialog_Reset
                    |> updateModel

            -- PROJECTS
            AddProject ->
                update_UI_Dialog_prepare UI_Dialog_AddProject model

            CreateProjectFromDialog ->
                let
                    newProject =
                        { name = ProjectName model.ui.dialog.addProject_ProjectNameField_Value }
                in
                    model
                        |> addProject newProject
                        |> update_UI_Dialog_Reset
                        |> updateModel

            -- PLANS
            AddPlans cycleIdx ->
                model |> update_UI_Dialog_prepare (UI_Dialog_AddPlans cycleIdx)

            AddPlansFromDialog cycleIdx ->
                model
                    |> addPlansFromDialog cycleIdx
                    |> update_UI_Dialog_Reset
                    |> updateModel

            RemovePlan planID ->
                update_UI_Dialog_prepare
                    (UI_Dialog_RemovePlanConfirmation planID)
                    model

            RemovePlanWithConfirmation planID ->
                model
                    |> removePlan planID
                    |> updateModel

            -- ASSIGNMENTS
            EditAssignments planID roleID ->
                model
                    |> update_UI_Dialog_prepare (UI_Dialog_EditAssignments planID roleID)

            UpdateAssignmentsFromDialog planID roleID ->
                model
                    |> updateAssignmentsFromDialog planID roleID
                    |> update_UI_Dialog_Reset
                    |> updateModel

            -- STATUS RECORD
            AddStatusRecord planID ->
                ( model, Task.perform (UI_Dialog_AddStatusRecord_PrepareAndOpen planID) Date.now )

            CreateStatusRecordFromDialog planID ->
                model
                    |> updateStatusRecordsFromDialog planID
                    |> update_UI_Dialog_Reset
                    |> updateModel

            -- COMMANDS
            SetDate date ->
                let
                    ui_ =
                        model.ui
                in
                    ( { model | ui = init_UI_Model (Just date) }, Cmd.none )

            FetchLatestStateDone result ->
                case result of
                    Result.Ok newState ->
                        ( { model | state = newState }, Cmd.none )

                    Result.Err error ->
                        ( { model | lastError = Just (httpErrorToString error) }, Cmd.none )

            StoreStateDone result ->
                case result of
                    Result.Ok () ->
                        ( model, Cmd.none )

                    Result.Err error ->
                        ( { model | lastError = Just (httpErrorToString error) }, Cmd.none )

            -- UI
            UI_SelectTab k ->
                ( { model | ui = { ui_ | selectedTab = k } }, Cmd.none )

            -- UI:DIALOG
            UI_Dialog_Reset ->
                ( model |> update_UI_Dialog_Reset, Cmd.none )

            -- UI:DIALOG:PERSONS
            UI_Dialog_Person_UpdateName newNameStr ->
                ( model |> update_UI_Dialog_Person_UpdateName newNameStr, Cmd.none )

            UI_Dialog_Person_SelectRole newRoleID ->
                ( model |> update_UI_Dialog_Person_SelectRole newRoleID, Cmd.none )

            -- UI:DIALOG:PLANS
            UI_Dialog_AddPlans_ToggleProjectByNameAllProjects ->
                ( model |> update_UI_Dialog_AddPlans_ToggleProjectByNameAllProjects, Cmd.none )

            UI_Dialog_AddPlans_ToggleProjectByName projectNameStr ->
                ( model |> update_UI_Dialog_AddPlans_ToggleProjectByName projectNameStr, Cmd.none )

            -- UI:DIALOG:PROJECTS
            UI_Dialog_AddProject_UpdateName nameStr ->
                ( model |> update_UI_Dialog_AddProject_UpdateName nameStr, Cmd.none )

            -- UI:DIALOG:ASSIGNMENTS
            UI_Dialog_EditAssignments_ToggleAllPersons ->
                ( model |> update_UI_Dialog_EditAssignments_ToggleAllPersons, Cmd.none )

            UI_Dialog_EditAssignments_TogglePersonByName personName ->
                ( model |> update_UI_Dialog_EditAssignments_TogglePersonByName personName, Cmd.none )

            -- UI:DIALOG:STATUS_RECORDS
            UI_Dialog_AddStatusRecord_PrepareAndOpen planID date ->
                update_UI_Dialog_prepare_AddStatusRecord planID date model

            UI_Dialog_AddStatusRecord_SelectStatus newStatusID ->
                ( model |> update_UI_Dialog_AddStatusRecord_SelectedStatusID newStatusID, Cmd.none )

            UI_Dialog_AddStatusRecord_UpdateDate planID newDate ->
                ( model |> update_UI_Dialog_AddStatusRecord_UpdateDate newDate, Cmd.none )

            UI_Dialog_AddStatusRecord_UpdateDescription planID newDescription ->
                ( model |> update_UI_Dialog_AddStatusRecord_Description newDescription, Cmd.none )

            Mdl msg_ ->
                Material.update Mdl msg_ model


updateModel : Model -> ( Model, Cmd Msg )
updateModel model =
    {- Helper to be used in `update` to return the new model and
       the command to perform the `StoreState` command.
    -}
    ( model, storeState model.state )


updateState : State -> Model -> Model
updateState newState model =
    { model | state = newState }



-- UPDATE:CYCLES


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


cycleForIndex : CycleIndex -> Model -> Maybe Cycle
cycleForIndex cycleIdx model =
    model.state.cycles
        |> List.filter (\c -> c.index == cycleIdx)
        |> List.head


createCycle : Model -> Model
createCycle model =
    let
        state_ =
            model.state

        newCycle =
            { index = nextIndex model }

        newState =
            { state_ | cycles = newCycle :: state_.cycles }
    in
        { model | state = newState }


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


addPerson : Person -> Model -> Model
addPerson person model =
    let
        state_ =
            model.state
    in
        model
            |> updateState { state_ | persons = (person :: state_.persons) }


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


deletePerson : Person -> Model -> Model
deletePerson person model =
    let
        state_ =
            model.state

        newPersons =
            state_.persons
                |> List.filter (\p -> p.name /= person.name)

        newState =
            { state_ | persons = newPersons }
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


roleIconForID : RoleID -> Model -> String
roleIconForID id model =
    case roleForID id model of
        Nothing ->
            "Unexpected role ID"

        Just role ->
            role.icon



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
    in
        model |> updatePlans newPlans


updatePlans : List Plan -> Model -> Model
updatePlans plans model =
    let
        state_ =
            model.state

        newState =
            { state_ | plans = plans }
    in
        { model | state = newState }


removePlan : PlanID -> Model -> Model
removePlan planID model =
    let
        newPlans =
            model.state.plans
                |> List.filter (\p -> p.id /= planID)
    in
        model |> updatePlans newPlans


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
            { statusID = model.ui.dialog.addStatusRecord_SelectedStatusID
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


update_UI_Dialog_prepare : UI_Dialog_Kind -> Model -> ( Model, Cmd Msg )
update_UI_Dialog_prepare kind model =
    let
        model_ =
            update_UI_Dialog_Kind kind model
    in
        case kind of
            UI_Dialog_EditAssignments planID roleID ->
                update_UI_Dialog_prepare_EditAssignments planID roleID model_

            UI_Dialog_Person (Just person) ->
                update_UI_Dialog_prepare_EditPerson person model_

            _ ->
                ( model_, Cmd.none )


update_UI_Dialog_prepare_EditPerson : Person -> Model -> ( Model, Cmd Msg )
update_UI_Dialog_prepare_EditPerson person model =
    {- This dialog requires the `dialog.person_EditedPerson` to be filled
       with the attributes of the edited person before display.
    -}
    let
        dialog_ =
            model.ui.dialog

        newDialog =
            { dialog_ | person_EditedPerson = person }
    in
        ( model |> update_UI_Dialog newDialog, Cmd.none )


update_UI_Dialog_prepare_EditAssignments : PlanID -> RoleID -> Model -> ( Model, Cmd Msg )
update_UI_Dialog_prepare_EditAssignments planID roleID model =
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


update_UI_Dialog_prepare_AddStatusRecord : PlanID -> Date -> Model -> ( Model, Cmd Msg )
update_UI_Dialog_prepare_AddStatusRecord planID date model =
    let
        dialog_ =
            model.ui.dialog

        newDialog =
            { dialog_
                | kind = UI_Dialog_AddStatusRecord planID
                , addStatusRecord_DateField_Value = maybeDateAsString (Just date)
                , addStatusRecord_ParsedDate = Ok date
            }
    in
        ( model |> update_UI_Dialog newDialog, Cmd.none )


update_UI_Dialog : UI_Dialog_Model -> Model -> Model
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
            { ui_ | dialog = init_UI_Dialog_Model }
    in
        { model | ui = newUI }


update_UI_Dialog_Kind : UI_Dialog_Kind -> Model -> Model
update_UI_Dialog_Kind kind model =
    let
        dialog_ =
            model.ui.dialog
    in
        model |> update_UI_Dialog { dialog_ | kind = kind }



-- UPDATE:UI:DIALOG:PERSONS


update_UI_Dialog_Person_UpdateName : String -> Model -> Model
update_UI_Dialog_Person_UpdateName nameStr model =
    let
        dialog_ =
            model.ui.dialog

        editedPerson_ =
            dialog_.person_EditedPerson
    in
        model
            |> update_UI_Dialog { dialog_ | person_EditedPerson = { editedPerson_ | name = nameStr } }


update_UI_Dialog_Person_SelectRole : RoleID -> Model -> Model
update_UI_Dialog_Person_SelectRole newRoleID model =
    let
        dialog_ =
            model.ui.dialog

        editedPerson_ =
            dialog_.person_EditedPerson
    in
        model
            |> update_UI_Dialog { dialog_ | person_EditedPerson = { editedPerson_ | roleID = newRoleID } }



-- UPDATE:UI:DIALOG:PROJECTS


update_UI_Dialog_AddProject_UpdateName : String -> Model -> Model
update_UI_Dialog_AddProject_UpdateName nameStr model =
    let
        dialog_ =
            model.ui.dialog
    in
        model
            |> update_UI_Dialog { dialog_ | addProject_ProjectNameField_Value = nameStr }



-- UPDATE:UI:DIALOG:PLANS


update_UI_Dialog_AddPlans_Selected : Model -> Set String -> Model
update_UI_Dialog_AddPlans_Selected model newPlansSelected =
    let
        dialog_ =
            model.ui.dialog
    in
        model |> update_UI_Dialog { dialog_ | addPlans_Projects_SelectedProjectNames = newPlansSelected }


update_UI_Dialog_AddPlans_ToggleProjectByNameNone : Model -> Model
update_UI_Dialog_AddPlans_ToggleProjectByNameNone model =
    update_UI_Dialog_AddPlans_Selected model Set.empty


update_UI_Dialog_AddPlans_ToggleProjectByNameAllProjects : Model -> Model
update_UI_Dialog_AddPlans_ToggleProjectByNameAllProjects model =
    let
        allProjectNames =
            model.state.projects
                |> List.map (.name >> projectNameToString)
                |> Set.fromList

        newPlansSelected =
            if areUI_Dialog_AddPlansAllSelected model then
                Set.empty
            else
                allProjectNames
    in
        update_UI_Dialog_AddPlans_Selected model newPlansSelected


update_UI_Dialog_AddPlans_ToggleProjectByName : String -> Model -> Model
update_UI_Dialog_AddPlans_ToggleProjectByName projectNameStr model =
    let
        dialog_ =
            model.ui.dialog

        newPlansSelected =
            toggle projectNameStr dialog_.addPlans_Projects_SelectedProjectNames
    in
        update_UI_Dialog_AddPlans_Selected model newPlansSelected


areUI_Dialog_AddPlansAllSelected : Model -> Bool
areUI_Dialog_AddPlansAllSelected model =
    Set.size model.ui.dialog.addPlans_Projects_SelectedProjectNames == List.length model.state.projects



-- UPDATE:UI:DIALOG:ASSIGNMENTS


update_UI_Dialog_EditAssignmentsReset : Model -> Model
update_UI_Dialog_EditAssignmentsReset model =
    let
        ui_ =
            model.ui

        dialog_ =
            ui_.dialog

        newUI_Dialog_EditAssignmentsPersons =
            ui_.dialog.editAssignments_Persons_PersonNameSelected
                |> List.map (\( pn, _ ) -> ( pn, False ))

        newDialog =
            { dialog_ | editAssignments_Persons_PersonNameSelected = newUI_Dialog_EditAssignmentsPersons }

        newUI =
            { ui_ | dialog = newDialog }
    in
        { model | ui = newUI }


update_UI_Dialog_EditAssignments_ToggleAllPersons : Model -> Model
update_UI_Dialog_EditAssignments_ToggleAllPersons model =
    let
        ui_ =
            model.ui

        dialog_ =
            ui_.dialog

        alreadySelected =
            List.all Tuple.second ui_.dialog.editAssignments_Persons_PersonNameSelected

        newUI_Dialog_EditAssignmentsPersons =
            dialog_.editAssignments_Persons_PersonNameSelected
                |> List.map (\( pn, _ ) -> ( pn, not alreadySelected ))

        newDialog =
            { dialog_ | editAssignments_Persons_PersonNameSelected = newUI_Dialog_EditAssignmentsPersons }

        newUI =
            { ui_ | dialog = newDialog }
    in
        { model | ui = newUI }


update_UI_Dialog_EditAssignments_TogglePersonByName : String -> Model -> Model
update_UI_Dialog_EditAssignments_TogglePersonByName personName model =
    let
        ui_ =
            model.ui

        dialog_ =
            ui_.dialog

        newUI_Dialog_EditAssignmentsPersons =
            ui_.dialog.editAssignments_Persons_PersonNameSelected
                |> List.map
                    (\( pn, selected ) ->
                        if pn == personName then
                            ( pn, not selected )
                        else
                            ( pn, selected )
                    )

        newDialog =
            { dialog_ | editAssignments_Persons_PersonNameSelected = newUI_Dialog_EditAssignmentsPersons }

        newUi =
            { ui_ | dialog = newDialog }
    in
        { model | ui = newUi }


areUI_Dialog_EditAssignmentsAllSelected : Model -> Bool
areUI_Dialog_EditAssignmentsAllSelected model =
    model.ui.dialog.editAssignments_Persons_PersonNameSelected
        |> List.all Tuple.second



-- UPDATE:UI:DIALOG:ADD_STATUS_RECORD


update_UI_Dialog_AddStatusRecord_SelectedStatusID : StatusID -> Model -> Model
update_UI_Dialog_AddStatusRecord_SelectedStatusID newStatusID model =
    let
        dialog_ =
            model.ui.dialog

        newDialog =
            { dialog_ | addStatusRecord_SelectedStatusID = newStatusID }
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
    let
        errorBox =
            case model.lastError of
                Nothing ->
                    div [] []

                Just errorMsg ->
                    Html.div
                        [ Html.Attributes.id "app-last-error" ]
                        [ text <| Maybe.withDefault "" model.lastError ]
    in
        Html.div
            [ Html.Attributes.id "app" ]
            [ layout model, errorBox ]


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
        [ cs "cycles__column" ]
        (cycleColumnHeader model cycle :: cycleColumnProjects model cycle)


cycleColumnHeader : Model -> Cycle -> Html Msg
cycleColumnHeader model cycle =
    let
        nonAssignments =
            personsNotAssignedForCycle model cycle.index
                |> List.map (\p -> { personName = p.name })

        plansCount =
            plansForCycleIndex cycle.index model
                |> List.length

        buttonTitle =
            case plansCount of
                0 ->
                    "Plan projects"

                1 ->
                    " 1 project planned"

                _ ->
                    (plansCount |> toString) ++ " projects planned"
    in
        Card.view
            [ cs "cycles__column__header"
            , css "width" "256px"
            , Elevation.e2
            ]
            [ Card.title
                [ cs "cycles__column__header__title"
                , css "flex-direction" "row"
                ]
                [ span [ cs "cycles__column__header__title__hashtag" ] [ text "#" ]
                , span [ cs "cycles__column__header__title__index" ] [ text <| (toString << cycleIndexToInt) cycle.index ]
                , span [ cs "cycles__column__header__title__description" ] [ text "Some description (not implemented yet)" ]
                ]
            , Card.text [ cs "cycles__column__header__text" ]
                [ Button.render Mdl
                    [ 110, cycleIndexToInt cycle.index ]
                    model.mdl
                    [ Button.ripple
                    , Options.onClick (AddPlans cycle.index)
                    , Dialog.openOn "click"
                    ]
                    [ text buttonTitle ]
                , assignmentsBox
                    [ 112, cycleIndexToInt cycle.index ]
                    model
                    Nothing
                    nonAssignments
                    "Not assigned: "
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

        keySuffix =
            [ cycleIndexToInt cycleIdx, i ]

        latestStatusText =
            case latestStatusRecord_ of
                Nothing ->
                    div [] [ text <| "No status yet" ]

                Just sr ->
                    div [] [ text <| sr.description ]

        latestStatusIcon =
            case latestStatusRecord_ of
                Nothing ->
                    Icon.i "access_time"

                Just sr ->
                    case statusForID sr.statusID model of
                        Nothing ->
                            Icon.i "access_time"

                        Just s ->
                            Icon.i s.icon

        addStatusRecordButton =
            [ Button.render Mdl
                ([ 130 ] ++ keySuffix)
                model.mdl
                [ Button.minifab
                , Options.onClick (AddStatusRecord plan.id)
                , Dialog.openOn "click"
                , Tooltip.attach Mdl ([ 131 ] ++ keySuffix)
                ]
                [ Icon.i "add" ]
            , Tooltip.render Mdl
                ([ 131 ] ++ keySuffix)
                model.mdl
                [ Tooltip.top ]
                [ text "Add new status record" ]
            ]

        planActionsMenu =
            Menu.render Mdl
                ([ 132 ] ++ keySuffix)
                model.mdl
                [ Menu.bottomLeft ]
                [ Item.item
                    [ Item.onSelect (RemovePlan plan.id)
                    , Dialog.openOn "click"
                    ]
                    [ text "Remove this plan" ]
                ]
    in
        case project_ of
            Nothing ->
                div [] []

            Just project ->
                Card.view
                    [ cs "cycles__project-card"
                    , css "width" "256px"
                    , Elevation.e2
                    ]
                    [ Card.title
                        [ css "flex-direction" "column" ]
                        [ Card.head [] [ text <| toString (projectNameToString project.name) ]
                        ]
                    , Card.text
                        [ cs "cycles__project-card__content" ]
                        [ div
                            [ cs "cycles__project-card__latest-status" ]
                            [ latestStatusIcon, latestStatusText ]
                        , div
                            [ cs "cycles__project-card__add-status-button" ]
                            addStatusRecordButton
                        ]
                    , Card.actions
                        [ cs "cycles__project-card__actions" ]
                        [ div []
                            [ assignmentsBox
                                [ 120, cycleIndexToInt cycleIdx, i ]
                                model
                                (Just (EditAssignments (PlanID cycleIdx project.name)))
                                plan.assignments
                                "Assigned: "
                            , planActionsMenu
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
                [ Tooltip.top ]
                [ text <| tooltipTextPrefix ++ List.foldl (++) "" (List.intersperse ", " personNames) ]

        itemElement : Int -> Role -> List String -> List (Html Msg)
        itemElement i role personNames =
            let
                personCount =
                    personNames |> List.length

                colorIfApplicable =
                    css "color" "#D43F00" |> Options.when (personCount == 0)
            in
                case buttonAction of
                    Nothing ->
                        [ span
                            [ tooltipAttachment i
                            , cs "assignments-box__item"
                            , colorIfApplicable
                            ]
                            [ Icon.i role.icon, text <| toString personCount ]
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
                                , text <| toString personCount
                                ]
                            ]
                        , tooltipRender i personNames
                        ]
    in
        div [ cs "assignments-box" ]
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
        , Options.onClick AddPerson
        , Dialog.openOn "click"
        ]
        [ text "Add person" ]


personsTable : Model -> Html Msg
personsTable model =
    let
        editButton idx person =
            Button.render Mdl
                [ 310, idx ]
                model.mdl
                [ Button.icon
                , Options.onClick (EditPerson person)
                , Dialog.openOn "click"
                ]
                [ Icon.i "edit" ]

        personRow idx person =
            List.li [ List.withSubtitle ]
                [ List.content []
                    [ List.avatarIcon (roleIconForID person.roleID model) []
                    , text person.name
                    , List.subtitle [] [ text <| roleReadForID person.roleID model ]
                    ]
                , editButton idx person
                ]
    in
        List.ul [ cs "persons" ]
            (model.state.persons
                |> List.indexedMap personRow
            )



-- VIEW:DIALOG


dialog : Model -> Html Msg
dialog model =
    let
        dialogKind =
            model.ui.dialog.kind
    in
        case dialogKind of
            UI_Dialog_None ->
                view_Dialog_Error "Unexpected dialog" model

            -- ASSIGNMENTS
            UI_Dialog_EditAssignments planID roleID ->
                view_Dialog_EditAssignments planID roleID model

            -- PERSONS
            UI_Dialog_Person maybePerson ->
                view_Dialog_Person maybePerson model

            -- PLANS
            UI_Dialog_AddPlans cycleIdx ->
                let
                    cycle_ =
                        cycleForIndex cycleIdx model
                in
                    case cycle_ of
                        Nothing ->
                            view_Dialog_Error "Unknown cycle" model

                        Just cycle ->
                            view_Dialog_AddPlan cycle model

            UI_Dialog_RemovePlanConfirmation planID ->
                case personsAssignedForPlan model planID of
                    [] ->
                        view_Dialog_RemovePlanConfirmation planID model

                    _ ->
                        view_Dialog_Error "Can't remove plan with assignments" model

            -- PROJECTS
            UI_Dialog_AddProject ->
                view_Dialog_AddProject model

            -- STATUS_RECORDS
            UI_Dialog_AddStatusRecord planID ->
                view_Dialog_AddStatusRecord planID model


view_Dialog_Error : String -> Model -> Html Msg
view_Dialog_Error message model =
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



-- VIEW:DIALOG:PERSON


view_Dialog_Person : Maybe Person -> Model -> Html Msg
view_Dialog_Person maybePerson model =
    let
        editedPerson =
            model.ui.dialog.person_EditedPerson

        roleSelect : Html Msg
        roleSelect =
            Select.render Mdl
                [ 322 ]
                model.mdl
                [ Select.label "Role"
                , Select.floatingLabel
                , Select.ripple
                , Select.value <| roleReadForID editedPerson.roleID model
                ]
                (model.state.roles
                    |> List.map .id
                    |> List.map
                        (\roleID ->
                            Select.item
                                [ Item.onSelect (UI_Dialog_Person_SelectRole roleID)
                                ]
                                [ text <| roleReadForID roleID model
                                ]
                        )
                )

        nameErrorIfApplicable =
            let
                value =
                    editedPerson.name
            in
                if String.length value == 0 then
                    Textfield.error "The name can't be empty"
                else
                    Options.nop

        dialogHasError =
            not (nameErrorIfApplicable == Options.nop)

        nameField : Html Msg
        nameField =
            Textfield.render Mdl
                [ 321 ]
                model.mdl
                [ Options.onInput UI_Dialog_Person_UpdateName
                , Textfield.label "Name"
                , Textfield.floatingLabel
                , Textfield.text_
                , Textfield.value editedPerson.name
                , nameErrorIfApplicable
                ]
                []

        ( dialogTitle, dialogPrimaryAction, dialogMsg, additionalActions ) =
            case maybePerson of
                Nothing ->
                    ( "Add person", "Create", CreatePersonFromDialog, [] )

                Just person ->
                    ( "Edit person"
                    , "Update"
                    , UpdatePersonFromDialog person
                    , [ Button.render Mdl
                            [ 327 ]
                            model.mdl
                            [ Dialog.closeOn "click"
                            , Options.onClick (DeletePersonFromDialog person)
                            ]
                            [ Icon.i "delete" ]
                      ]
                    )

        dialogActions =
            [ Button.render Mdl
                [ 328 ]
                model.mdl
                [ Dialog.closeOn "click"
                , Options.onClick dialogMsg
                , Button.disabled |> Options.when dialogHasError
                ]
                [ text dialogPrimaryAction ]
            , Button.render Mdl
                [ 329 ]
                model.mdl
                [ Dialog.closeOn "click"
                , Options.onClick UI_Dialog_Reset
                ]
                [ text "Cancel" ]
            ]
                ++ additionalActions
    in
        Dialog.view
            []
            [ Dialog.title [] [ text dialogTitle ]
            , Dialog.content [] [ nameField, roleSelect ]
            , Dialog.actions [] dialogActions
            ]



-- VIEW:DIALOG:PROJECT


view_Dialog_AddProject : Model -> Html Msg
view_Dialog_AddProject model =
    Dialog.view
        []
        [ Dialog.title [] [ text "Add project" ]
        , Dialog.content []
            [ Textfield.render Mdl
                [ 6 ]
                model.mdl
                [ Options.onInput UI_Dialog_AddProject_UpdateName
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



-- VIEW:DIALOG:PLAN


view_Dialog_AddPlan : Cycle -> Model -> Html Msg
view_Dialog_AddPlan cycle model =
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
                                        [ Options.onToggle UI_Dialog_AddPlans_ToggleProjectByNameAllProjects
                                        , Toggles.value (areUI_Dialog_AddPlansAllSelected model)
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
                                                    [ Options.onToggle (UI_Dialog_AddPlans_ToggleProjectByName projectNameStr)
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


view_Dialog_RemovePlanConfirmation : PlanID -> Model -> Html Msg
view_Dialog_RemovePlanConfirmation planID model =
    Dialog.view
        []
        [ Dialog.title [] [ text "Remove plan" ]
        , Dialog.content [] [ Html.p [] [ text "Are you sure?" ] ]
        , Dialog.actions []
            [ Button.render Mdl
                [ 902 ]
                model.mdl
                [ Dialog.closeOn "click"
                , Options.onClick (RemovePlanWithConfirmation planID)
                ]
                [ text "Remove" ]
            , Button.render Mdl
                [ 901 ]
                model.mdl
                [ Dialog.closeOn "click"
                , Options.onClick UI_Dialog_Reset
                ]
                [ text "Cancel" ]
            ]
        ]



-- VIEW:DIALOG:ASSIGNMENTS


view_Dialog_EditAssignments : PlanID -> RoleID -> Model -> Html Msg
view_Dialog_EditAssignments planID roleID model =
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
                                        [ Options.onToggle UI_Dialog_EditAssignments_ToggleAllPersons
                                        , Toggles.value (areUI_Dialog_EditAssignmentsAllSelected model)
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
                                                    [ Options.onToggle (UI_Dialog_EditAssignments_TogglePersonByName pn)
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



-- VIEW:DIALOG:STATUS_RECORD


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
                , Select.value <| statusReadForID model.ui.dialog.addStatusRecord_SelectedStatusID model
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
-- COMMANDS:SET_DATE


setDate : Cmd Msg
setDate =
    Task.perform SetDate Date.now



-- COMMANDS:FETCH_LATEST_STATE


fetchLatestState : Cmd Msg
fetchLatestState =
    Http.get getLatestStateUrl fetchLatestStatePayloadDecoder
        |> Http.send FetchLatestStateDone


fetchLatestStatePayloadDecoder : Json.Decode.Decoder State
fetchLatestStatePayloadDecoder =
    Json.Decode.field "state" stateDecoder


stateDecoder : Json.Decode.Decoder State
stateDecoder =
    Json.Decode.map6 State
        (Json.Decode.field "roles" (Json.Decode.list roleDecoder))
        (Json.Decode.field "statuses" (Json.Decode.list statusDecoder))
        (Json.Decode.field "cycles" (Json.Decode.list cycleDecoder))
        (Json.Decode.field "persons" (Json.Decode.list personDecoder))
        (Json.Decode.field "plans" (Json.Decode.list planDecoder))
        (Json.Decode.field "projects" (Json.Decode.list projectDecoder))


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


getLatestStateUrl : String
getLatestStateUrl =
    "http://localhost:8081/states/latest"



-- COMMANDS:STORE_STATE


storeState : State -> Cmd Msg
storeState state =
    let
        payloadJson =
            Json.Encode.object
                [ ( "state", state |> stateEncoder ) ]

        body =
            Http.stringBody "application/json" (Json.Encode.encode 0 payloadJson)
    in
        Http.toTask (createRequest storeStateUrl body)
            |> Task.attempt StoreStateDone


storeStateUrl : String
storeStateUrl =
    "http://localhost:8081/states/"


stateEncoder : State -> Json.Encode.Value
stateEncoder state =
    Json.Encode.object
        [ ( "roles", Json.Encode.list <| List.map roleEncoder state.roles )
        , ( "statuses", Json.Encode.list <| List.map statusEncoder state.statuses )
        , ( "cycles", Json.Encode.list <| List.map cycleEncoder state.cycles )
        , ( "persons", Json.Encode.list <| List.map personEncoder state.persons )
        , ( "plans", Json.Encode.list <| List.map planEncoder state.plans )
        , ( "projects", Json.Encode.list <| List.map projectEncoder state.projects )
        ]


roleEncoder : Role -> Json.Encode.Value
roleEncoder role =
    Json.Encode.object
        [ ( "id", Json.Encode.string <| roleIDToString role.id )
        , ( "read", Json.Encode.string role.read )
        , ( "icon", Json.Encode.string role.icon )
        ]


statusEncoder : Status -> Json.Encode.Value
statusEncoder status =
    Json.Encode.object
        [ ( "id", Json.Encode.string <| statusIDToString status.id )
        , ( "read", Json.Encode.string status.read )
        , ( "icon", Json.Encode.string status.icon )
        ]


cycleEncoder : Cycle -> Json.Encode.Value
cycleEncoder cycle =
    Json.Encode.object
        [ ( "index", Json.Encode.int <| cycleIndexToInt cycle.index ) ]


personEncoder : Person -> Json.Encode.Value
personEncoder person =
    Json.Encode.object
        [ ( "name", Json.Encode.string person.name )
        , ( "roleID", Json.Encode.string (roleIDToString person.roleID) )
        ]


planEncoder : Plan -> Json.Encode.Value
planEncoder plan =
    Json.Encode.object
        [ ( "id", planIDEncoder plan.id )
        , ( "assignments", Json.Encode.list <| List.map assignmentEncoder plan.assignments )
        , ( "statusRecords", Json.Encode.list <| List.map statusRecordEncoder plan.statusRecords )
        ]


planIDEncoder : PlanID -> Json.Encode.Value
planIDEncoder { cycleIndex, projectName } =
    Json.Encode.object
        [ ( "cycleIndex", Json.Encode.int (cycleIndexToInt cycleIndex) )
        , ( "projectName", Json.Encode.string (projectNameToString projectName) )
        ]


assignmentEncoder : Assignment -> Json.Encode.Value
assignmentEncoder assignment =
    Json.Encode.object
        [ ( "personName", Json.Encode.string assignment.personName ) ]


statusRecordEncoder : StatusRecord -> Json.Encode.Value
statusRecordEncoder statusRecord =
    Json.Encode.object
        [ ( "id", Json.Encode.string <| statusIDToString statusRecord.statusID )
        , ( "description", Json.Encode.string statusRecord.description )
        , ( "date", Json.Encode.string (maybeDateAsString (Just statusRecord.date)) )
        ]


projectEncoder : Project -> Json.Encode.Value
projectEncoder { name } =
    Json.Encode.object
        [ ( "name", Json.Encode.string (projectNameToString name) ) ]


createRequest : String -> Http.Body -> Http.Request ()
createRequest url body =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = url
        , body = body
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Cmd.batch [ setDate, fetchLatestState ] )
        , view = view
        , update = update
        , subscriptions = Material.subscriptions Mdl
        }



-- UTILS


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl msg ->
            "BadUrl: " ++ msg

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus _ ->
            "BadStatus"

        Http.BadPayload msg _ ->
            "BadPayload: " ++ msg


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
