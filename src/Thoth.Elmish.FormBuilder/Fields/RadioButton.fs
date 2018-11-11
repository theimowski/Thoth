namespace Thoth.Elmish.FormBuilder.Fields

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Thoth.Elmish.FormBuilder.Types
open System
open Thoth.Json

module FormCmd = Thoth.Elmish.FormBuilder.Cmd

[<RequireQualifiedAccess>]
module RadioButton =

    type Key = string

    type State =
        { Label : string
          SelectedKey : Key option
          Values : (Key * string) list
          Group : string
          Validators : Validator list
          ValidationState : ValidationState
          JsonLabel : string option }

    and Validator = State -> ValidationState

    type Msg =
        | ChangeValue of string
        interface IFieldMsg

    let private init (state : FieldState) =
        state, FormCmd.none

    let private update (msg : FieldMsg) (state : FieldState) =
        let msg = msg :?> Msg
        let state = state :?> State

        match msg with
        | ChangeValue key ->
            box { state with SelectedKey = Some key }, FormCmd.none

    let private renderRadio (group : string) (selectedKey : Key option) (onChange : IFieldMsg -> unit) (key, value) =
        Radio.radio [ Props [ Prop.Key key ] ]
            [ Radio.input [ Radio.Input.Props [ OnChange (fun _ -> ChangeValue key |> onChange)
                                                selectedKey
                                                |> Option.map (fun cKey -> cKey = key)
                                                |> Option.defaultValue false
                                                |> Checked ]
                            Radio.Input.Name group ]
              str value ]

    let private render (state : FieldState) (onChange : IFieldMsg -> unit) =
        let state : State = state :?> State

        Field.div [ ]
            [ Label.label [ ]
                [ str state.Label ]
              Control.div [ ]
                [ state.Values
                  |> List.map (renderRadio state.Group state.SelectedKey onChange)
                  |> ofList ]
            //   Help.help [ Help.Color IsDanger ]
            //     [ str state.ValidationInputState.ToText ]
                ]

    let private validate (state : FieldState) =
        let state : State = state :?> State
        let rec applyValidators (validators : Validator list) (state : State) =
            match validators with
                | validator::rest ->
                    match validator state with
                    | Valid -> applyValidators rest state
                    | Invalid msg ->
                        { state with ValidationState = Invalid msg }
                | [] -> state

        applyValidators state.Validators state
        |> toFieldState

    let private isValid (state : FieldState) =
        let state : State = state :?> State
        state.ValidationState = Valid

    let private toJson (state : FieldState) =
        let state : State = state :?> State
        state.JsonLabel
            |> Option.defaultValue state.Label, state.SelectedKey
                                                |> Option.map Encode.string
                                                |> Option.defaultValue Encode.nil

    let config : FieldConfig =
        { Render = render
          Update = update
          Init = init
          Validate = validate
          IsValid = isValid
          ToJson = toJson }

    let create (label : string) : State =
        { Label = label
          SelectedKey = None
          Values = []
          Group = (Guid.NewGuid()).ToString()
          Validators = [ ]
          ValidationState = Valid
          JsonLabel = None }

    let withValues (values : (Key * string) list) (state : State) =
        { state with Values = values }

    let withSelectedKey (key : Key ) (state : State) =
        { state with SelectedKey = Some key }

    let withDefaultRenderer (state : State) : Field =
        { Type = "default-radio-button"
          State = state
          Guid = Guid.NewGuid() }
