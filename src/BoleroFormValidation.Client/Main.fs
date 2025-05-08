// SPDX-License-Identifier: MIT
module BoleroFormValidation.Client.Main

open Bolero.Html
open Bolero.Remoting
open BoleroFormValidation.Client.Models
open BoleroFormValidation.Client.Services
open BoleroFormValidation.Client.Validation
open Elmish
open Bolero
open Bolero.Templating.Client

type PageModel =
    { UserRegistration: UserRegistration
      RegistrationValid: ValidationResult
      ShowValidationSummary: bool
      RegisteredUsers: User list }

let initModel =
    { UserRegistration = UserRegistration.Empty
      RegistrationValid = ValidationResult.Default
      ShowValidationSummary = true
      RegisteredUsers = [] }

type RegistrationMessage =
    | UpdateUserName of string
    | UpdateEmail of string
    | UpdatePassword of string
    | UpdateConfirmPassword of string
    | ValidateUserRegistration of UserRegistration
    | RegisterUser of UserRegistration
    | RegisteredUser of ValidationResult

type UserMessage =
    | GetRegisteredUsers
    | GotRegisteredUsers of User list

type Message =
    | RegistrationMessage of RegistrationMessage
    | UserMessage of UserMessage
    | ClearValidation of string option
    | ResetForm
    | ToggleValidationSummary

type FormTemplates = Template<"Templates/form.html">

let registrationUpdate remote message model =
    let clearValidationCmd = Cmd.ofMsg << ClearValidation << Some

    match message with
    | UpdateUserName userName ->
        { model with
            PageModel.UserRegistration.UserName = userName },
        clearValidationCmd (nameof model.UserRegistration.UserName)
    | UpdateEmail email ->
        { model with
            PageModel.UserRegistration.Email = email },
        clearValidationCmd (nameof model.UserRegistration.Email)
    | UpdatePassword password ->
        { model with
            PageModel.UserRegistration.Password = password },
        clearValidationCmd (nameof model.UserRegistration.Password)
    | UpdateConfirmPassword password ->
        { model with
            PageModel.UserRegistration.ConfirmPassword = password },
        clearValidationCmd (nameof model.UserRegistration.ConfirmPassword)
    | ValidateUserRegistration registration ->
        let messages = Validators.validateUserRegistration registration

        match Map.isEmpty messages with
        | true -> model, Cmd.ofMsg << RegistrationMessage << RegisterUser <| registration
        | false ->
            { model with
                RegistrationValid = Invalid messages },
            Cmd.none
    | RegisterUser person -> model, Cmd.OfAsync.perform remote.registerUser person (RegistrationMessage << RegisteredUser)
    | RegisteredUser result ->
        let model =
            { model with
                RegistrationValid = result }

        match result with
        | Valid ->
            { model with
                UserRegistration = UserRegistration.Empty },
            Cmd.OfAsync.perform remote.getRegisteredUsers () (UserMessage << GotRegisteredUsers)
        | Invalid _ -> model, Cmd.none

let userUpdate remote message model =
    match message with
    | GetRegisteredUsers -> model, Cmd.OfAsync.perform remote.getRegisteredUsers () (UserMessage << GotRegisteredUsers)
    | GotRegisteredUsers users -> { model with RegisteredUsers = users }, Cmd.none

let update remote message model =
    match message with
    | RegistrationMessage message -> registrationUpdate remote message model
    | UserMessage message -> userUpdate remote message model
    | ResetForm ->
        { model with
            UserRegistration = UserRegistration.Empty },
        Cmd.ofMsg << ClearValidation <| None
    | ClearValidation fieldName ->
        match model.RegistrationValid with
        | Valid -> model, Cmd.none
        | Invalid messages ->
            match fieldName with
            | None ->
                { model with
                    RegistrationValid = ValidationResult.Default },
                Cmd.none
            | Some fieldName ->
                let messages = messages |> Map.filter (fun k _ -> k <> fieldName)

                { model with
                    RegistrationValid = Invalid messages },
                Cmd.none
    | ToggleValidationSummary ->
        { model with
            ShowValidationSummary = not model.ShowValidationSummary },
        Cmd.none


let getValidationClass fieldName validationResult =
    if Validation.containsField fieldName validationResult then
        "invalid"
    else
        ""

let generateValidationSummary validationResult showSummary dispatch =
    div {
        button {
            attr.``type`` "button"

            on.click (fun _ -> dispatch ToggleValidationSummary)

            cond showSummary
            <| function
                | true -> text "⍉ Hide Summary"
                | false -> text "👁 Show Summary"
        }

        cond showSummary
        <| function
            | true ->
                div {
                    cond validationResult
                    <| function
                        | Valid ->
                            div {
                                attr.``class`` "validation-success"
                                text "User registration successful!"
                            }
                        | Invalid messages ->
                            ul {
                                let messages = messages |> Validation.getMessages

                                for message in messages do
                                    li {
                                        attr.``class`` "validation-error"
                                        text message
                                    }
                            }
                }
            | false -> empty ()
    }

let requiredTag (text: string) =
    FormTemplates.RequiredTag().Text(text).Elt()

let view model dispatch =
    let userNameFieldName = nameof Unchecked.defaultof<UserRegistration>.UserName
    let emailFieldName = nameof Unchecked.defaultof<UserRegistration>.Email
    let passwordFieldName = nameof Unchecked.defaultof<UserRegistration>.Password

    let confirmPasswordFieldName =
        nameof Unchecked.defaultof<UserRegistration>.ConfirmPassword

    FormTemplates
        .RegisterUserForm()
        .FormBody(
            div {
                div {
                    attr.``class`` "form-row"

                    FormTemplates
                        .ValidatableInput()
                        .FieldName(userNameFieldName)
                        .Label(requiredTag "Username")
                        .InputClasses(getValidationClass userNameFieldName model.RegistrationValid)
                        .Placeholder("Enter username...")
                        .Value(model.UserRegistration.UserName, (dispatch << RegistrationMessage << UpdateUserName))
                        .Hint("🛈 Usernames must be at least 3 characters long and can only contain letters, numbers, and underscores.")
                        .ValidationMessage(Validation.getMessage userNameFieldName model.RegistrationValid)
                        .Elt()

                    FormTemplates
                        .ValidatableInput()
                        .FieldName(emailFieldName)
                        .Label(requiredTag "Email")
                        .InputClasses(getValidationClass emailFieldName model.RegistrationValid)
                        .Placeholder("Enter email...")
                        .Value(model.UserRegistration.Email, (dispatch << RegistrationMessage << UpdateEmail))
                        .ValidationMessage(Validation.getMessage emailFieldName model.RegistrationValid)
                        .Elt()
                }

                FormTemplates
                    .ValidatableInput()
                    .Type("password")
                    .FieldName(passwordFieldName)
                    .Label(requiredTag "Password")
                    .InputClasses(getValidationClass passwordFieldName model.RegistrationValid)
                    .Placeholder("Enter password...")
                    .Value(model.UserRegistration.Password, (dispatch << RegistrationMessage << UpdatePassword))
                    .Hint("🛈 Passwords must be at least 6 characters long.")
                    .ValidationMessage(Validation.getMessage passwordFieldName model.RegistrationValid)
                    .Elt()

                FormTemplates
                    .ValidatableInput()
                    .Type("password")
                    .FieldName(confirmPasswordFieldName)
                    .Label(requiredTag "Confirm Password")
                    .InputClasses(getValidationClass confirmPasswordFieldName model.RegistrationValid)
                    .Placeholder("Re-enter password...")
                    .Value(model.UserRegistration.ConfirmPassword, (dispatch << RegistrationMessage << UpdateConfirmPassword))
                    .ValidationMessage(Validation.getMessage confirmPasswordFieldName model.RegistrationValid)
                    .Elt()
            }
        )
        .OnSubmit(fun _ -> dispatch << RegistrationMessage << ValidateUserRegistration <| model.UserRegistration)
        .OnReset(fun _ -> dispatch ResetForm)
        .ValidationSummary(generateValidationSummary model.RegistrationValid model.ShowValidationSummary dispatch)
        .RegisteredUsers(forEach model.RegisteredUsers <| fun user -> li { $"%s{user.UserName}: %s{user.Email}" })
        .Elt()

type MyApp() =
    inherit ProgramComponent<PageModel, Message>()

    override this.Program =
        let personService = this.Remote<RegistrationService>()
        let update = update personService

        let command =
            Cmd.OfAsync.perform personService.getRegisteredUsers () (UserMessage << GotRegisteredUsers)

        Program.mkProgram (fun _ -> initModel, command) update view
#if DEBUG
        |> Program.withHotReload
#endif
