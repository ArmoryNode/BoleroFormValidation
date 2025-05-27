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
    | RemoveUser of string
    | UserRemoved of User list

type Message =
    | RegistrationMessage of RegistrationMessage
    | UserMessage of UserMessage
    | ClearValidation of string option
    | ResetForm
    | ToggleValidationSummary

type FormTemplates = Template<"Templates/form.html">

let clearValidationCmd = Cmd.ofMsg << ClearValidation << Some

let updateRegistration message model =
    match message with
    | UpdateUserName userName ->
        { model with
            PageModel.UserRegistration.UserName = userName }
    | UpdateEmail email ->
        { model with
            PageModel.UserRegistration.Email = email }
    | UpdatePassword password ->
        { model with
            PageModel.UserRegistration.Password = password }
    | UpdateConfirmPassword password ->
        { model with
            PageModel.UserRegistration.ConfirmPassword = password }
    | ValidateUserRegistration registration ->
        let messages = Validators.validateUserRegistration registration

        if Map.isEmpty messages then
            model
        else
            { model with
                RegistrationValid = Invalid messages }
    | RegisterUser _ -> model
    | RegisteredUser result ->
        let model =
            { model with
                RegistrationValid = result }

        match result with
        | Valid ->
            { model with
                UserRegistration = UserRegistration.Empty }
        | NotValidated
        | Invalid _ -> model

let withRegistrationCommands remote message model =
    match message with
    | UpdateUserName _ -> model, clearValidationCmd (nameof model.UserRegistration.UserName)
    | UpdateEmail _ -> model, clearValidationCmd (nameof model.UserRegistration.Email)
    | UpdatePassword _ -> model, clearValidationCmd (nameof model.UserRegistration.Password)
    | UpdateConfirmPassword _ -> model, clearValidationCmd (nameof model.UserRegistration.ConfirmPassword)
    | ValidateUserRegistration registration ->
        Validators.validateUserRegistration registration
        |> Map.isEmpty
        |> function
            | true -> model, Cmd.ofMsg (RegistrationMessage(RegisterUser registration))
            | false -> model, Cmd.none
    | RegisterUser person -> model, Cmd.OfAsync.perform remote.registerUser person (RegistrationMessage << RegisteredUser)
    | RegisteredUser result ->
        match result with
        | Valid -> model, Cmd.OfAsync.perform remote.getRegisteredUsers () (UserMessage << GotRegisteredUsers)
        | NotValidated
        | Invalid _ -> model, Cmd.none

let userUpdate remote message model =
    match message with
    | GetRegisteredUsers ->
        { model with Loaded = false }, Cmd.OfAsync.perform remote.getRegisteredUsers () (UserMessage << GotRegisteredUsers)
    | GotRegisteredUsers users ->
        { model with
            RegisteredUsers = users
            Loaded = true },
        Cmd.none
    | RemoveUser userName -> { model with Loaded = false }, Cmd.OfAsync.perform remote.removeUser userName (UserMessage << UserRemoved)
    | UserRemoved userList ->
        { model with
            Loaded = true
            RegisteredUsers = userList },
        Cmd.none

let update remote message model =
    match message with
    | RegistrationMessage message -> model |> updateRegistration message |> withRegistrationCommands remote message
    | UserMessage message -> userUpdate remote message model
    | ResetForm ->
        { model with
            UserRegistration = UserRegistration.Empty },
        Cmd.ofMsg << ClearValidation <| None
    | ClearValidation fieldName ->
        match model.RegistrationValid with
        | Valid
        | NotValidated -> model, Cmd.none
        | Invalid messages ->
            match fieldName with
            | None ->
                { model with
                    RegistrationValid = NotValidated },
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
                        | Invalid messages ->
                            ul {
                                let messages = messages |> Validation.getMessages

                                for message in messages do
                                    li {
                                        attr.``class`` "validation-error"
                                        text message
                                    }
                            }
                        | NotValidated
                        | Valid -> empty ()
                }
            | false -> empty ()
    }

let requiredTag (text: string) =
    FormTemplates.RequiredTag().Text(text).Elt()

let registeredUsers model dispatch =
    div {
        cond model.Loaded
        <| function
            | true ->
                match model.RegisteredUsers with
                | [] ->
                    div {
                        attr.``class`` "text-muted"
                        text "No users registered yet."
                    }
                | _ ->
                    forEach model.RegisteredUsers
                    <| fun user ->
                        li {
                            text $"%s{user.UserName}: %s{user.Email}"

                            button {
                                on.click (fun _ -> dispatch << UserMessage << RemoveUser <| user.UserName)

                                attr.``type`` "button"
                                attr.``class`` "delete-user"
                                text "\u0020❌"
                            }
                        }
            | false ->
                div {
                    attr.style "position: relative; min-height: 50px;"

                    FormTemplates.Loader().Elt()
                }
    }


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

                cond model.RegistrationValid
                <| function
                    | Valid ->
                        div {
                            attr.``class`` "validation-success"
                            text "User registration successful!"
                        }
                    | NotValidated
                    | Invalid _ -> empty ()
            }
        )
        .OnSubmit(fun _ -> dispatch << RegistrationMessage << ValidateUserRegistration <| model.UserRegistration)
        .OnReset(fun _ -> dispatch ResetForm)
        .ValidationSummary(generateValidationSummary model.RegistrationValid model.ShowValidationSummary dispatch)
        .RegisteredUsers(registeredUsers model dispatch)
        .Elt()

type MyApp() =
    inherit ProgramComponent<PageModel, Message>()

    override this.Program =
        let personService = this.Remote<RegistrationService>()
        let update = update personService

        let command =
            Cmd.OfAsync.perform personService.getRegisteredUsers () (UserMessage << GotRegisteredUsers)

        Program.mkProgram (fun _ -> PageModel.Default, command) update view
#if DEBUG
        |> Program.withHotReload
#endif
