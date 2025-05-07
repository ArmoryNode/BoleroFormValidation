// SPDX-License-Identifier: MIT
namespace BoleroFormValidation.Client.Validation

open System.Text.RegularExpressions
open BoleroFormValidation.Client.Helpers
open BoleroFormValidation.Client.Models

module internal Patterns =
    [<Literal>]
    let legalCharacters = @"^[\w]+$"

    [<Literal>]
    let beginsWithLetter = @"^[a-zA-Z]"

    [<Literal>]
    let email =
        @"\A[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\z"

    let (|Match|_|) pattern str =
        let m = Regex.Match(str, pattern, RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
        if m.Success then Some m else None

    let hasIllegalCharacters input =
        match input with
        | Match legalCharacters _ -> false
        | _ -> true

    let beginsWithNonLetter input =
        match input with
        | Match beginsWithLetter _ -> false
        | _ -> true

    let emailInvalid input =
        match input with
        | Match email _ -> false
        | _ -> true

module Validators =
    module UserName =
        let validate registration errors =
            let username = registration.UserName |> StringHelpers.handleNullOrEmpty
            let fieldName = nameof registration.UserName

            Validation.tryAppendMessage fieldName errors
            <| match username with
               | "" -> Some "Username cannot be empty"
               | u when u.Length < 3 -> Some "Username must be at least 3 characters long"
               | u when Patterns.hasIllegalCharacters u -> Some "Username must only contain letters, numbers, and underscores"
               | u when Patterns.beginsWithNonLetter u -> Some "Username must being with a letter"
               | _ -> None

    module Email =
        let validate registration errors =
            let email = registration.Email |> StringHelpers.handleNullOrEmpty
            let fieldName = nameof registration.Email

            Validation.tryAppendMessage fieldName errors
            <| match email with
               | "" -> Some "Email cannot be empty"
               | e when Patterns.emailInvalid e -> Some "Email is not valid"
               | _ -> None

    module Password =
        let validate registration errors =
            let password = registration.Password |> StringHelpers.handleNullOrEmpty
            let fieldName = nameof registration.Password

            Validation.tryAppendMessage fieldName errors
            <| match password with
               | "" -> Some "Password cannot be empty"
               | p when p.Length < 6 -> Some "Password must be at least 6 characters long"
               | _ -> None

    module ConfirmPassword =
        let validate registration errors =
            let confirmPassword =
                registration.ConfirmPassword |> StringHelpers.handleNullOrEmpty

            let fieldName = nameof registration.ConfirmPassword

            Validation.tryAppendMessage fieldName errors
            <| match confirmPassword with
               | c when c <> registration.Password -> Some "Passwords do not match"
               | "" -> Some "Password confirmation cannot be empty"
               | _ -> None

    let validateUserRegistration userRegistration =
        Map.empty
        |> UserName.validate userRegistration
        |> Email.validate userRegistration
        |> Password.validate userRegistration
        |> ConfirmPassword.validate userRegistration
