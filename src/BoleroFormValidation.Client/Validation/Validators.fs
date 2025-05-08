// SPDX-License-Identifier: MIT
namespace BoleroFormValidation.Client.Validation

open System.Text.RegularExpressions
open BoleroFormValidation.Client.Helpers
open BoleroFormValidation.Client.Models

module private Patterns =
    let private isMatch pattern (input: string) =
        Regex.IsMatch(input, pattern, RegexOptions.Compiled)

    let hasIllegalCharacters input = not <| isMatch RegexPatterns.legalCharacters input
    let beginsWithNonLetter input = not <| isMatch RegexPatterns.beginsWithLetter input
    let emailInvalid input = not <| isMatch RegexPatterns.email input

module Validators =
    let validateUserName (registration: UserRegistration) messages =
        Validation.tryAppendMessage (nameof registration.UserName) messages
        <| match (registration.UserName |> StringHelpers.handleNullOrEmpty) with
            | "" -> Some "Username cannot be empty"
            | u when u.Length < 3 -> Some "Username must be at least 3 characters long"
            | u when Patterns.hasIllegalCharacters u -> Some "Username must only contain letters, numbers, and underscores"
            | u when Patterns.beginsWithNonLetter u -> Some "Username must begin with a letter"
            | _ -> None

    let validateEmail (registration: UserRegistration) messages =
        Validation.tryAppendMessage (nameof registration.Email) messages
        <| match (registration.Email |> StringHelpers.handleNullOrEmpty) with
            | "" -> Some "Email cannot be empty"
            | e when Patterns.emailInvalid e -> Some "Email is not valid"
            | _ -> None

    let validatePassword (registration: UserRegistration) messages =
        Validation.tryAppendMessage (nameof registration.Password) messages
        <| match (registration.Password |> StringHelpers.handleNullOrEmpty) with
            | "" -> Some "Password cannot be empty"
            | p when p.Length < 6 -> Some "Password must be at least 6 characters long"
            | _ -> None

    let validateConfirmPassword (registration: UserRegistration) messages =
        Validation.tryAppendMessage (nameof registration.ConfirmPassword) messages
        <| match (registration.ConfirmPassword |> StringHelpers.handleNullOrEmpty) with
            | "" -> Some "Password confirmation cannot be empty"
            | c when c <> registration.Password -> Some "Passwords do not match"
            | _ -> None

    let validateUserRegistration userRegistration =
        Map.empty
        |> validateUserName userRegistration
        |> validateEmail userRegistration
        |> validatePassword userRegistration
        |> validateConfirmPassword userRegistration
