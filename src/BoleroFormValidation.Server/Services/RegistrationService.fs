// SPDX-License-Identifier: MIT
namespace BoleroFormValidation.Server.Services

open System
open Bolero.Remoting.Server
open BoleroFormValidation.Client.Models
open BoleroFormValidation.Client.Validation

module Store =
    let registeredUsers =
        Collections.Generic.Dictionary(StringComparer.OrdinalIgnoreCase)

type RegistrationService() =
    inherit RemoteHandler<BoleroFormValidation.Client.Services.RegistrationService>()

    let validateUniqueUsername registration messages =
        let fieldName = nameof registration.UserName

        Validation.tryAppendMessage fieldName messages
        <| match Store.registeredUsers.ContainsKey registration.UserName with
           | true -> Some "Username is already taken"
           | false -> None

    let validateUniqueEmail registration messages =
        let fieldName = nameof registration.Email

        Validation.tryAppendMessage fieldName messages
        <| match Store.registeredUsers.ContainsValue registration.Email with
           | true -> Some "Email address is already registered"
           | false -> None

    override this.Handler =
        { registerUser =
            fun userRegistration ->
                async {
                    let validationMessages =
                        Validators.validateUserRegistration userRegistration
                        |> validateUniqueUsername userRegistration
                        |> validateUniqueEmail userRegistration

                    if Map.isEmpty validationMessages then
                        Store.registeredUsers.Add(userRegistration.UserName, userRegistration.Email)
                        return Valid
                    else
                        return Invalid validationMessages
                }
          getRegisteredUsers =
            fun () ->
                async {
                    return
                        Store.registeredUsers
                        |> Seq.map (fun userMap ->
                            { UserName = userMap.Key
                              Email = userMap.Value })
                        |> Seq.toList
                } }
