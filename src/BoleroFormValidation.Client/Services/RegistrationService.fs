// SPDX-License-Identifier: MIT
namespace BoleroFormValidation.Client.Services

open Bolero.Remoting
open BoleroFormValidation.Client.Models

type RegistrationService =
    { registerUser: UserRegistration -> Async<ValidationResult>
      getRegisteredUsers: unit -> Async<User list> }

    interface IRemoteService with
        member this.BasePath = "/api/registration"
