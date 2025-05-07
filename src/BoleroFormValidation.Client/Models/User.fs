// SPDX-License-Identifier: MIT
namespace BoleroFormValidation.Client.Models

open System

type User =
    { UserName: string
      Email: string }

    static member Empty =
        { UserName = String.Empty
          Email = String.Empty }
