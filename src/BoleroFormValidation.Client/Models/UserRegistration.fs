// SPDX-License-Identifier: MIT
namespace BoleroFormValidation.Client.Models

type UserRegistration =
    { UserName: string
      Email: string
      Password: string
      ConfirmPassword: string }

    static member Empty =
        { UserName = ""
          Email = ""
          Password = ""
          ConfirmPassword = "" }
