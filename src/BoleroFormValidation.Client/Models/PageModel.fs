namespace BoleroFormValidation.Client.Models

type PageModel =
    { UserRegistration: UserRegistration
      RegistrationValid: ValidationResult
      ShowValidationSummary: bool
      RegisteredUsers: User list
      Loaded: bool }

    static member Default =
        { UserRegistration = UserRegistration.Empty
          RegistrationValid = NotValidated
          ShowValidationSummary = true
          RegisteredUsers = []
          Loaded = false }
