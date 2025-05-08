module BoleroFormValidation.Client.Validation.RegexPatterns

[<Literal>]
let legalCharacters = @"^[\w]+$"

[<Literal>]
let beginsWithLetter = @"^[a-zA-Z]"

[<Literal>]
let email = @"\A[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\z"