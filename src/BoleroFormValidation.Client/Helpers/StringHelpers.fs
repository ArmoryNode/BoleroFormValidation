// SPDX-License-Identifier: MIT
namespace BoleroFormValidation.Client.Helpers

module StringHelpers =
    let handleNullOrEmpty (input: string) =
        match input with
        | null -> ""
        | s when s.Trim() = "" -> ""
        | _ -> input
