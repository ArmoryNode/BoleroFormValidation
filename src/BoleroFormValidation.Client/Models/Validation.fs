// SPDX-License-Identifier: MIT
namespace BoleroFormValidation.Client.Models

type ValidationResult =
    | Valid
    | Invalid of Map<string, string list>

    static member Default = Invalid Map.empty

module Validation =
    let appendMessage fieldName map message =
        Map.tryFind fieldName map
        |> function
            | Some messages -> map |> Map.add fieldName (message :: messages)
            | None -> map |> Map.add fieldName [ message ]

    let tryAppendMessage fieldName map message =
        match message with
        | Some msg -> appendMessage fieldName map msg
        | None -> map

    let tryFind fieldName map =
        map |> Map.tryFind fieldName |> Option.bind List.tryHead

    let getMessage fieldName validationResult =
        match validationResult with
        | Valid -> ""
        | Invalid messages -> messages |> tryFind fieldName |> Option.defaultValue ""

    let getMessages map = map |> Map.values |> Seq.collect id

    let containsField fieldName validationResult =
        match validationResult with
        | Valid -> false
        | Invalid messages -> messages |> Map.containsKey fieldName
