// SPDX-License-Identifier: MIT
namespace BoleroFormValidation.Client.Helpers

module StringHelpers =
    /// <summary>
    /// Maps null or whitespace strings to an empty string.
    /// </summary>
    /// <param name="input">The input string</param>
    /// <returns>An empty string if null or whitespace, otherwise returns the <see cref="input"/> string</returns>
    let handleNullOrEmpty (input: string) =
        match input with
        | null -> ""
        | s when s.Trim() = "" -> ""
        | _ -> input
