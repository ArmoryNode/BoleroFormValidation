// SPDX-License-Identifier: MIT
module BoleroFormValidation.Server.Index

open Bolero.Html
open Bolero.Server.Html
open BoleroFormValidation

let page = doctypeHtml {
    head {
        meta { attr.charset "UTF-8" }
        meta { attr.name "viewport"; attr.content "width=device-width, initial-scale=1.0" }
        title { "Bolero Validation Demo" }
        ``base`` { attr.href "/" }
        link { attr.rel "stylesheet"; attr.href "css/main.css" }
    }
    body {
        div { attr.id "main"; comp<Client.Main.MyApp> }
        boleroScript
    }
}
