module DataTypes.SubmissionFormat exposing (..)

import Json.Decode exposing (Decoder, andThen, string, succeed)


type SubmissionFormat
    = Presentation
    | LightningTalk
    | Workshop
    | Exhibition
    | PechaKucha
    | Video
    | WhateverYouWant
    | Unknown String


decode : Decoder SubmissionFormat
decode =
    let
        convert : String -> Decoder SubmissionFormat
        convert raw =
            raw |> fromSlug |> succeed
    in
        string |> andThen convert


fromSlug : String -> SubmissionFormat
fromSlug raw =
    case raw of
        "presentation" ->
            Presentation

        "lightning-talk" ->
            LightningTalk

        "workshop" ->
            Workshop

        "exhibition" ->
            Exhibition

        "pecha-kucha" ->
            PechaKucha

        "video" ->
            Video

        "whatever-you-want" ->
            WhateverYouWant

        somethingElse ->
            Unknown somethingElse


toSlug : SubmissionFormat -> String
toSlug s =
    case s of
        Presentation ->
            "presentation"

        LightningTalk ->
            "lightning-talk"

        Workshop ->
            "workshop"

        Exhibition ->
            "exhibition"

        PechaKucha ->
            "pecha-kucha"

        Video ->
            "video"

        WhateverYouWant ->
            "whatever-you-want"

        Unknown format ->
            format


toString : SubmissionFormat -> String
toString s =
    case s of
        Presentation ->
            "Foredrag"

        LightningTalk ->
            "Lyntale"

        Workshop ->
            "Workshop"

        Exhibition ->
            "Utstilling"

        PechaKucha ->
            "Pecha Kucha"

        Video ->
            "Video"

        WhateverYouWant ->
            "Hva du vil! Finn på noe kult"

        Unknown format ->
            format
