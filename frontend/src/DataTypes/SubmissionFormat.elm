module DataTypes.SubmissionFormat exposing (..)

import Json.Decode exposing (Decoder, andThen, string, succeed)


type SubmissionFormat
    = Presentation20
    | Presentation40
    | LightningTalk
    | Workshop
    | Exhibition
    | PechaKucha
    | VideoExisting
    | VideoNew
    | TheGoodTalk
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
        "presentation-20-min" ->
            Presentation20

        "presentation-40-min" ->
            Presentation40

        "lightning-talk" ->
            LightningTalk

        "workshop" ->
            Workshop

        "exhibition" ->
            Exhibition

        "pecha-kucha" ->
            PechaKucha

        "video-existing" ->
            VideoExisting

        "video-new" ->
            VideoNew

        "den-gode-samtalen" ->
            TheGoodTalk

        "whatever-you-want" ->
            WhateverYouWant

        somethingElse ->
            Unknown somethingElse


toSlug : SubmissionFormat -> String
toSlug s =
    case s of
        Presentation20 ->
            "presentation-20-min"

        Presentation40 ->
            "presentation-40-min"

        LightningTalk ->
            "lightning-talk"

        Workshop ->
            "workshop"

        Exhibition ->
            "exhibition"

        PechaKucha ->
            "pecha-kucha"

        VideoExisting ->
            "video-existing"

        VideoNew ->
            "video-new"

        TheGoodTalk ->
            "den-gode-samtalen"

        WhateverYouWant ->
            "whatever-you-want"

        Unknown format ->
            format


toString : SubmissionFormat -> String
toString s =
    case s of
        Presentation20 ->
            "Foredrag, 20 minutter"

        Presentation40 ->
            "Foredrag, 40 minutter"

        LightningTalk ->
            "Lyntale"

        Workshop ->
            "Workshop"

        Exhibition ->
            "Prosjektutstilling"

        PechaKucha ->
            "Pecha Kucha"

        VideoExisting ->
            "Video - forslag på eksisterende"

        VideoNew ->
            "Video - lage ny"

        TheGoodTalk ->
            "Den gode samtalen"

        WhateverYouWant ->
            "Hva du vil! Finn på noe kult"

        Unknown format ->
            format
