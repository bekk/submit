module DataTypes.SubmissionFormat exposing (..)

import Json.Decode exposing (Decoder, andThen, string, succeed)


type SubmissionFormat
    = Presentation
    | LightningTalk
    | Workshop
    | ExhibitionProject
    | ExhibitionHobby
    | PechaKucha
    | VideoExternal
    | VideoInternal
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

        "exhibition-project" ->
            ExhibitionProject

        "exhibition-hobby" ->
            ExhibitionHobby

        "pecha-kucha" ->
            PechaKucha

        "video-external" ->
            VideoExternal

        "video-internal" ->
            VideoInternal

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

        ExhibitionProject ->
            "exhibition-project"

        ExhibitionHobby ->
            "exhibition-hobby"

        PechaKucha ->
            "pecha-kucha"

        VideoExternal ->
            "video-eksternal"

        VideoInternal ->
            "video-internal"

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

        ExhibitionProject ->
            "Utstilling fra jobbprosjekt"

        ExhibitionHobby ->
            "Utstilling fra hobbyprosjekt"

        PechaKucha ->
            "Pecha Kucha"

        VideoExternal ->
            "Video - fra internett"

        VideoInternal ->
            "Video - din egen video"

        WhateverYouWant ->
            "Hva du vil! Finn på noe kult"

        Unknown format ->
            format
