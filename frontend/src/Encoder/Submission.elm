module Encoder.Submission exposing (encoder, encodeComment)

import Model.Submission exposing (..)
import Json.Encode exposing (Value, object, string, list)
import DataTypes.SubmissionFormat as SubFormat


encoder : Submission -> Value
encoder submission =
    object
        [ ( "status", string submission.status )
        , ( "title", string submission.title )
        , ( "abstract", string submission.abstract )
        , ( "intendedAudience", string submission.intendedAudience )
        , ( "equipment", string submission.equipment )
        , ( "format", string <| SubFormat.toSlug submission.format )
        , ( "language", string submission.language )
        , ( "length", string submission.length )
        , ( "outline", string submission.outline )
        , ( "level", string submission.level )
        , ( "suggestedKeywords", string submission.suggestedKeywords )
        , ( "infoToProgramCommittee", string submission.infoToProgramCommittee )
        , ( "speakers", list <| List.map encodeSpeaker submission.speakers )
        , ( "status", string submission.status )
        ]


encodeSpeaker : ( Int, Speaker ) -> Value
encodeSpeaker ( i, speaker ) =
    object
        [ ( "id", string speaker.id )
        , ( "name", string speaker.name )
        , ( "email", string speaker.email )
        , ( "bio", string speaker.bio )
        , ( "zipCode", string speaker.zipCode )
        , ( "twitter", string speaker.twitter )
        ]


encodeComment : Model.Submission.Model -> Value
encodeComment model =
    object
        [ ( "comment", string model.comment ) ]
