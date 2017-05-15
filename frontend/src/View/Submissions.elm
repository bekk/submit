module View.Submissions exposing (view)

import Model exposing (..)
import Model.Submissions exposing (..)
import Html exposing (Html, div, h1, h2, p, text, a, button, img)
import Html.Attributes exposing (class, href, src, title, disabled)
import Html.Events exposing (onClick)
import Nav.Nav exposing (toHash)
import Nav.Model
import Backend.Network exposing (RequestStatus(..))
import String
import Messages exposing (Msg(..))


view : Model -> Html Msg
view model =
    case model.submissions.submissions of
        Initial ->
            div [] []

        Loading ->
            viewWrapper [ div [ class "submissions-list loading" ] [ text "Loading..." ] ]

        Complete submissions ->
            viewWrapper <| viewSubmissions model submissions

        Error message ->
            viewWrapper <| viewError message


viewWrapper : List (Html Msg) -> Html Msg
viewWrapper content =
    div [ class "wrapper" ]
        [ div [ class "logo-wrapper" ] [ div [ class "logo" ] [] ]
        , div [ class "submissions-list" ]
            content
        ]


viewError : String -> List (Html Msg)
viewError message =
    [ text message ]


viewSubmissions : Model -> Submissions -> List (Html Msg)
viewSubmissions model submissions =
    let
        years =
            if List.length submissions.years == 0 then
                [ viewEmpty ]
            else
                List.map viewYear submissions.years

        introtext =
            if List.length submissions.years == 0 then
                text ""
            else
                p [ class "intro-text" ] [ text "These are all talks you have submitted or participated on. You can edit all talks from the currect active event. All earlier talks are also available for your reference." ]
    in
        [ div [ class "flex-header" ]
            [ h1 [ class "flex-header-element" ] [ text "Your Talks" ]
            , viewCreateSubmission model
            ]
        , introtext
        , div [ class "submissions" ] years
        ]


viewCreateSubmission : Model -> Html Msg
viewCreateSubmission model =
    if model.appConfig.submissionsOpen then
        div [ class "flex-header-element" ]
            [ button [ onClick SubmissionsCreateTalk, class "new-talk button-new" ] [ text "Create new draft" ]
            ]
    else
        div [ class "flex-header-element flex-header-element-vertical" ]
            [ button [ disabled True, class "new-talk button-new" ] [ text "Call for speakers is closed" ]
            , div [ class "disabled-text" ] [ text "(but you can still edit your already submitted ones!)" ]
            ]


viewEmpty : Html Msg
viewEmpty =
    div [ class "no-talks" ]
        [ img [ src "assets/robot.png", class "welcome-robot" ] []
        , h2 [] [ text "Blank slate, baby!" ]
        , p [] [ text "It looks like you don't have any talks submitted yet." ]
        , p [ class "last" ] [ text "Go ahead, create your first draft. You can keep working on it, and edit it again and again until you're happy. Then, you mark it for submission and off it goes." ]
        ]


viewYear : Year -> Html Msg
viewYear year =
    let
        submissions =
            List.map viewSubmission year.submissions
    in
        div [ class "submissions-year" ]
            [ h2 [] [ text year.year ]
            , div [] submissions
            ]


viewSubmission : Submission -> Html Msg
viewSubmission submission =
    div [ class "submission-wrapper" ]
        [ a [ class "submission", href << toHash <| Nav.Model.Submission submission.id ]
            [ div [ class <| "status-light status-light-" ++ String.toLower submission.status ] [ text "-" ]
            , div [ class "submission-details" ]
                [ div [ class "title" ] [ text submission.name ]
                , viewCommentIndicator submission
                , div [ class "status" ] [ text submission.status ]
                , div [ class "open-arrow" ] [ text "-" ]
                ]
            ]
        ]


viewCommentIndicator : Submission -> Html Msg
viewCommentIndicator submission =
    if hasComments submission then
        div [ class "has-comments", title <| toString (List.length submission.comments) ++ " comments" ] []
    else
        div [ class "no-comments" ] []


hasComments : Submission -> Bool
hasComments s =
    not <| List.isEmpty s.comments
