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


introText : List String
introText =
    [ "Når vi nå rebooter fagdagen, er det ikke bare tid og sted vi tar tak i – vi prøver oss også på noen nye formater! "
    , "Er du ikke glad i å snakke foran mange mennesker kan du for eksempel lage en video, eller vise frem det du jobber med på prosjekt i en utstilling. Bobler du over av ting du har lyst å diskutere kan du komme med et forslag til hva du ønsker å diskutere i en god samtale, eller rett og slett høre at noen andre diskuterer ditt spørsmål."
    , "Vi utfordrer foredragssnakketøyet med 20 slides på 20 sekunder hver i Pecha Kucha stil, og håper at mange av dere tar denne utfordringen – tenk så gøy det kan bli!"
    , "Vi åpner også for at man kan skru ned tempoet et par hakk, og rusle rundt og få med seg fagdagen i eget tempo."
    , "Mye er med andre ord nytt. Om alt fungerer vet vi ikke helt enda. Det som er sikkert, er at det ikke blir noen fagdag uten bra faginnhold. Så kom med forslag, og bli med å lage en skikkelig bra fagdag!"
    ]


view : Model -> Html Msg
view model =
    case model.submissions.submissions of
        Initial ->
            div [] []

        Loading ->
            viewWrapper [ div [ class "submissions-list loading" ] [ text "Laster..." ] ]

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
                div [] <|
                    [ p [ class "intro-text intro-text--pitch" ] [ text "Bekk Fagdag, 19. oktober 2018!" ]
                    ]
                        ++ List.map (\t -> p [ class "intro-text" ] [ text t ]) introText
    in
        [ div [ class "flex-header" ]
            [ h1 [ class "flex-header-element" ] [ text "Dine foredrag" ]
            , viewCreateSubmission model
            ]
        , introtext
        , div [ class "submitted-talks" ]
            [ a [ class "button", href "https://admin.cfp.bekk.no/#/5b7079a29bb64d36a5704e95563ad53c" ] [ text "Inspirasjon? Se innsendte forslag" ] ]
        , div [ class "submissions" ] years
        ]


viewCreateSubmission : Model -> Html Msg
viewCreateSubmission model =
    if model.appConfig.submissionsOpen then
        div [ class "flex-header-element" ]
            [ button [ onClick SubmissionsCreateTalk, class "new-talk button-new" ] [ text "Opprett ny kladd" ]
            ]
    else
        div [ class "flex-header-element flex-header-element-vertical" ]
            [ button [ disabled True, class "new-talk button-new" ] [ text "Innsending er stengt" ]
            , div [ class "disabled-text" ] [ text "(men du kan redigere allerede innsendte foredrag!)" ]
            ]


viewEmpty : Html Msg
viewEmpty =
    div [ class "no-talks" ]
        [ img [ src "assets/robot.png", class "welcome-robot" ] []
        , h2 [] [ text "Blank slate, baby!" ]
        , p [] [ text "Det ser ut som om du ikke har sendt inn noen foredrag enda." ]
        , p [ class "last" ] [ text "Lag din første kladd og fyr løs! Du kan jobbe videre med kladden helt til du er fornøyd. Vi autolagrer for deg så du ikke risikerer å miste noe. Når du er ferdig, så markerer du det som klart og krysser fingrene for at akkurat _du_ får snakke på fagdag! :)" ]
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
