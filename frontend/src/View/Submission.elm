module View.Submission exposing (view)

import DataTypes.SubmissionFormat as SubFormat exposing (SubmissionFormat(..))
import Model.Submission exposing (..)
import Messages exposing (..)
import Messages exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, style, type_, value, src, placeholder, href, name, checked, selected, id, for, disabled)
import Html.Events exposing (onInput, onClick, on)
import Nav.Nav exposing (toHash)
import Nav.Model
import Backend.Network exposing (RequestStatus(..))
import Time
import Date
import String
import Json.Decode exposing (succeed)


view : Model -> Html Msg
view model =
    case model.submission of
        Initial ->
            div [] []

        Loading ->
            viewLoading

        Complete submission ->
            viewSubmission submission model

        Error message ->
            viewError message


viewLoading : Html Msg
viewLoading =
    div [ class "wrapper" ]
        [ div [ class "logo-wrapper" ] [ div [ class "logo" ] [] ]
        , div [ class "edit-submission" ]
            [ div [ class "edit-submission loading" ] [ text "Laster ..." ] ]
        ]


viewSubmission : Submission -> Model -> Html Msg
viewSubmission submission model =
    div [ class "wrapper" ]
        [ viewFooter submission model
        , Html.map UpdateSubmission <| viewSubmissionDetails submission model
        ]


viewFooter : Submission -> Model -> Html Msg
viewFooter submission model =
    div [ class "sticky-footer" ]
        [ div [ class "sticky-footer-content" ]
            [ div []
                [ a [ href << toHash <| Nav.Model.Submissions ]
                    [ button [ class "button-back" ] [ text "Tilbake til listen" ] ]
                ]
            , div [ class <| "save-controls " ++ hideIfNotEditable submission.editable ]
                [ div [ class "autosave" ]
                    [ div []
                        [ label [ for "autosave" ] [ text "Autolagre endringer" ]
                        , div [ class "lastsaved" ] [ text <| viewLastSaved model.lastSaved ]
                        ]
                    , div [ class "autosave-checkbox" ]
                        [ input [ id "autosave", type_ "checkbox", onClick ToggleAutosaveSubmission, checked model.autosave ] []
                        ]
                    ]
                , div []
                    [ button [ class "button-save", onClick (SaveSubmission 0) ] [ text "Lagre nå" ] ]
                ]
            ]
        ]


viewSubmissionDetails : Submission -> Model -> Html SubmissionField
viewSubmissionDetails submission model =
    let
        possibleSubmissionFormats =
            [ Presentation20
            , Presentation40
            , Exhibition
            , PechaKucha
            , VideoInternal
            , VideoExternal
            , TheGoodTalk
            , WhateverYouWant
            ]
    in
        div []
            [ div [ class "logo-wrapper" ] [ div [ class "logo" ] [] ]
            , div [ class <| "edit-intro " ++ hideIfNotEditable submission.editable ]
                [ h1 [] [ text "Kjør på, beskriv bidraget ditt" ]
                , p [ class "ingress" ] [ text "Fyll inn feltene nedenfor for å sende inn ditt forslag til fagdag-bidrag. Ikke glem å markere det som innsendt når du er ferdig, så komiteen vet at du er ferdig med kladdingen." ]
                , div [ class "help-part" ]
                    [ strong [] [ span [] [ text "6. august 2018" ], text "Frist for innsending" ]
                    , p [] [ text "Skriv ferdig foredrags-forslagene dine og marker de som innsendt innen denne datoen. Vi autolagrer for deg underveis så du aldri risikerer å miste noe." ]
                    ]
                , div [ class "help-part" ]
                    [ strong [] [ span [] [ text "1. september 2018" ], text "Foredrag blir valgt ut" ]
                    , p [] [ text "Vi forsøker å velge ut alle bidragene innen denne tiden." ]
                    ]
                , div [ class "help-part" ]
                    [ strong [] [ span [] [ text "19. oktober 2018" ], text "Fagdag! Wooh!" ]
                    , p [] [ text "Frem med HDMI-donglene, pynt på håret – her skal det presenteres. Uansett om det er en lyntale du skal holde for aller første gang; eller du har gjort dette 100 ganger før, men likevel har sommerfugler i magen: lykke til! :)" ]
                    ]
                ]
            , div [ class <| "comments-wrapper " ++ hideIfNoComments submission ]
                [ h2 [] [ text "Kommentarer fra komiteen" ]
                , p [] [ text "Komiteen har lagt inn følgende kommentarer på ditt bidrag. Du kan kommentere tilbake, eller bare redigere foredraget ditt nedenfor dersom det er gode forbedringsforslag." ]
                , viewComments submission model
                ]
            , div [ class "edit-submission" ]
                [ div [ class <| "cant-edit-message " ++ hideIfEditable submission.editable ] [ text "Du kan ikke redigere dette foredraget. Kun foredrag fra fremtidige fagdager er redigerbare." ]
                , div [ class "input-section" ]
                    [ h2 [] [ text "Vent, hva er alle disse formatene?" ]
                    , p [ class "input-description" ] [ text "Vi har laget noen slides som forklarer litt mer om de ulike formatene. Slidsene finner du her:" ]
                    , a [ class "input-description", href "/bidrag_formater.pdf" ] [ text "Info om bidragsformater - lenke til pdf" ]
                    ]
                , div [ class "input-section" ] <|
                    List.append
                        [ h2 [] [ text "Format" ]
                        ]
                        (List.map (radio2 submission) possibleSubmissionFormats)
                , viewTitleSection submission
                , viewDescription submission
                , div [ class "input-section" ]
                    [ h2 [] [ text "Ekstra utstyr?" ]
                    , p [ class "input-description" ] [ text "Er det noe spesielt du trenger for å gjennomføre sesjonen din? Du trenger ikke spesifisere åpenbare ting som prosjektor, nettverk og mikrofon, men trenger du en utstoppet oter så bør du skrive opp det..." ]
                    , textarea [ class "small-textarea", value submission.equipment, onInput Equipment, placeholder "Skriv hva du trenger, din personlige raider. Røde non-stop?..." ] []
                    ]
                , div [ class "input-section" ]
                    [ div [ class "flex-header" ]
                        [ h2 [ class "flex-header-element" ] [ text "Hvem er du? (eller dere)" ]
                        , div [ class "flex-header-element" ]
                            [ if List.length submission.speakers > 4 then
                                div [] []
                              else
                                button [ onClick AddSpeaker, class "button-new" ] [ text "Legg til foredragsholder" ]
                            ]
                        ]
                    , p [ class "input-description" ] [ text "Skriv noen setninger om deg selv, og hvorfor akkurat du brenner for å snakke om dette." ]
                    , ul [ class "speakers" ] <|
                        List.map (viewSpeaker submission <| List.length submission.speakers) submission.speakers
                    ]
                , div [ class <| "input-section " ++ hideIfNotEditable submission.editable ++ " " ++ hideIfApprovedRejected submission.status ]
                    [ h2 [] [ text "Er du ferdig eller ikke?" ]
                    , p [ class "input-description" ] [ text "Behold gjerne foredraget ditt som en kladd så lenge du vil. Ingen får se det før du flipper den magiske radio-button'en." ]
                    , p [ class "input-description input-description-strong" ] [ text "...men husk å gjøre det før deadline om du vil bli med i vurderingen!" ]
                    , radio "Kladdemodus: jeg er ikke helt klar enda" "status" "DRAFT" (Status "DRAFT") <| submission.status == "DRAFT"
                    , radio "Klar, ferdig, gå: La komiteen se på det og vurdere" "status" "SUBMITTED" (Status "SUBMITTED") <| submission.status == "SUBMITTED"
                    ]
                , div [ class "sticky-footer-filler" ] []
                ]
            ]


viewTitleSection : Submission -> Html SubmissionField
viewTitleSection submission =
    let
        titleSection : String -> String -> Html SubmissionField
        titleSection titleText placeholderText =
            div [ class "input-section" ]
                [ h2 [] [ text "Tittel" ]
                , p [ class "input-description" ]
                    [ text titleText
                    ]
                , input [ type_ "text", value submission.title, onInput Title, placeholder placeholderText ] []
                ]
    in
        case submission.format of
            VideoExternal ->
                titleSection "Hva er tittel på videoen?" ""

            TheGoodTalk ->
                titleSection "Hvem ønsker du skal snakke?" "Navn på kule folk"

            _ ->
                titleSection "Beskriv bidraget ditt med en kort og konsis tittel. Humor er viktig, men ikke gjør det for fancy – husk at folk skal skjønne hva du skal snakke om ;)" "Kort og konsist = perfekt og presist!"


viewDescription : Submission -> Html SubmissionField
viewDescription submission =
    case submission.format of
        VideoExternal ->
            div [ class "input-section" ]
                [ h2 [] [ text "URL" ]
                , p
                    [ class "input-description" ]
                    [ text "Legg ved url og skriv en kort beskrivelse av videoen. F.eks. http://youtube.com?"
                    ]
                , textarea [ value submission.abstract, onInput Abstract, placeholder "Legg ved URL til videoen og beskriv den kort" ] []
                ]

        TheGoodTalk ->
            div [ class "input-section" ]
                [ h2 [] [ text "Beskrivelsse" ]
                , p
                    [ class "input-description" ]
                    [ text "Hva vil du at den/de skal snakke om?"
                    ]
                , textarea [ value submission.abstract, onInput Abstract, placeholder "Fyll inn popcorn-tema her" ] []
                ]

        _ ->
            div [ class "input-section" ]
                [ h2 [] [ text "Beskrivelse" ]
                , p
                    [ class "input-description" ]
                    [ text "Skriv en kort beskrivelse av bidraget ditt. Maks 150 ord er en fin tommelfinger-regel."
                    ]
                , textarea [ value submission.abstract, onInput Abstract, placeholder "Her kan du selge bidraget ditt! Hvorfor burde vi komme og høre eller se på _akkurat deg_?" ] []
                ]


viewSpeaker : Submission -> Int -> ( Int, Speaker ) -> Html SubmissionField
viewSpeaker submission n ( i, speaker ) =
    let
        removeButton =
            if n == 1 then
                div [] []
            else if not speaker.deletable then
                div [] []
            else
                div [ class "flex-header-element" ]
                    [ button [ onClick (RemoveSpeaker i), class "button-delete" ] [ text "Fjern foredragsholder" ]
                    ]
    in
        li [ class "speaker" ]
            [ div [ class "flex-header" ]
                [ h2 [ class "flex-header-element" ] [ text ("Foredragsholder " ++ toString (i + 1)) ]
                , removeButton
                ]
            , div [ class "speaker-input-section" ]
                [ h3 [] [ text "Foredragsholderens navn (som skrevet i CV-basen)" ]
                , input [ type_ "text", value speaker.name, placeholder "Hei hei hei, jeg heter...", onInput <| SpeakerName i ] []
                ]
            , div [ class "speaker-input-section" ]
                [ h3 [] [ text "Epost" ]
                , input [ type_ "text", value speaker.email, placeholder "sånn elektronisk...", onInput <| SpeakerEmail i ] []
                ]
            ]


viewComments : Submission -> Model -> Html SubmissionField
viewComments submission model =
    div [ class "comment-section" ]
        [ ul [ class "comments" ] <|
            List.map (\comment -> viewComment comment) submission.comments
        , viewCommentSubmission model
        ]


viewComment : Comment -> Html SubmissionField
viewComment comment =
    li [ class "comment" ]
        [ h3 [] [ text comment.name ]
        , p [ class "comment-text" ] [ text comment.comment ]
        ]


viewCommentSubmission : Model -> Html SubmissionField
viewCommentSubmission model =
    div [ class "send-comment" ]
        [ h2 [] [ text "Svar" ]
        , textarea [ onInput NewComment, class "comment-area", value model.comment ] []
        , button [ onClick SaveComment, disabled <| String.isEmpty model.comment ] [ text "Send" ]
        ]


viewError : String -> Html Msg
viewError message =
    div [] [ text message ]


hideIfEditable : Bool -> String
hideIfEditable editable =
    if editable then
        "hide"
    else
        ""


hideIfNotEditable : Bool -> String
hideIfNotEditable editable =
    if not editable then
        "hide"
    else
        ""


hideIfApprovedRejected : String -> String
hideIfApprovedRejected status =
    if status == "APPROVED" then
        "hide"
    else if status == "REJECTED" then
        "hide"
    else
        ""


hideIfNoComments : Submission -> String
hideIfNoComments submission =
    if List.isEmpty submission.comments then
        "hide"
    else
        ""


radio : String -> String -> String -> msg -> Bool -> Html msg
radio l group val msg selected =
    div [ class "radio-input" ]
        [ label []
            [ input [ type_ "radio", name group, value val, onClick msg, checked selected ] []
            , text l
            ]
        ]


radio2 : Submission -> SubmissionFormat -> Html SubmissionField
radio2 submission subFormat =
    radio (SubFormat.toString subFormat) "format" "presentation" (Format subFormat) <| submission.format == subFormat


formatText : SubmissionFormat -> String
formatText format =
    case format of
        Presentation20 ->
            "Hvor lang burde presentasjonen din være?"

        LightningTalk ->
            "Hvor lang skal lyntalen din være? (PS: akkurat nå kjører vi bare 10 minutters lyntaler, så du har ikke noe valg her. Sånn er livet av og til :P)"

        Workshop ->
            "Hvor lang burde workshop'en din være?"

        Exhibition ->
            "Hvor lenge skal utstillingen vare?"

        PechaKucha ->
            "Dumt spørsmål. 20 slides * 20 sekunder er???"

        Unknown f ->
            "Hvor lenge skal " ++ f ++ " vare?"

        _ ->
            "Hvor lenge skal herligheten vare?"


viewLastSaved : Maybe Time.Time -> String
viewLastSaved time =
    case time of
        Just t ->
            let
                date =
                    Date.fromTime t
            in
                "Sist lagret "
                    ++ (String.join ":"
                            << List.map (zeroPad << toString)
                        <|
                            [ Date.hour date, Date.minute date, Date.second date ]
                       )

        Nothing ->
            "Ikke redigert enda"


zeroPad : String -> String
zeroPad n =
    if String.length n == 1 then
        "0" ++ n
    else
        n


speakerImage : Speaker -> Html SubmissionField
speakerImage speaker =
    if speaker.hasPicture then
        div [ style <| [ ( "background-image", "url(" ++ speaker.pictureUrl ++ ")" ) ], class "speaker-image" ] []
    else
        img [ src "assets/robot_padded_arm.png", class "speaker-image" ] []
