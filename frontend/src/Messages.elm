module Messages exposing (Msg(..), SubmissionField(..))

import DataTypes.SubmissionFormat exposing (SubmissionFormat)
import Nav.Model exposing (Page)
import Http exposing (Error)
import Model.Submissions exposing (Submissions)
import Model.Submission exposing (Speaker, Submission)
import Ports exposing (UploadedImageData)
import Time


type Msg
    = UpdateUrl Page
    | LoginEmail String
    | LoginSubmitEmail
    | LoginSubmit (Result Error String)
    | TokenSaved ()
    | SubmissionsGet (Result Http.Error Submissions)
    | SubmissionsCreateTalk
    | SubmissionsCreated (Result Http.Error Model.Submission.Submission)
    | SubmissionsLogout
    | SubmissionsLoggedOut (Result Http.Error String)
    | SubmissionsTokenRemoved ()
    | GetSubmission (Result Http.Error Submission)
    | SaveSubmission Time.Time
    | SavedSubmission (Result Http.Error Submission)
    | ToggleAutosaveSubmission
    | TimeUpdatedSubmission Time.Time
    | UpdateSubmission SubmissionField
    | CommentSent (Result Http.Error Submission)
    | Reauthenticate


type SubmissionField
    = Title String
    | Abstract String
    | Equipment String
    | Format SubmissionFormat
    | IntendedAudience String
    | Language String
    | Length String
    | Outline String
    | Level String
    | Status String
    | SuggestedKeywords String
    | InfoToProgramCommittee String
    | AddSpeaker
    | SpeakerName Int String
    | SpeakerEmail Int String
    | SpeakerBio Int String
    | SpeakerZipCode Int String
    | SpeakerTwitter Int String
    | RemoveSpeaker Int
    | FileSelected Speaker Int String
    | FileUploaded UploadedImageData
    | NewComment String
    | SaveComment
