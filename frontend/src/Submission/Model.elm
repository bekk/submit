module Submission.Model exposing (Model, initModel)


type alias Model =
    { entry : String
    }


initModel : Model
initModel =
    Model ""
