module Models.Requests exposing (..)

type alias AddPlayer =
    {
        name : String
    }

type alias Buzz =
    {
        playerName : String
    }

type alias Answer =
    {
        playerName: String,
        questionNumber: Int,
        answerNumber: Int
    }