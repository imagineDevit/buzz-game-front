module Models.Messages exposing (..)

type alias Answer =
    {
        number: Int,
        label: String,
        good: Bool
    }

type alias CanBuzzMsg = { canBuzz: Bool }

type alias PlayerScoreMsg =
   {
        playerName: String,
        score: Int,
        goodAnswer: String,
        update: Bool
   }

type alias QuestionMsg =
   {
        number: Int,
        label: String,
        points: Int,
        answers: List Answer
   }

type alias BuzzMsg = { author: String }

type alias PlayerAnswerMsg =
   {
        playerName: String,
        answer: Answer
   }

type alias ErrorMsg = { message : String }

type Message =
    GameStart
    | GameEnd
    | PlayerScore PlayerScoreMsg
    | Question QuestionMsg
    | Buzz BuzzMsg
    | PlayerAnswer PlayerAnswerMsg
    | CanBuzz CanBuzzMsg
    | Error ErrorMsg


type alias StateChange =
   {
        changeType: String,
        message: Message,
        players: List String,
        requiredNbPlayers: Int
   }
