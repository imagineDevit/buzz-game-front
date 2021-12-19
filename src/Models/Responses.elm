module Models.Responses exposing (..)

type Response =
    Error String
    | GameStarted
    | PlayerAdded
    | BuzzRegistered
    | AnswerRegistered