module Encoders.Requests exposing (..)

import Json.Encode as Encode
import Models.Requests exposing (Answer, Buzz)

buzzRequestEncoder: Buzz -> Encode.Value
buzzRequestEncoder buzz =
    Encode.object
    [
        ("playerName", Encode.string buzz.playerName)
    ]


answerRequestEncoder: Answer -> Encode.Value
answerRequestEncoder answer =
    Encode.object
    [
        ("playerName", Encode.string answer.playerName),
        ("answerNumber", Encode.int answer.answerNumber),
        ("questionNumber", Encode.int answer.questionNumber)
    ]