module Services.GameService exposing (..)
import Decoders.Responses exposing (apiResponseDecoder)
import Http

import Json.Encode as Encode
import Models.Requests exposing (Answer, Buzz)
import Models.ViewState exposing (Msg(..))

registerBuzz: Buzz -> Cmd Msg
registerBuzz buzz =
    Http.post
        {

            url = "http://localhost:3030/game/buzz",
            body = Http.jsonBody (buzzRequestEncoder buzz),
            expect = Http.expectJson ApiResult apiResponseDecoder
        }

registerAnswer: Answer -> Cmd Msg
registerAnswer answer =
    Http.post
        {
            url = "http://localhost:3030/game/answer",
            body = Http.jsonBody (answerRequestEncoder answer),
            expect = Http.expectJson ApiResult apiResponseDecoder
        }

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