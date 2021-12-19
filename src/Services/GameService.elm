module Services.GameService exposing (..)
import Decoders.Responses exposing (apiResponseDecoder)
import Encoders.Requests exposing (answerRequestEncoder, buzzRequestEncoder)
import Http

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