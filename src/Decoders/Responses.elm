module Decoders.Responses exposing (..)

import Json.Decode as D
import Models.Responses exposing (Response(..))

apiResponseDecoder: D.Decoder Response
apiResponseDecoder =
    D.field "type" D.string
    |> D.andThen responseDecoder

responseDecoder: String -> D.Decoder Response
responseDecoder response_type =
    case response_type of
        "ERROR" -> D.map Error (D.field "message" D.string)
        "GAME_STARTED" -> D.succeed GameStarted
        "BUZZ_REGISTERED" -> D.succeed BuzzRegistered
        "ANSWER_REGISTERED" -> D.succeed AnswerRegistered
        _ -> D.succeed PlayerAdded