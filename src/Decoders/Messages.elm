module Decoders.Messages exposing (..)
import Models.Messages as Msg
import Json.Decode as D
import Json.Decode.Pipeline exposing (required, optional)

stateDecoder : String -> List String -> Int ->  D.Decoder Msg.StateChange
stateDecoder cType players nb =
    D.succeed Msg.StateChange
    |> required "type" D.string
    |> optional "message" (messageDecoder cType)  (defautMessage cType)
    |> optional "players" (D.list D.string) players
    |> optional "requiredNbPlayers" D.int nb

defautMessage : String -> Msg.Message
defautMessage cType =
    if cType == "GAME_START" then Msg.GameStart else Msg.GameEnd

messageDecoder: String -> D.Decoder Msg.Message
messageDecoder cType =
    case cType of
        "CAN_BUZZ" ->
            D.map Msg.CanBuzz canBuzzMsgDecoder
        "NEW_PLAYER_SCORE" ->
            D.map Msg.PlayerScore playerScoreMsgDecoder
        "NEW_QUESTION" ->
            D.map Msg.Question questionMsgDecoder
        "NEW_BUZZ" ->
            D.map Msg.Buzz buzzMsgDecoder
        "NEW_ANSWER" ->
            D.map Msg.PlayerAnswer playerAnswerMsgDecoder

        _ ->
            D.map Msg.Error errorMsgDecoder

errorMsgDecoder: D.Decoder Msg.ErrorMsg
errorMsgDecoder =
    D.map Msg.ErrorMsg
    (D.field "message" D.string)

playerAnswerMsgDecoder: D.Decoder Msg.PlayerAnswerMsg
playerAnswerMsgDecoder =
    D.succeed Msg.PlayerAnswerMsg
    |> required "playerName" D.string
    |> required "answer" answerDecoder

buzzMsgDecoder: D.Decoder Msg.BuzzMsg
buzzMsgDecoder =
    D.map Msg.BuzzMsg
    (D.field "author" D.string)

questionMsgDecoder: D.Decoder Msg.QuestionMsg
questionMsgDecoder =
    D.succeed Msg.QuestionMsg
    |> required "number" D.int
    |> required "label" D.string
    |> required "points" D.int
    |>required "answers" (D.list answerDecoder)

canBuzzMsgDecoder: D.Decoder Msg.CanBuzzMsg
canBuzzMsgDecoder =
    D.map Msg.CanBuzzMsg (D.field "canBuzz" D.bool)

playerScoreMsgDecoder: D.Decoder Msg.PlayerScoreMsg
playerScoreMsgDecoder =
    D.succeed Msg.PlayerScoreMsg
    |> required "playerName" D.string
    |> required "score" D.int
    |> required "goodAnswer" D.string
    |> required "update" D.bool

answerDecoder : D.Decoder Msg.Answer
answerDecoder =
    D.succeed Msg.Answer
    |> required "number" D.int
    |> required "label" D.string
    |> required "good" D.bool