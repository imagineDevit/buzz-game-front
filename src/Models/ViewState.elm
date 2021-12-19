module Models.ViewState exposing (..)

import Http
import Models.Messages as M
import Models.Responses exposing (Response)
import Process
import Random
import String exposing (isEmpty)
import Json.Encode as E
import Task

-- aliases
type GameState =
    WAITING (List String) (Maybe Int)   -- nb of players waiting for
    | STARTED
    | BUZZED String -- buzz author
    | ANSWERED String Int  -- answer author , selected answer number
    | ERROR String  -- error message
    | RUNNING
    | ENDED (List String)

type alias QuestionAnswer =
    {
        number: Int,
        label: String
    }

type alias CurrentQuestion =
    {
        number: Int,
        label: String,
        answers: List QuestionAnswer,
        points: Int
    }

type alias Player =
    {
        name: String,
        score: Int,
        imgNb: Maybe String
    }

type alias Count = {num: Int, name: String}

type alias State =
   {
        connected: Bool,
        player: Maybe String,
        players: List Player,
        canBuzz: Bool,
        question: Maybe CurrentQuestion,
        goodAnswer: Maybe (Bool, String),
        gameState: GameState
   }


-- View Messages
type Msg =
    PlayerNameUpdated String
    | SignUpBtnClicked
    | PlayerNameEmptyWhileSigningUp
    | ProfilImgChosen String Int
    | Buzz
    | Answer Int Int
    | ApiResult (Result Http.Error Response)
    | ReceiveStateChange E.Value
    | NewQuestion CurrentQuestion
    | EnableBuzz
    | Noop


-- Initialize the state
initState: Int -> State
initState nb =
    {
        connected = False,
        player = Nothing,
        players = [],
        canBuzz = False,
        question = Nothing,
        goodAnswer = Nothing,
        gameState = WAITING [] (Just nb)
    }

-- Get requiredNbPlayers
getRequiredNbPlayers: State -> Int
getRequiredNbPlayers state =
    case state.gameState of
        WAITING  _ optNb ->
            optNb |> Maybe.withDefault  0
        _ -> 0

-- Update the player name
updatePlayerName: String -> State -> State
updatePlayerName name state =
    let
        playerName = if isEmpty name then Nothing else Just name
    in
        {state | player = playerName}


-- calculate winners
winners: State -> List String
winners state =
    let
        max =
            state.players
            |> List.map (\p -> p.score)
            |> List.maximum

        wins = case max of
            Just value ->
                state.players
                |> List.filter (\p -> p.score == value)
                |> List.map (\p -> p.name)

            Nothing -> []
    in
        wins


-- On stateChange
onStateChange : State -> M.StateChange -> (State, Cmd Msg)
onStateChange state stateChange =
    case stateChange.message of
        M.PlayerScore pScoreMsg ->
            if stateChange.requiredNbPlayers > (List.length state.players) then
                 -- add player
                 -- update gameState
                 let

                     gState = 1
                                |> (-) (getRequiredNbPlayers state)
                                |>  Just
                                |> WAITING stateChange.players

                 in
                     ({state | gameState = gState }, Cmd.none)

            else
                -- update score of the player
                let
                    playerz =
                        state.players
                        |> List.map (\p -> if p.name == pScoreMsg.playerName then { p | score = pScoreMsg.score} else p)

                    ga = if pScoreMsg.update then Just (True, "") else Just (False, pScoreMsg.goodAnswer)
                in
                    ({state | players = playerz, goodAnswer = ga}, Cmd.none)

        M.Question qMsg ->
            let
                answers = qMsg.answers
                    |> List.map (\a -> QuestionAnswer a.number a.label)

                cQuestion = CurrentQuestion qMsg.number qMsg.label answers qMsg.points
            in
                (state, wait 1000 (NewQuestion cQuestion))

        M.Buzz buzzMsg ->
            ({state | gameState = BUZZED buzzMsg.author}, Cmd.none)

        M.PlayerAnswer apMsg ->
            ({ state | gameState = ANSWERED apMsg.playerName apMsg.answer.number}, Cmd.none)

        M.CanBuzz cbMsg ->
            if cbMsg.canBuzz then
                ({ state | gameState = RUNNING}, wait 1200 EnableBuzz)
            else
                ({ state | canBuzz = False , gameState = RUNNING}, Cmd.none)

        M.Error errorMsg ->
            ({state | gameState = ERROR errorMsg.message }, Cmd.none)

        M.GameStart ->
            let
                allPlayers =
                    stateChange.players
                    |> List.map (getPlayer state.players)
                    |> List.append state.players

                cmds = List.map (\p -> randonImgNb p.name) allPlayers

            in

            ({state | gameState = STARTED, players = allPlayers}, Cmd.batch cmds)

        M.GameEnd ->
            ({state | gameState = ENDED (winners state)}, Cmd.none)

randonImgNb: String ->  Cmd Msg
randonImgNb name =
    Random.generate (ProfilImgChosen name) (Random.int 0 9)

getPlayer: List Player -> String -> Player
getPlayer players pName =
    players
        |> List.filter (\p -> p.name == pName)
        |> List.head
        |> Maybe.withDefault (Player pName 0 Nothing)


wait: Float -> Msg -> Cmd Msg
wait time msg =
   Process.sleep time
   |> Task.perform (\_ -> msg)