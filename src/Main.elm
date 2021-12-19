port module Main exposing (main)



import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row

import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Utilities.Display as Display
import Bootstrap.Utilities.Size as Size
import Browser

import Html.Attributes exposing (class)
import Models.Requests as Reqs
import Models.ViewState exposing (..)
import Decoders.Messages as MD
import Html exposing (Attribute, Html, h2, li, p, text, ul)
import Json.Decode as D
import Json.Encode as E
import Services.GameService exposing (registerAnswer, registerBuzz)
import Views.Views as Views


-- MAIN
main: Program Int State Msg
main =
    Browser.element {
        init = init,
        update = update,
        subscriptions = subscriptions,
        view = view
    }

-- PORTS
port log : String -> Cmd msg
port addPlayer: String -> Cmd msg
port stateChange: (E.Value -> msg) -> Sub msg

-- INIT
init : Int -> (State, Cmd Msg)
init nb =  (initState nb, Cmd.none)

-- UPDATE
update: Msg -> State -> (State, Cmd Msg)
update msg state =
    case msg of
        PlayerNameUpdated name -> (updatePlayerName name state, Cmd.none)

        SignUpBtnClicked ->
            case state.player of
                Just name -> ({state | connected = True}, addPlayer name)
                Nothing -> (state, Cmd.none)

        PlayerNameEmptyWhileSigningUp -> (state, Cmd.none )

        ProfilImgChosen name img ->
            let
                playerz = state.players
                    |> List.map (\p ->  if p.name == name then { p | imgNb = Just <| String.fromInt img} else p)
            in
                ({state | players = playerz}, Cmd.none)

        Buzz ->
            let
                cmd =
                    case state.player of
                        Just playerName ->
                            let
                                buzz: Reqs.Buzz
                                buzz =
                                    {
                                        playerName = playerName
                                    }
                            in
                                registerBuzz buzz

                        Nothing -> Cmd.none

            in
                (state, cmd)

        Answer qNum aNum ->
            let
                cmd = case state.player of
                    Just playerName ->
                        let
                            answer : Reqs.Answer
                            answer =
                                {
                                    playerName = playerName ,
                                    questionNumber = qNum,
                                    answerNumber = aNum
                                }
                        in
                            registerAnswer answer

                    Nothing -> Cmd.none
            in
            (state, cmd)

        ReceiveStateChange json ->
            let
                cType = case D.decodeValue (D.field "type" D.string) json of
                    Ok value -> value
                    Err _ -> "ERROR"

                playerz = state.players |> List.map (\p -> p.name)

                requiredNbPlayers = getRequiredNbPlayers state
            in
                case D.decodeValue (MD.stateDecoder cType playerz  requiredNbPlayers) json of
                    Ok value ->
                        onStateChange state value

                    Err e ->
                        case e of
                            D.Field m _ -> (state, log m )
                            D.Index i _ -> (state, log (String.fromInt i) )
                            D.OneOf _ -> (state, Cmd.none)
                            D.Failure m _ -> (state, log m )

        ApiResult _ -> (state, Cmd.none)

        Noop -> (state, Cmd.none)

        NewQuestion currentQuestion ->
            ({state | question = Just currentQuestion, goodAnswer = Nothing, gameState = RUNNING }, Cmd.none)

        EnableBuzz ->
            ({ state | canBuzz = True , gameState = RUNNING}, Cmd.none)




-- SUBSCRIPTIONS
subscriptions: State -> Sub Msg
subscriptions _ = stateChange ReceiveStateChange

-- VIEW
view: State -> Html Msg
view state =
    let
         (author, answerNumber, canAnswer) = case state.gameState of

             BUZZED buzzAuthor -> (Just buzzAuthor, Nothing, buzzAuthor == ( Maybe.withDefault "" state.player ))

             ANSWERED answerAuthor nb -> (Just answerAuthor, Just nb, False)

             _ -> (Nothing, Nothing, False)

         connected = if state.connected then Display.none else Display.block

         playerNameExist =  case state.player of
             Maybe.Just _ -> True
             Maybe.Nothing -> False

         playersCards = state.players
                |> List.map (Views.userCard author)

         (displayResult, good, goodAnswer) = case state.goodAnswer of
             Just (g, a)->
                (Display.block, g, if g then "Bonne réponse !" else String.append "Mauvaise réponse. La bonne réponse était : " (String.toUpper a))
             Nothing ->
                (Display.none, False, "")

         started = case state.gameState of
             STARTED  -> Display.block
             BUZZED _  -> Display.block
             ANSWERED _ _ -> Display.block
             RUNNING -> Display.block
             _ -> Display.none

         (waiting, players) = case state.gameState of
             WAITING p _ -> (Display.block, p)
             _ -> (Display.none, [])

         (end, winners) = case state.gameState of
             ENDED v ->
                 (True, v)
             _ -> (False, [])

         views = if end then [
                    Views.winnersView winners
                ]
                else [
                    Views.playerNameInput connected playerNameExist,

                    Grid.container [Spacing.pt2][

                        Grid.row[] playersCards,

                        Grid.row[Row.attrs[waiting]][
                            Views.conectedPlayersView players
                        ],
                        Grid.row[Row.attrs[Spacing.pt2]] [
                            state.question
                            |> Views.questionView canAnswer answerNumber
                        ],

                        Grid.row[Row.attrs[displayResult]][ Views.resultView good goodAnswer ],

                        Grid.row[Row.centerXs, Row.attrs[Size.w100, Spacing.mt5, class "text-center", started]][
                            Views.buzzerView state.canBuzz
                        ]
                    ]
                ]
    in
        Grid.container [] views

