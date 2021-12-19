module Views.Views exposing (..)

import  Bootstrap.ListGroup as ListGroup
import Bootstrap.Button as Button
import Bootstrap.Alert as Alert
import Bootstrap.ButtonGroup as ButtonGroup exposing (ButtonItem)
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Utilities.Display as Display
import Html exposing (Attribute, Html, div, h2, h3, h4, img, p, text)
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid as Grid exposing (Column)
import Html.Attributes exposing (class, src, style)
import Models.ViewState exposing (CurrentQuestion, Msg(..), Player, QuestionAnswer)

playerNameInput: Attribute Msg -> Bool -> Html Msg
playerNameInput display exist =
    let
        cmd = if exist then SignUpBtnClicked else PlayerNameEmptyWhileSigningUp
        color = if exist then Button.success else Button.light
    in
        Form.formInline[ display ][
                        Form.group[][
                            Input.email [ Input.id "pseudo", Input.placeholder "Enter your pseudo", Input.onInput PlayerNameUpdated]
                            , Button.button[color,  Button.attrs [ class "ml-sm-2 my-2" ] ,Button.onClick cmd][ text "ENTER"]
                        ]
        ]

userCard : Maybe String -> Player -> Column msg
userCard answer_author { name, score , imgNb} =
    let
        headers = case imgNb of
            Just a ->
                let
                    f = "./asset/" ++ a ++ ".png"
                in
                [
                    img[src f, style "width" "200px", style "height" "200px"][],
                    h3[][text name]
                ]

            Nothing -> [ h3[][text name]]

        card_color = case answer_author of
            Just author ->
                if author == name then Card.warning else Card.light

            Nothing -> Card.light

    in
     Grid.col[Col.sm]
     [
         Card.config[card_color]
            |> Card.header[class "text-center"] headers
            |> Card.block[]
                [
                   div
                    [ class "rounded-pill bg-primary text-center"]
                    [ h2 [ style "color" "black" ][ text <| String.fromInt score]]

                    |> Block.custom
                ]
            |> Card.view

    ]

questionView: Bool -> Maybe Int -> Maybe CurrentQuestion -> Column Msg
questionView canAnswer answer_selected question =

    let
        (display, {number, label , answers, points}) = case question of
            Just a ->
                (Display.block, a )

            Nothing ->
                (Display.none, {number = 0, label ="", answers = [], points = 0 })

        answersView = answers
            |> List.map (answerView canAnswer answer_selected number)
    in
    Grid.col[Col.bottomLg, Col.attrs[ style "background-color" "gray", class "text-center", display ]][

        p [style "color" "white", Spacing.pt2Lg, style "font-size" "1.5em"][
            text label
        ],

        ButtonGroup.buttonGroup[ButtonGroup.large, ButtonGroup.attrs[ Size.w100, Spacing.pb4Lg]] answersView

    ]

answerView: Bool -> Maybe Int ->  Int -> QuestionAnswer -> ButtonItem Msg
answerView canAnswer answer_selected question_number {number, label} =
    let
        (btn_color, cmd ) =
            case answer_selected of
                Just nb ->
                    if nb == number then (Button.success, Noop) else (Button.light, Noop)

                Nothing ->
                    if canAnswer then   (Button.primary, (Answer question_number number)) else (Button.light, Noop)

    in

        ButtonGroup.button[ btn_color, Button.onClick cmd, Button.attrs[Spacing.m1] ][ text label ]


resultView: Bool -> String -> Column Msg
resultView good result =
    Grid.col[Col.attrs [ Spacing.mt3]] [
        if good then
            Alert.simpleSuccess[ class "text-center" ][ text result]
        else
            Alert.simpleDanger[ class "text-center" ][ text result ]
    ]

buzzerView: Bool -> Column Msg
buzzerView enable =
    Grid.col[][
        Button.button[
                Button.outlinePrimary
                , if enable then Button.danger else Button.light
                , Button.attrs[style "width" "120px", style "height" "120px", style "border-radius" "120px"]
                ,  Button.onClick (if enable then Buzz else Noop )
            ][
                h4[][text "BUZZ"]
            ]
    ]

winnersView: List String -> Html Msg
winnersView winners =
    let
        phrase =
            if (List.length winners) > 1 then
                " And the winners are "
            else
                " And the winner is "

        wins =
            winners
            |> String.join " & "

    in
        Alert.simpleSuccess[class "text-center", Spacing.mt4][
            h3[][text phrase ],
            h4[][ text wins]
        ]

conectedPlayersView: List String -> Column Msg
conectedPlayersView players =
    Grid.col[][
        h2[][text  " Connected Players :" ],

        players
        |> List.map (\n -> ListGroup.li[][ text n])
        |> ListGroup.ul

    ]
