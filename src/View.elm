module View exposing (view)

import Browser exposing (Document)
import Dict exposing (Dict)
import Html exposing (Html, button, div, footer, h2, header, input, label, li, main_, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, for, id, name, placeholder, spellcheck, style, tabindex, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Decode
import Set exposing (Set)
import Types exposing (ChatMessage, Model, Msg(..))


onEnter : msg -> Html.Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg

            else
                Decode.fail "not ENTER"
    in
    on "keydown" (Decode.andThen isEnter keyCode)


renderMicroModalMarkup : Model -> Html Msg
renderMicroModalMarkup model =
    div
        [ id "modal-1"
        , class "modal micromodal-slide"
        , attribute "aria-hidden" "true"
        ]
        [ div [ tabindex -1, class "modal__overlay", attribute "data-micromodal-close" "" ]
            [ div
                [ class "modal__container"
                , attribute "role" "dialog"
                , attribute "aria-modal" "true"
                , attribute "aria-labelledby" "modal-1-title"
                ]
                [ header [ class "modal__header" ]
                    [ h2 [ id "modal-1-title" ] [ text "Hi there!" ]
                    , button
                        [ class "modal__close"
                        , attribute "aria-label" "Close modal"
                        , attribute "data-micromodal-close" ""
                        ]
                        []
                    ]
                , main_
                    [ id "modal-1-content", class "modal__content" ]
                    [ div []
                        [ label [ for "chat-handle", style "display" "block" ]
                            [ text "Please enter your name:"
                            , Html.input
                                [ class "modal__name-input"
                                , style "display" "block"
                                , style "border" "0.0625rem solid steelblue"
                                , style "border-radius" "0.2rem"
                                , style "padding" "0.412rem"
                                , name "chat-handle"
                                , placeholder "Name (chat handle)"
                                , type_ "text"
                                , spellcheck False
                                , value model.name
                                , onInput EditChatHandle
                                , onEnter SetChatHandle
                                ]
                                []
                            ]
                        ]
                    ]
                , footer
                    [ class "modal__footer" ]
                    [ button
                        [ class "modal__btn modal__btn-primary"
                        , onClick SetChatHandle
                        ]
                        [ text "Continue" ]
                    , button
                        [ class "modal__btn"
                        , attribute "data-micromodal-close" ""
                        , attribute "aria-label" "Close this dialog window"
                        ]
                        [ text "Close" ]
                    ]
                ]
            ]
        ]


renderChatInput : String -> Html Msg
renderChatInput draft =
    div [ class "chat__input" ]
        [ input
            [ id "msgTxt"
            , type_ "text"
            , placeholder "Input your text here"
            , onInput DraftChanged
            , onEnter Send
            , value draft
            ]
            []
        ]


renderReaction : String -> ChatMessage -> Dict String (Set String) -> Html Msg
renderReaction myConnectionId chatMsg reactions =
    let
        reactionCount =
            Dict.get chatMsg.id reactions
                |> Maybe.map (\rs -> Set.size rs)
                |> Maybe.withDefault 0

        reactionClasses =
            classList
                [ ( "chat__reaction-btn", True )
                , ( "chat__reaction-btn--sent"
                  , chatMsg.fromConnection == myConnectionId
                  )
                , ( "chat__reaction-btn--rcvd"
                  , chatMsg.fromConnection /= myConnectionId
                  )
                , ( "chat__reaction-btn--filled", reactionCount /= 0 )
                ]

        contents =
            if reactionCount /= 0 then
                text (String.fromInt reactionCount ++ " 😃")

            else
                text "☺"
    in
    div
        [ reactionClasses
        , style "padding-left" "0.2rem"
        , style "padding-right" "0.2rem"
        , style "font-size" "1rem"
        , onClick (React chatMsg.id)
        ]
        [ contents ]


chatMessage :
    Dict String (Set String)
    -> Dict String String
    -> String
    -> ChatMessage
    -> Html Msg
chatMessage reactions connections myConnectionId chatMsg =
    let
        classes =
            classList
                [ ( "chat__bubble", True )
                , ( "chat__bubble--sent"
                  , chatMsg.fromConnection == myConnectionId
                  )
                , ( "chat__bubble--rcvd"
                  , chatMsg.fromConnection /= myConnectionId
                  )
                ]

        containerClasses =
            classList
                [ ( "chat__bubble-container", True )
                , ( "chat__bubble-container--sent"
                  , chatMsg.fromConnection == myConnectionId
                  )
                , ( "chat__bubble-container--rcvd"
                  , chatMsg.fromConnection /= myConnectionId
                  )
                ]

        infoClasses =
            classList
                [ ( "chat__bubble-info", True )
                , ( "chat__bubble-info--sent"
                  , chatMsg.fromConnection == myConnectionId
                  )
                , ( "chat__bubble-info--rcvd"
                  , chatMsg.fromConnection /= myConnectionId
                  )
                ]

        handle =
            connections
                |> Dict.get chatMsg.fromConnection
                |> Maybe.withDefault "sender unknown"
    in
    li
        [ style "display" "flex"
        , style "flex-direction" "column"
        , containerClasses
        ]
        [ div
            [ id chatMsg.id
            , class "chat__bubble-content"
            , classes
            ]
            [ text chatMsg.content ]
        , div [ infoClasses ]
            [ renderReaction
                myConnectionId
                chatMsg
                reactions
            , div
                [ style "align-self" "flex-end"
                , class "chat__bubble-author"
                ]
                [ text handle ]
            ]
        ]


renderChat : Model -> Html Msg
renderChat model =
    div [ id "textchat ", class "container__chat" ]
        [ ul
            [ id "history"
            , class "chat__history"
            , class "chat"
            ]
            (model.messages
                |> List.reverse
                |> List.map
                    (chatMessage
                        model.reactions
                        model.streamConnections
                        model.connectionId
                    )
            )
        , renderChatInput model.draftMessage
        ]


renderContent : Model -> Html Msg
renderContent model =
    div [ class "container" ]
        [ div [ id "videos", class "container__videos" ]
            [ div [ id "subscriber", class "video__tile" ] []
            , div [ id "publisher", class "video__tile" ] []
            ]
        , renderChat model
        , renderMicroModalMarkup model
        ]


view : Model -> Document Msg
view model =
    let
        appContent =
            renderContent model
    in
    { title = "Elm - OpenTok"
    , body = [ appContent ]
    }
