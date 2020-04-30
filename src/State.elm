module State exposing (init, subscriptions, update)

import Api
import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Ports exposing (MessageEventDto, hideNameModal, initializeClient, messageEventDecoder, messageReceiver, newReaction, newStreamConnected, newStreamDecoder, reactToMessage, reactionDecoder, receiveConnectionId, sendMessage, sendReply, showNameModal)
import Set
import Types exposing (ChatMessageId(..), ChatReply, Flags, Model, Msg(..), OpenTokAuth(..), ReplyState(..), SessionDto, createOpenTokAuth, toChatMessage, toChatReply, toClientTuple)


initialModel : Flags -> Model
initialModel flags =
    { openTokUrl = flags.openTokServer
    , openTokAuth = NotAuthenticated
    , draftMessage = ""
    , messages = []
    , name = ""
    , connectionId = ""
    , streamConnections = Dict.empty
    , reactions = Dict.empty
    , replies = Dict.empty
    , replyState = NotAuthoring
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            initialModel flags
    in
    ( model
    , showNameModal ()
    )


updateRepliesIn : Model -> ChatReply -> Model
updateRepliesIn model reply =
    { model
        | replies =
            model.replies
                |> Dict.get reply.correlationId
                |> Maybe.withDefault []
                |> (\rs ->
                        Dict.insert
                            reply.correlationId
                            (reply :: rs)
                            model.replies
                   )
    }


updateMessagesWith : MessageEventDto -> Model -> Model
updateMessagesWith dto model =
    if dto.type_ == "signal:msg" then
        { model
            | messages = (dto |> toChatMessage) :: model.messages
        }

    else if dto.type_ == "signal:reply" then
        (dto |> toChatReply)
            |> updateRepliesIn model

    else
        model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        EditChatHandle handle ->
            ( { model | name = handle }, Cmd.none )

        SetChatHandle ->
            ( model
            , Cmd.batch
                [ Api.initSessionCmd model.openTokUrl
                , hideNameModal ()
                ]
            )

        DraftChanged txt ->
            ( { model | draftMessage = txt }
            , Cmd.none
            )

        Send ->
            ( { model
                | draftMessage = ""
              }
            , sendMessage model.draftMessage
            )

        AuthorReply chatMsgId ->
            ( { model
                | replyState = ReplyingTo (ChatMsgId chatMsgId) ""
              }
            , Cmd.none
            )

        DraftReplyChanged replyText ->
            let
                updatedState =
                    case model.replyState of
                        ReplyingTo msgId _ ->
                            ReplyingTo msgId replyText

                        NotAuthoring ->
                            ReplyingTo (ChatMsgId "") replyText
            in
            ( { model | replyState = updatedState }
            , Cmd.none
            )

        SendReply ->
            let
                cmd =
                    case model.replyState of
                        ReplyingTo (ChatMsgId ident) content ->
                            sendReply
                                (String.join "‡" [ ident, content ])

                        NotAuthoring ->
                            Cmd.none
            in
            ( { model | replyState = NotAuthoring }, cmd )

        Recv (Ok dto) ->
            ( model
                |> updateMessagesWith dto
            , Cmd.none
            )

        Recv (Err error) ->
            let
                _ =
                    Debug.log "error in decoding" (Decode.errorToString error)
            in
            ( model, Cmd.none )

        React chatMsgId ->
            ( { model
                | reactions =
                    model.reactions
                        |> Dict.insert
                            chatMsgId
                            (Set.fromList [ model.connectionId ])
              }
            , reactToMessage ( model.connectionId, chatMsgId )
            )

        SessionInitialized (Ok dto) ->
            let
                newAuth =
                    createOpenTokAuth dto
            in
            ( { model | openTokAuth = newAuth }
            , dto
                |> encodeClientConfig model.name
                |> initializeClient
            )

        SessionInitialized (Err err) ->
            let
                _ =
                    Debug.log "Session error" err
            in
            ( model, Cmd.none )

        LatestConnectionId connectionId ->
            ( { model
                | connectionId = connectionId
                , streamConnections =
                    model.streamConnections
                        |> Dict.insert
                            connectionId
                            model.name
              }
            , Cmd.none
            )

        NewStreamConnected (Ok dto) ->
            ( { model
                | streamConnections =
                    model.streamConnections
                        |> Dict.insert
                            dto.connectionId
                            dto.streamName
              }
            , Cmd.none
            )

        NewStreamConnected (Err err) ->
            let
                _ =
                    Debug.log "Session error" err
            in
            ( model, Cmd.none )

        ReceivedNewReaction (Ok dto) ->
            let
                existingReactions =
                    model.reactions
                        |> Dict.get dto.chatMsgId
                        |> Maybe.withDefault Set.empty

                updatedReactions =
                    model.reactions
                        |> Dict.insert
                            dto.chatMsgId
                            (Set.insert
                                dto.connectionId
                                existingReactions
                            )
            in
            ( { model | reactions = updatedReactions }
            , Cmd.none
            )

        ReceivedNewReaction (Err err) ->
            let
                _ =
                    Debug.log "Session error" err
            in
            ( model, Cmd.none )


encodeClientConfig : String -> SessionDto -> Encode.Value
encodeClientConfig chatHandle dto =
    Encode.object
        [ ( "apiKey", Encode.string dto.apiKey )
        , ( "sessionId", Encode.string dto.sessionId )
        , ( "token", Encode.string dto.token )
        , ( "chatHandle", Encode.string chatHandle )
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ messageReceiver
            (Decode.decodeValue messageEventDecoder >> Recv)
        , newStreamConnected
            (Decode.decodeValue newStreamDecoder >> NewStreamConnected)
        , newReaction
            (Decode.decodeValue reactionDecoder >> ReceivedNewReaction)
        , receiveConnectionId LatestConnectionId
        ]
