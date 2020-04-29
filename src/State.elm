module State exposing (init, subscriptions, update)

import Api
import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Ports exposing (hideNameModal, initializeClient, messageEventDecoder, messageReceiver, newReaction, newStreamConnected, newStreamDecoder, reactToMessage, reactionDecoder, receiveConnectionId, sendMessage, showNameModal)
import Set
import Types exposing (Flags, Model, Msg(..), OpenTokAuth(..), SessionDto, createOpenTokAuth, toChatMessage, toClientTuple)


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

        Recv (Ok dto) ->
            ( { model
                | messages = (dto |> toChatMessage) :: model.messages
              }
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
