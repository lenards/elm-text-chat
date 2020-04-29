module Types exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import Ports exposing (MessageEventDto, NewStreamDto, ReactionDto)
import Set exposing (Set)
import Time


type alias Flags =
    { openTokServer : String
    }


{-| Responses from the "learning-opentok-node" app server look like the following:

```
{
    "apiKey":  "",
    "sessionId":  "",
    "token":  ""
}
```

-}
type alias SessionDto =
    { apiKey : String
    , sessionId : String
    , token : String
    }


type alias OpenTokState =
    { apiKey : String
    , sessionId : String
    , token : String
    }


type OpenTokAuth
    = NotAuthenticated
    | Authed OpenTokState


type alias ChatMessage =
    { id : String
    , type_ : String
    , content : String
    , fromConnection : String
    , creationTime : Time.Posix
    , connectionData : String
    }


type alias Model =
    { openTokUrl : String
    , openTokAuth : OpenTokAuth
    , draftMessage : String
    , messages : List ChatMessage
    , name : String
    , connectionId : String
    , streamConnections : Dict String String
    , reactions : Dict String (Set String)
    }


type Msg
    = NoOp
    | SessionInitialized (Result Http.Error SessionDto)
    | EditChatHandle String
    | SetChatHandle
    | DraftChanged String
    | Send
    | Recv (Result Decode.Error MessageEventDto)
    | React String
    | LatestConnectionId String
    | NewStreamConnected (Result Decode.Error NewStreamDto)
    | ReceivedNewReaction (Result Decode.Error ReactionDto)


toOpenTokState : SessionDto -> OpenTokState
toOpenTokState dto =
    { apiKey = dto.apiKey
    , sessionId = dto.sessionId
    , token = dto.token
    }


createOpenTokAuth : SessionDto -> OpenTokAuth
createOpenTokAuth dto =
    Authed (dto |> toOpenTokState)


toClientTuple : OpenTokAuth -> Maybe ( String, String, String )
toClientTuple auth =
    case auth of
        NotAuthenticated ->
            Nothing

        Authed state_ ->
            Just
                ( state_.apiKey
                , state_.sessionId
                , state_.token
                )


dataDelimiter : String
dataDelimiter =
    "‡"


toChatMessage : MessageEventDto -> ChatMessage
toChatMessage dto =
    let
        ( correlationId, txtMsg ) =
            case String.split dataDelimiter dto.data of
                [ cId, txt ] ->
                    ( cId, txt )

                cId :: txt :: _ ->
                    ( cId, txt )

                _ ->
                    ( dto.data, "" )
    in
    { id = correlationId
    , type_ = dto.type_
    , content = txtMsg
    , fromConnection = dto.from.connectionId
    , creationTime = Time.millisToPosix dto.from.creationTime
    , connectionData = dto.from.data
    }
