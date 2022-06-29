port module Ports exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


port showNameModal : () -> Cmd msg


port hideNameModal : () -> Cmd msg


port sendMessage : String -> Cmd msg


port sendReply : String -> Cmd msg


port reactToMessage : ( String, String ) -> Cmd msg


port initializeClient : Encode.Value -> Cmd msg


port newStreamConnected : (Encode.Value -> msg) -> Sub msg


port newReaction : (Encode.Value -> msg) -> Sub msg


port receiveConnectionId : (String -> msg) -> Sub msg


{-| Receive messages (signals) from the OpenTok client, on "signal:msg" and on "signal:reply"

ref: <https://lengrand.fr/a-short-introduction-to-ports-and-flags-in-elm/>

-}
port messageReceiver : (Encode.Value -> msg) -> Sub msg


{-| connectionId String The ID of this connection.
creationTime Number The timestamp for the creation of the connection. This value is calculated in milliseconds. You can convert this value to a Date object by calling new Date(creationTime), where creationTime is the creationTime property of the Connection object.
data String A string containing metadata describing the connection. When you generate a user token, you can define connection data (see the Token creation overview).

<https://tokbox.com/developer/sdks/js/reference/Connection.html>

-}
type alias ConnectionDto =
    { connectionId : String
    , creationTime : Int -- time but geez, cannot find a concise decode from timestamp to Posix out there
    , data : String
    }


connectionDecoder : Decode.Decoder ConnectionDto
connectionDecoder =
    Decode.succeed ConnectionDto
        |> required "connectionId" Decode.string
        |> required "creationTime" Decode.int
        |> optional "data" (Decode.oneOf [ Decode.string, Decode.null "" ]) ""


{-| A data transfer object representation of a message event.

The OpenTok.js Client refers to "messages" as signals and events regarding messages are `SignalEvent`Sub.batch

These are there properties:

data — (String) The data string sent with the signal (if there is one).
from — (Connection) The Connection corresponding to the client that sent the signal.
type - The type assigned to the signal (if there is one).
Use the type to filter signals received (by adding an event handler for signal:type1 or signal:type2, etc.)

<https://tokbox.com/developer/sdks/js/reference/SignalEvent.html>

-}
type alias MessageEventDto =
    { type_ : String
    , data : String
    , from : ConnectionDto
    }



{-



   https://tokbox.com/developer/sdks/js/reference/Session.html#signal
-}


messageEventDecoder : Decode.Decoder MessageEventDto
messageEventDecoder =
    Decode.succeed MessageEventDto
        |> required "type" Decode.string
        |> required "data" Decode.string
        |> required "from" connectionDecoder


type alias NewStreamDto =
    { streamName : String
    , connectionId : String
    }


newStreamDecoder : Decode.Decoder NewStreamDto
newStreamDecoder =
    Decode.succeed NewStreamDto
        |> required "streamName" Decode.string
        |> required "connectionId" Decode.string


type alias ReactionDto =
    { connectionId : String
    , chatMsgId : String
    }


reactionDecoder : Decode.Decoder ReactionDto
reactionDecoder =
    Decode.succeed ReactionDto
        |> required "connectionId" Decode.string
        |> required "chatMsgId" Decode.string
