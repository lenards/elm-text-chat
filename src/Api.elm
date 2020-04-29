module Api exposing (..)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Types exposing (Msg(..), SessionDto)


initSessionCmd : String -> Cmd Msg
initSessionCmd url =
    Http.get
        { url = url ++ "/room/t650"
        , expect = Http.expectJson SessionInitialized sessionDecoder
        }


sessionDecoder : Decode.Decoder SessionDto
sessionDecoder =
    Decode.succeed SessionDto
        |> required "apiKey" Decode.string
        |> required "sessionId" Decode.string
        |> required "token" Decode.string
