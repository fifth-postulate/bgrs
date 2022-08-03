module BackGammon.Id.Position exposing (Error, Id, Key, decode, encode)

import Base64.Decode as Decode
import Base64.Encode as Encode
import Bytes exposing (Bytes)


type alias Key =
    Bytes


type alias Id =
    String


encode : Key -> Id
encode key =
    key
        |> Encode.bytes
        |> Encode.encode
        |> String.dropRight 2


decode : Id -> Result Error Key
decode id =
    id
        |> flip String.append "=="
        |> Decode.decode Decode.bytes
        |> Result.mapError DecodingError


type Error
    = DecodingError Decode.Error


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b
