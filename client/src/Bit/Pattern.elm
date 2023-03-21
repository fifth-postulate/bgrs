module Bit.Pattern exposing (Pattern, decoder, encoder)

import Bit exposing (Bit)
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Encode as Encode exposing (Encoder)


type alias Pattern =
    List Bit


decoder : Endianness -> Int -> Decoder Pattern
decoder endianness width =
    repeat width (fromInteger endianness)
        |> Decode.map List.concat


repeat : Int -> Decoder a -> Decoder (List a)
repeat n dec =
    Decode.loop ( n, [] ) (repeatStep dec)


repeatStep : Decoder a -> ( Int, List a ) -> Decoder (Step ( Int, List a ) (List a))
repeatStep dec ( n, xs ) =
    if n <= 0 then
        Decode.succeed <| Done <| List.reverse xs

    else
        Decode.map (\x -> Loop ( n - 1, x :: xs )) dec


fromInteger : Endianness -> Decoder Pattern
fromInteger endianness =
    let
        toBit n =
            n
                |> Bit.fromInt
                |> Maybe.withDefault Bit.zero
    in
    Decode.unsignedInt8
        |> Decode.map (toCiphers endianness 2 8)
        |> Decode.map (List.map toBit)


toCiphers : Endianness -> Int -> Int -> Int -> List Int
toCiphers =
    tailrec_toCiphers []


tailrec_toCiphers : List Int -> Endianness -> Int -> Int -> Int -> List Int
tailrec_toCiphers accumulator endianness base width n =
    if width <= 0 then
        case endianness of
            LE ->
                List.reverse accumulator

            BE ->
                accumulator

    else
        tailrec_toCiphers (modBy base n :: accumulator) endianness base (width - 1) (n // base)


encoder : Endianness -> Pattern -> Encoder
encoder endianness pattern =
    pattern
        |> pad
        |> to8BitIntegers endianness
        |> List.map Encode.unsignedInt8
        |> Encode.sequence


pad : Pattern -> Pattern
pad =
    padToMultipleOf 8 Bit.zero


padToMultipleOf : Int -> Bit -> Pattern -> Pattern
padToMultipleOf d padding pattern =
    let
        residue =
            modBy d <| List.length pattern
    in
    if residue == 0 then
        pattern

    else
        pattern ++ List.repeat (d - residue) padding


to8BitIntegers : Endianness -> Pattern -> List Int
to8BitIntegers =
    toIntegers 8


toIntegers : Int -> Endianness -> Pattern -> List Int
toIntegers width endianness pattern =
    pattern
        |> factors width
        |> List.map (List.map Bit.toInt)
        |> List.map (toInteger 2 endianness)


factors : Int -> List a -> List (List a)
factors =
    tailrec_factors []


tailrec_factors : List (List a) -> Int -> List a -> List (List a)
tailrec_factors accumulator width pattern =
    if List.isEmpty pattern then
        List.reverse accumulator

    else
        tailrec_factors (List.take width pattern :: accumulator) width (List.drop width pattern)


toInteger : Int -> Endianness -> List Int -> Int
toInteger base endianness ciphers =
    let
        toValue index cipher =
            cipher * (base ^ index)

        correctForEndianness =
            case endianness of
                LE ->
                    identity

                BE ->
                    List.reverse
    in
    ciphers
        |> correctForEndianness
        |> List.indexedMap toValue
        |> List.sum
