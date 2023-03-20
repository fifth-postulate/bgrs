module Bit.Pattern exposing (Pattern, encoder)

import Bit exposing (Bit(..))
import Bytes exposing (Endianness(..))
import Bytes.Encode as Encode exposing (Encoder)
import Html.Attributes exposing (pattern)


type alias Pattern =
    List Bit


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
