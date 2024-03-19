module Pattern exposing (Type(..), combinedRegex, fromType, typeFrom)

import Regex exposing (Match, Regex)


type Type
    = Email
    | Acronym
    | Digits


type Pattern
    = Pattern Type String Regex


{-| Internal function that crashes for invalid patterns
-}
fromType : Type -> Pattern
fromType t =
    let
        build str =
            case Regex.fromString str of
                Just regex ->
                    Pattern t str regex

                Nothing ->
                    Debug.todo ("Pattern.fromType: invalid " ++ str)
    in
    case t of
        Email ->
            build "[a-zA-Z0-9.!\\#$%&*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:.[a-zA-Z0-9-]+)*"

        Acronym ->
            build "\\b([A-Z][A-Z0-9\\-]{1,}|[A-Z]\\w+[A-Z]+\\w*)\\b"

        Digits ->
            build "\\d[\\d\\,\\.]+"


{-| List of patterns to match. Order is important: from most specific to least
-}
autoPatterns : List Pattern
autoPatterns =
    List.map fromType
        [ Email
        , Acronym
        , Digits
        ]


{-| One regex to match them all (see typeFrom)
-}
combinedRegex : Regex
combinedRegex =
    let
        patternString =
            autoPatterns
                |> List.map
                    (\pattern ->
                        case pattern of
                            Pattern _ str _ ->
                                str
                    )
                |> String.join "|"
    in
    case Regex.fromString patternString of
        Just regex ->
            regex

        Nothing ->
            Debug.todo ("Pattern.combinedRegex: invalid" ++ patternString)


{-| Given matched string, find what type of pattern it matches
-}
typeFrom : Match -> Maybe Type
typeFrom { match } =
    autoPatterns
        |> List.filter
            (\pattern ->
                case pattern of
                    Pattern Email _ regex ->
                        Regex.contains regex match

                    Pattern Acronym _ regex ->
                        Regex.contains regex match

                    Pattern Digits _ regex ->
                        Regex.contains regex match
            )
        |> List.map (\(Pattern t _ _) -> t)
        |> List.head
