module Pattern exposing (Type(..), combinedRegex, fromType, typeFrom)

import Regex exposing (Match, Regex)


{-| Remember to update autoPatterns
-}
type Type
    = Email
    | SnakeCase
    | AcronymOrCamelCase
    | Digits


{-| List of patterns to match. Order is important: from most specific to least
-}
autoPatterns : List Pattern
autoPatterns =
    List.map fromType
        [ Email
        , SnakeCase
        , AcronymOrCamelCase
        , Digits
        ]


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

        SnakeCase ->
            build "\\b([a-z0-9_]+_[a-z0-9_]*|[a-z0-9_]*_[a-z0-9_]+)\\b"

        AcronymOrCamelCase ->
            build "\\b([A-Z][A-Z0-9\\-]{1,}|[A-Z]\\w+[A-Z]+\\w*)\\b"

        Digits ->
            build "\\d[\\d\\,\\.]+"


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

                    Pattern SnakeCase _ regex ->
                        Regex.contains regex match

                    Pattern AcronymOrCamelCase _ regex ->
                        Regex.contains regex match

                    Pattern Digits _ regex ->
                        Regex.contains regex match
            )
        |> List.map (\(Pattern t _ _) -> t)
        |> List.head
