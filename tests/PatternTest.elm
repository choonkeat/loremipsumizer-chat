module PatternTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Pattern
import Test exposing (..)


suite : Test
suite =
    [ ( "123", Just Pattern.Digits )
    , ( "1,200.03", Just Pattern.Digits )

    --
    , ( "user id", Nothing )
    , ( "user_id", Just Pattern.SnakeCase )
    , ( "_prefix", Just Pattern.SnakeCase )
    , ( "suffix_", Just Pattern.SnakeCase )

    --
    , ( "longword", Nothing )
    , ( "TitleCase123", Just Pattern.AcronymOrCamelCase )
    , ( "camelCase123", Just Pattern.AcronymOrCamelCase )
    , ( "camelCX", Just Pattern.AcronymOrCamelCase )
    , ( "CamelCX", Just Pattern.AcronymOrCamelCase )
    , ( "CAmelcx", Just Pattern.AcronymOrCamelCase )
    , ( "Camel99", Just Pattern.AcronymOrCamelCase )
    , ( "Camel99b", Just Pattern.AcronymOrCamelCase )

    --
    , ( "@example.com", Nothing )
    , ( "user@x", Just Pattern.Email )
    , ( "camel@example.com", Just Pattern.Email )
    , ( "camel123@example.co.uk", Just Pattern.Email )
    , ( "camel+spam@163.cn", Just Pattern.Email )
    , ( "camel_snake@example.com", Just Pattern.Email )
    ]
        |> List.map
            (\( input, expected ) ->
                test (input ++ " should match " ++ Debug.toString expected) <|
                    \_ ->
                        Expect.equal
                            expected
                            (Pattern.typeFrom { match = input })
            )
        |> describe "Pattern"
