module Main exposing (..)

import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html, button, div, form, h1, h4, input, label, node, pre, span, text, textarea)
import Html.Attributes exposing (class, for, href, id, placeholder, rel, title, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra
import Random
import Regex exposing (Regex)
import Task
import Time
import Url


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


type alias Alert =
    { color : String
    , message : String
    }


type alias Model =
    { navKey : Browser.Navigation.Key
    , alert : Maybe Alert
    , lookupTable : List ( String, String )
    , numbersList : List String
    , inputBefore : String
    , inputAfter : String
    , outputBefore : String
    , outputAfter : String
    }


type alias Flags =
    {}


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url.Url
    | UpdateLookup Int String String
    | AddLookup
    | ShuffleLookup (Maybe Time.Posix)
    | OnInput String
    | OnOutput String


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        lookupTable =
            Maybe.withDefault "FYI,John,WD40" url.fragment
                |> String.split ","
                |> List.foldl
                    (\key acc ->
                        acc ++ [ ( key, newWord 1 acc ) ]
                    )
                    []
    in
    ( { navKey = navKey
      , alert = Nothing
      , lookupTable = lookupTable
      , numbersList = []
      , inputBefore = ""
      , inputAfter = ""
      , outputBefore = ""
      , outputAfter = ""
      }
    , Task.perform (always (OnInput "FYI the WD40 costs $42.70 only, John")) (Task.succeed ())
    )


view : Model -> Browser.Document Msg
view model =
    Browser.Document "App"
        [ node "link"
            [ rel "stylesheet"
            , href "https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css"
            ]
            []
        , div
            [ class "flex bg-gray-200 min-h-screen" ]
            [ div
                [ class "md:w-3/5 my-8 ml-auto mr-auto bg-white min-h-full shadow" ]
                [ div [ class "md:p-4" ]
                    [ h1 [ class "text-3xl font-bold mb-5" ] [ text "Loremipsumizer for ChatGPTs" ]
                    , viewMaybe viewAlert model.alert
                    , h4 [] [ text "Replacer words (numbers are automatically obfucscated)" ]
                    , wordsLookupForm model.lookupTable
                    , beforeAfterForm OnInput
                        { colorBefore = "text-pink-500"
                        , colorAfter = "text-teal-500"
                        , titleBefore = "Enter your message"
                        , titleAfter = "Copy+Paste to ChatGPT"
                        , contentBefore = model.inputBefore
                        , contentAfter = model.inputAfter
                        }
                    , beforeAfterForm OnOutput
                        { colorBefore = "text-teal-500"
                        , colorAfter = "text-pink-500"
                        , titleBefore = "Copy+Paste reply from ChatGPT"
                        , titleAfter = "The real reply"
                        , contentBefore = model.outputBefore
                        , contentAfter = model.outputAfter
                        }
                    , h4 [] [ text "Debug" ]
                    , pre [ class "whitespace-pre-wrap" ]
                        [ text (Debug.toString model)
                        ]
                    ]
                ]
            ]
        ]


viewPair : Int -> ( String, String ) -> Html Msg
viewPair index ( leftValue, rightValue ) =
    div [ class "flex items-center space-x-4" ]
        [ input
            [ class "shadow appearance-none border rounded py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
            , type_ "text"
            , value leftValue
            , title "Sensitive word"
            , onInput (\s -> UpdateLookup index s rightValue)
            ]
            []
        , div [ class "text-gray-500" ] [ text "→" ]
        , input
            [ class "shadow appearance-none border rounded py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
            , type_ "text"
            , value rightValue
            , title "Loremipsum word"
            , onInput (\s -> UpdateLookup index leftValue s)
            ]
            []
        ]


wordsLookupForm : List ( String, String ) -> Html Msg
wordsLookupForm pairs =
    div [ class "mb-5" ]
        (List.indexedMap viewPair pairs
            ++ [ div [ class "mt-1" ]
                    [ button
                        [ class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
                        , onClick AddLookup
                        ]
                        [ text "Add Sensitive word → Loremipsum word" ]
                    , text " "

                    -- , button
                    --     [ class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
                    --     , onClick (ShuffleLookup Nothing)
                    --     ]
                    --     [ text "Shuffle" ]
                    ]
               ]
        )


beforeAfterForm :
    (String -> Msg)
    ->
        { colorBefore : String
        , colorAfter : String
        , titleBefore : String
        , titleAfter : String
        , contentBefore : String
        , contentAfter : String
        }
    -> Html Msg
beforeAfterForm msg cfg =
    div [ class "mb-5" ]
        [ form []
            [ div [ class "flex flex-wrap -mx-2" ]
                [ div [ class "w-1/2 px-2" ]
                    [ div [ class ("mh-2 " ++ cfg.colorBefore) ]
                        [ label [ for "before" ] [ text cfg.titleBefore ] ]
                    , textarea
                        [ class ("border border-gray-400 p-2 w-full h-64 " ++ cfg.colorBefore)
                        , id "before"
                        , value cfg.contentBefore
                        , onInput msg
                        ]
                        []
                    ]
                , div [ class "w-1/2 px-2" ]
                    [ div [ class ("mh-2 " ++ cfg.colorAfter) ]
                        [ label [ for "after" ] [ text cfg.titleAfter ] ]
                    , textarea
                        [ class ("border border-gray-400 p-2 w-full h-64 " ++ cfg.colorAfter)
                        , id "after"
                        , value cfg.contentAfter
                        ]
                        []
                    ]
                ]
            ]
        ]


viewMaybe : (a -> Html msg) -> Maybe a -> Html msg
viewMaybe f maybeValue =
    case maybeValue of
        Just a ->
            f a

        Nothing ->
            text ""


viewAlert : Alert -> Html Msg
viewAlert alert =
    div [ class "mb-5" ]
        [ div [ class ("bg-" ++ alert.color ++ "-100 border border-" ++ alert.color ++ "-400 text-" ++ alert.color ++ "-700 px-4 py-3 md:rounded relative") ]
            [ span [ class "block sm:inline" ] [ text alert.message ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update" msg of
        -- [url] decide what to do
        OnUrlRequest (Browser.Internal urlUrl) ->
            ( model, Browser.Navigation.pushUrl model.navKey (Url.toString urlUrl) )

        OnUrlRequest (Browser.External urlString) ->
            ( model, Browser.Navigation.load urlString )

        -- [url] given that we _are at this url_ how should our model change?
        OnUrlChange _ ->
            ( model, Cmd.none )

        UpdateLookup index key value ->
            let
                newLookupTable =
                    if key == "" then
                        model.lookupTable
                            |> List.Extra.removeAt index

                    else
                        List.Extra.updateAt index (always ( key, value )) model.lookupTable
            in
            ( { model
                | lookupTable = newLookupTable
              }
            , Task.perform (always (OnInput model.inputBefore)) (Task.succeed ())
            )

        AddLookup ->
            let
                s =
                    newWord 1 model.lookupTable
            in
            ( { model
                | lookupTable = model.lookupTable ++ [ ( s, s ) ]
              }
            , Cmd.none
            )

        ShuffleLookup Nothing ->
            ( model
            , Time.now
                |> Task.map (\t -> ShuffleLookup (Just t))
                |> Task.perform identity
            )

        ShuffleLookup (Just now) ->
            let
                newLookupTable =
                    model.lookupTable
                        |> List.sortBy
                            (\(( k, v ) as elem) ->
                                let
                                    index =
                                        List.Extra.elemIndex elem model.lookupTable
                                            |> Maybe.withDefault 0
                                in
                                now
                                    |> Time.posixToMillis
                                    |> modBy (1 + ((index + String.length v) * String.length k))
                            )
            in
            ( { model
                | lookupTable = newLookupTable
              }
            , Task.perform (always (OnInput model.inputBefore)) (Task.succeed ())
            )

        OnInput string ->
            let
                loremString =
                    string
                        |> replaceWith loremipsumize (Dict.fromList model.lookupTable)

                newNumbersList =
                    buildDigitsLookup loremString model.numbersList

                newInputAfter =
                    replaceDigits newNumbersList loremString
            in
            ( { model
                | inputBefore = string
                , inputAfter = newInputAfter
                , numbersList = newNumbersList
              }
            , Cmd.batch
                [ Task.perform
                    -- always perform the OnOutput task because there could be a change to model.numbersList
                    (always (OnOutput model.outputBefore))
                    (Task.succeed ())
                , Browser.Navigation.pushUrl model.navKey ("#" ++ String.join "," (List.map Tuple.first model.lookupTable))
                ]
            )

        OnOutput string ->
            let
                newString =
                    string
                        |> unreplaceDigits model.numbersList
                        |> replaceWith unloremipsumize (Dict.fromList model.lookupTable)
            in
            ( { model
                | outputBefore = string
                , outputAfter = newString
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


intToColumnName : Int -> String
intToColumnName n =
    if n <= 0 then
        "Invalid Input"

    else
        let
            -- Recursive helper function to build the column name
            helper num acc =
                if num <= 0 then
                    acc

                else
                    let
                        adjustedNum =
                            num - 1

                        -- Adjusting because A=1, B=2, ..., Z=26
                        charCode =
                            modBy 26 adjustedNum

                        nextNum =
                            adjustedNum // 26

                        newChar =
                            String.fromChar (Char.fromCode (charCode + 65))
                    in
                    helper nextNum (newChar ++ acc)
        in
        helper n ""


stringInTuples : String -> List ( String, String ) -> Bool
stringInTuples target tuples =
    List.any (\( a, b ) -> a == target || b == target) tuples


newWord : Int -> List ( String, String ) -> String
newWord n lookupTable =
    let
        word =
            intToColumnName n
    in
    if stringInTuples word lookupTable then
        newWord (n + 1) lookupTable

    else
        word


replaceWith : (Dict String String -> String -> String) -> Dict String String -> String -> String
replaceWith replacer lookupTable string =
    replacer lookupTable string


loremipsumize : Dict String String -> String -> String
loremipsumize lookupTable string =
    Dict.foldl
        (\k v newstring -> Regex.replace (matchFullword k) (matchReplacer k v) newstring)
        string
        lookupTable


unloremipsumize : Dict String String -> String -> String
unloremipsumize lookupTable string =
    Dict.foldl
        (\k v newstring -> Regex.replace (matchFullword v) (matchReplacer v k) newstring)
        string
        lookupTable


matchReplacer : String -> String -> Regex.Match -> String
matchReplacer before after { match } =
    String.replace match before after


matchFullword : String -> Regex
matchFullword string =
    case Regex.fromString ("\\b" ++ string ++ "\\b") of
        Just regex ->
            regex

        Nothing ->
            -- infinite loop crash, or return Regex.never
            matchFullword (Debug.log "Regex.fromString failed: matchFullword:" string)


matchFulldigit : a -> Regex
matchFulldigit a =
    case Regex.fromString "\\d+" of
        Just regex ->
            regex

        Nothing ->
            -- infinite loop crash, or return Regex.never
            matchFulldigit (Debug.log "Regex.fromString failed: matchFulldigit:" a)


buildDigitsLookup : String -> List String -> List String
buildDigitsLookup string oldList =
    let
        newList =
            Regex.find (matchFulldigit ()) string
                |> List.map .match
    in
    List.Extra.unique (oldList ++ newList)


replaceDigits : List String -> String -> String
replaceDigits numberList string =
    Regex.replace (matchFulldigit ())
        (\{ match } ->
            List.Extra.elemIndex match numberList
                |> Maybe.map (\i -> String.fromInt (i + 1))
                |> Maybe.withDefault "00"
        )
        string


unreplaceDigits : List String -> String -> String
unreplaceDigits numberList string =
    Regex.replace (matchFulldigit ())
        (\{ match } ->
            String.toInt match
                |> Maybe.andThen (\i -> List.Extra.getAt (i - 1) numberList)
                |> Maybe.withDefault "00"
        )
        string
