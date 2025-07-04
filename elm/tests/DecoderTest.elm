module DecoderTest exposing (suite)

import Expect
import Json.Decode as D
import Decoder exposing (decodeinputspec)
import Type exposing (SpecType(..), IOSpec)
import Test exposing (Test, describe, test)


-- Test JSON for valid spec types
validSpecJson : String
validSpecJson =
    """
    {
        "type": "string",
        "name": "test_input",
        "required": true,
        "default": null,
        "choices": null
    }
    """


-- Test JSON for invalid spec type that should fail but currently defaults to Str
invalidSpecJson : String
invalidSpecJson =
    """
    {
        "type": "unknown_type",
        "name": "test_input",
        "required": true,
        "default": null,
        "choices": null
    }
    """


-- Test JSON for another invalid spec type
anotherInvalidSpecJson : String
anotherInvalidSpecJson =
    """
    {
        "type": "custom_type",
        "name": "test_input",
        "required": false,
        "default": "default_value",
        "choices": ["option1", "option2"]
    }
    """


expectedValidSpec : IOSpec
expectedValidSpec =
    { spectype = Str
    , name = "test_input"
    , required = True
    , default = Nothing
    , choices = Nothing
    }


expectedInvalidSpec : IOSpec
expectedInvalidSpec =
    { spectype = Str  -- This demonstrates the problem: unknown types default to Str
    , name = "test_input"
    , required = True
    , default = Nothing
    , choices = Nothing
    }


suite : Test
suite =
    describe "Input Spec Decoder Tests"
        [ test "Valid spec type decodes correctly"
            (\_ ->
                Expect.equal
                    (D.decodeString decodeinputspec validSpecJson)
                    (Ok expectedValidSpec)
            )
        , test "Invalid spec type now properly fails (bug fixed!)"
            (\_ ->
                let
                    result = D.decodeString decodeinputspec invalidSpecJson
                in
                case result of
                    Ok _ ->
                        Expect.fail "Expected decoder to fail on unknown spec type, but it succeeded"
                    Err error ->
                        let
                            errorString = D.errorToString error
                        in
                        if String.contains "Unknown spec type: unknown_type" errorString then
                            Expect.pass
                        else
                            Expect.fail <| "Expected error about unknown spec type, but got: " ++ errorString
            )
        , test "Another invalid spec type also properly fails"
            (\_ ->
                let
                    result = D.decodeString decodeinputspec anotherInvalidSpecJson
                in
                case result of
                    Ok _ ->
                        Expect.fail "Expected decoder to fail on custom spec type, but it succeeded"
                    Err error ->
                        let
                            errorString = D.errorToString error
                        in
                        if String.contains "Unknown spec type: custom_type" errorString then
                            Expect.pass
                        else
                            Expect.fail <| "Expected error about custom spec type, but got: " ++ errorString
            )
        , test "All known spec types decode correctly"
            (\_ ->
                let
                    testCases =
                        [ ("file", File)
                        , ("string", Str)
                        , ("number", Num)
                        , ("boolean", Bool)
                        , ("datetime", Datetime)
                        , ("moment", Moment)
                        ]

                    makeJson typeName =
                        """{"type": \"""" ++ typeName ++ """", "name": "test", "required": true, "default": null, "choices": null}"""

                    testCase (typeName, expectedType) =
                        case D.decodeString decodeinputspec (makeJson typeName) of
                            Ok spec ->
                                spec.spectype == expectedType
                            Err _ ->
                                False
                in
                if List.all testCase testCases then
                    Expect.pass
                else
                    Expect.fail "Some spec types did not decode correctly"
            )
        , test "Type safety improvement: decoder fails fast on invalid input"
            (\_ ->
                -- This test demonstrates that the decoder now properly validates input
                let
                    invalidTypes = ["unknown", "typo", "invalid", "random"]
                    testInvalidType typeName =
                        let
                            json = """{"type": \"""" ++ typeName ++ """", "name": "test", "required": true, "default": null, "choices": null}"""
                            result = D.decodeString decodeinputspec json
                        in
                        case result of
                            Ok _ -> False  -- Should not succeed
                            Err _ -> True  -- Should fail
                in
                if List.all testInvalidType invalidTypes then
                    Expect.pass
                else
                    Expect.fail "Some invalid spec types were incorrectly accepted"
            )
        ]
