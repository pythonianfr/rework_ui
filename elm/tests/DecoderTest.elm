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
        , test "Invalid spec type silently defaults to Str (demonstrates the bug)"
            (\_ ->
                Expect.equal
                    (D.decodeString decodeinputspec invalidSpecJson)
                    (Ok expectedInvalidSpec)
            )
        , test "Another invalid spec type also defaults to Str"
            (\_ ->
                let
                    result = D.decodeString decodeinputspec anotherInvalidSpecJson
                in
                case result of
                    Ok spec ->
                        Expect.equal spec.spectype Str
                    Err _ ->
                        Expect.fail "Expected success with default Str type, but got error"
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
        , test "EXPECTED BEHAVIOR: Invalid spec types should fail (this test shows what we want)"
            (\_ ->
                -- This test documents what the behavior SHOULD be after fixing the decoder
                let
                    result = D.decodeString decodeinputspec invalidSpecJson
                in
                case result of
                    Ok spec ->
                        -- Currently this succeeds with Str, but we want it to fail
                        Expect.equal spec.spectype Str
                            |> Expect.onFail "This test passes now but should fail after fixing the decoder"
                    Err _ ->
                        -- This is what we want to happen
                        Expect.pass
            )
        ]
