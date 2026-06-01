module Tests

open System
open System.IO
open Expecto
open Example
open Example.Lens
open Input
open UnknownNamespace
open Fantomas.FCS.Syntax
open Myriad.Core

let private parseSource (source: string) =
    Fantomas.Core.CodeFormatter.ParseAsync(false, source)
    |> Async.RunSynchronously
    |> Array.head
    |> fst

let literalBindingTests =
    testList "Literal binding tests" [
        test "extractLiteralBindings returns string literal" {
            let source = """module A =
    let [<Literal>] MyConst = "Hello"
"""
            let ast = parseSource source
            let bindings = Ast.extractLiteralBindings ast
            Expect.equal bindings.Count 1 "should have one literal binding"
            match bindings.TryFind "MyConst" with
            | Some(SynConst.String(text, _, _)) ->
                Expect.equal text "Hello" "literal value should be 'Hello'"
            | _ -> failwith "Expected string literal binding for MyConst"
        }

        test "extractLiteralBindings ignores non-literal bindings" {
            let source = """module A =
    let nonLiteral = "Hello"
    let [<Literal>] OnlyLiteral = "World"
"""
            let ast = parseSource source
            let bindings = Ast.extractLiteralBindings ast
            Expect.equal bindings.Count 1 "should have only one literal binding"
            Expect.isNone (bindings.TryFind "nonLiteral") "non-literal should not be in bindings"
            Expect.isSome (bindings.TryFind "OnlyLiteral") "literal should be in bindings"
        }

        test "extractLiteralBindings finds literals in nested modules" {
            let source = """module Outer =
    module Inner =
        let [<Literal>] NestedConst = "Nested"
"""
            let ast = parseSource source
            let bindings = Ast.extractLiteralBindings ast
            Expect.isSome (bindings.TryFind "NestedConst") "literal in nested module should be found"
        }

        test "extractLiteralBindings returns integer literal" {
            let source = """module A =
    let [<Literal>] MyInt = 42
"""
            let ast = parseSource source
            let bindings = Ast.extractLiteralBindings ast
            match bindings.TryFind "MyInt" with
            | Some(SynConst.Int32 42) -> ()
            | _ -> failwith "Expected Int32 literal binding for MyInt"
        }

        test "getAttributeConstantsWithBindings resolves ident to string" {
            let source = """module A =
    let [<Literal>] MyConst = "fields"
    [<Generator.Fields(MyConst)>]
    type MyRecord = { Value: int }
"""
            let ast = parseSource source
            let bindings = Ast.extractLiteralBindings ast
            let typeDefs = Ast.extractTypeDefn ast
            let attrib =
                typeDefs
                |> List.collect snd
                |> List.tryPick (fun td -> Ast.getAttribute<Myriad.Plugins.Generator.FieldsAttribute> td)
                |> Option.defaultWith (fun () -> failwith "Generator.FieldsAttribute not found in test source")
            let constants = Ast.getAttributeConstantsWithBindings bindings attrib
            Expect.equal constants ["fields"] "should resolve MyConst to 'fields'"
        }

        test "getAttributeConstantsWithBindings falls back to direct string constant" {
            let source = """module A =
    [<Generator.Fields "lens">]
    type MyRecord = { Value: int }
"""
            let ast = parseSource source
            let bindings = Ast.extractLiteralBindings ast
            let typeDefs = Ast.extractTypeDefn ast
            let attrib =
                typeDefs
                |> List.collect snd
                |> List.tryPick (fun td -> Ast.getAttribute<Myriad.Plugins.Generator.FieldsAttribute> td)
                |> Option.defaultWith (fun () -> failwith "Generator.FieldsAttribute not found in test source")
            let constants = Ast.getAttributeConstantsWithBindings bindings attrib
            Expect.equal constants ["lens"] "should return direct string constant"
        }

        test "getAttributeConstantsWithBindings returns empty for unresolved ident" {
            let source = """module A =
    [<Generator.Fields(UnknownIdent)>]
    type MyRecord = { Value: int }
"""
            let ast = parseSource source
            let bindings = Ast.extractLiteralBindings ast
            let typeDefs = Ast.extractTypeDefn ast
            let attrib =
                typeDefs
                |> List.collect snd
                |> List.tryPick (fun td -> Ast.getAttribute<Myriad.Plugins.Generator.FieldsAttribute> td)
                |> Option.defaultWith (fun () -> failwith "Generator.FieldsAttribute not found in test source")
            let constants = Ast.getAttributeConstantsWithBindings bindings attrib
            Expect.equal constants [] "should return empty list for unresolved ident"
        }
    ]

let editorConfigTests =
    testList "EditorConfig" [
        test "readConfiguration returns FormatConfig.Default when no .editorconfig exists" {
            // Use a path in a temp directory that has no .editorconfig
            let tmpDir = Path.Combine(Path.GetTempPath(), $"myriad_ec_test_noconfig_{Guid.NewGuid()}")
            Directory.CreateDirectory(tmpDir) |> ignore
            try
                let testFile = Path.Combine(tmpDir, "test.fs")
                File.WriteAllText(testFile, "")
                let cfg = Myriad.Core.EditorConfig.readConfiguration testFile
                Expect.equal cfg.IndentSize Fantomas.Core.FormatConfig.Default.IndentSize "IndentSize should match default"
                Expect.equal cfg.MaxLineLength Fantomas.Core.FormatConfig.Default.MaxLineLength "MaxLineLength should match default"
            finally
                Directory.Delete(tmpDir, true)
        }

        test "readConfiguration applies indent_size from .editorconfig" {
            let tmpDir = Path.Combine(Path.GetTempPath(), $"myriad_ec_test_indent_{Guid.NewGuid()}")
            Directory.CreateDirectory(tmpDir) |> ignore
            try
                File.WriteAllText(Path.Combine(tmpDir, ".editorconfig"), "[*.fs]\nindent_size = 2\n")
                let testFile = Path.Combine(tmpDir, "Output.fs")
                File.WriteAllText(testFile, "")
                let cfg = Myriad.Core.EditorConfig.readConfiguration testFile
                Expect.equal cfg.IndentSize 2 "IndentSize should be 2 as per .editorconfig"
            finally
                Directory.Delete(tmpDir, true)
        }

        test "readConfiguration applies max_line_length from .editorconfig" {
            let tmpDir = Path.Combine(Path.GetTempPath(), $"myriad_ec_test_linelen_{Guid.NewGuid()}")
            Directory.CreateDirectory(tmpDir) |> ignore
            try
                File.WriteAllText(Path.Combine(tmpDir, ".editorconfig"), "[*.fs]\nmax_line_length = 80\n")
                let testFile = Path.Combine(tmpDir, "Output.fs")
                File.WriteAllText(testFile, "")
                let cfg = Myriad.Core.EditorConfig.readConfiguration testFile
                Expect.equal cfg.MaxLineLength 80 "MaxLineLength should be 80 as per .editorconfig"
            finally
                Directory.Delete(tmpDir, true)
        }

        test "readConfiguration applies fsharp-specific settings from .editorconfig" {
            let tmpDir = Path.Combine(Path.GetTempPath(), $"myriad_ec_test_fsharp_{Guid.NewGuid()}")
            Directory.CreateDirectory(tmpDir) |> ignore
            try
                File.WriteAllText(Path.Combine(tmpDir, ".editorconfig"), "[*.fs]\nfsharp_space_before_colon = true\n")
                let testFile = Path.Combine(tmpDir, "Output.fs")
                File.WriteAllText(testFile, "")
                let cfg = Myriad.Core.EditorConfig.readConfiguration testFile
                Expect.isTrue cfg.SpaceBeforeColon "SpaceBeforeColon should be true as per .editorconfig"
            finally
                Directory.Delete(tmpDir, true)
        }

        test "readConfiguration respects .editorconfig glob patterns - only applies to .fs files" {
            let tmpDir = Path.Combine(Path.GetTempPath(), $"myriad_ec_test_glob_{Guid.NewGuid()}")
            Directory.CreateDirectory(tmpDir) |> ignore
            try
                // indent_size = 2 only applies to .fs files, not .fsi
                File.WriteAllText(Path.Combine(tmpDir, ".editorconfig"), "[*.fs]\nindent_size = 2\n")
                let testFile = Path.Combine(tmpDir, "Output.fsi")
                File.WriteAllText(testFile, "")
                let cfg = Myriad.Core.EditorConfig.readConfiguration testFile
                Expect.equal cfg.IndentSize Fantomas.Core.FormatConfig.Default.IndentSize "IndentSize should be default for .fsi file"
            finally
                Directory.Delete(tmpDir, true)
        }
    ]

let tests =
    testList "basic tests" [

        test "Test txt based module generator generated with config" {
            Expect.equal TestExample1.First.fourtyTwo 42 "generated value should be 42"
            Expect.equal TestExample1.Second.fourtyTwo 42 "generated value should be 42"
            Expect.equal TestExample1.Third.fourtyTwo 42 "generated value should be 42"
            Expect.equal TestExample1.Fourth.fourtyTwo 42 "generated value should be 42"
        }

        test "Test txt based module generator generated no config" {
            Expect.equal UnknownNamespace.First.fourtyTwo 42 "generated value should be 42"
            Expect.equal UnknownNamespace.Second.fourtyTwo 42 "generated value should be 42"
            Expect.equal UnknownNamespace.Third.fourtyTwo 42 "generated value should be 42"
            Expect.equal UnknownNamespace.Fourth.fourtyTwo 42 "generated value should be 42"
        }

        test "Test1 create Test" {
            let t = TestFields.Test1.create 1 "2" 3. (float32 4)
            Expect.equal t {Test1.one = 1; two = "2"; three = 3.; four = float32 4 } "generated records should be ok"
        }

        test "Test1 accessor Test" {
            let t = TestFields.Test1.create 1 "2" 3. (float32 4)
            let z = TestFields.Test1.one t
            Expect.equal z 1 "generated getters should be ok"
        }

        test "Test1 map Test" {
            let t = TestFields.Test1.create 1 "2" 3. (float32 4)
            let mapped = TestFields.Test1.map (fun x -> x + 1) (fun s -> s + "!") (fun f -> f + 1.) (fun f -> f + float32 1) t
            Expect.equal mapped { Test1.one = 2; two = "2!"; three = 4.; four = float32 5 } "map should transform all fields"
        }

        testList "Lenses" [
            testList "Records" [
                let t = TestFields.Test1.create 1 "2" 3. (float32 4)

                test "Getter" {
                    let getter = fst TestLens.Test1Lenses.one
                    Expect.equal 1 (getter t) "getter returns the value"
                }

                test "Setter" {
                    let setter = snd TestLens.Test1Lenses.one
                    let updated = setter t 2
                    Expect.equal 2 updated.one "setter updates the value"
                }

                test "Wrapped getter" {
                    let (Lens(getter, _)) = TestLens.RecordWithWrappedLensLenses.one
                    let src : RecordWithWrappedLens = { one = 1 }
                    let value = getter src
                    Expect.equal 1 value "getter returns the value"
                }

                test "Wrapped setter" {
                    let (Lens(_, setter)) = TestLens.RecordWithWrappedLensLenses.one
                    let src : RecordWithWrappedLens = { one = 1 }
                    let updated = setter src 2
                    Expect.equal { one = 2 } updated "setter updates the value"
                }

                test "Empty wrapper name" {
                    let (getter, _) = TestLens.RecordWithEmptyWrapperNameLenses.one_empty_wrapper_name
                    let src = { one_empty_wrapper_name = 1 }
                    Expect.equal 1 (getter src) "getter returns the value"
                }
            ]

            testList "Single-case DUs" [
                test "Unwrapped getter" {
                    let getter = fst TestLens.SingleCaseDULenses.Lens'
                    let t = Single 1

                    Expect.equal (getter t) 1 "getter returns the value"
                }
                test "Unwrapped setter" {
                    let setter = snd TestLens.SingleCaseDULenses.Lens'
                    let t = Single 1

                    let updated = setter t 2
                    let (Single actualValue) = updated
                    Expect.equal actualValue 2 "getter returns the value"
                }
                test "Wrapped getter" {
                    let (Lens (getter, _)) = TestLens.WrappedSingleCaseDULenses.Lens'
                    let t = SingleWrapped 1

                    Expect.equal (getter t) 1 "getter returns the value"
                }
                test "Wrapped setter" {
                    let (Lens (_, setter)) = TestLens.WrappedSingleCaseDULenses.Lens'
                    let t = SingleWrapped 1

                    let updated = setter t 2
                    let (SingleWrapped actualValue) = updated
                    Expect.equal actualValue 2 "getter returns the value"
                }
            ]

            test "Lens composition" {
                let houseNumberLens = TestLens.PersonLenses.Address << TestLens.AddressLenses.HouseNumber
                let person: Person = {
                    Name = "Sherlock"
                    Address = {
                        Street = "Baker st."
                        HouseNumber = 221
                    }
                }

                let houseNumber = Lens.get houseNumberLens person

                Expect.equal houseNumber 221 "Gets correct house number"

                let updatedPerson = person |> Lens.set houseNumberLens 1
                let updatedHouseNumber = Lens.get houseNumberLens updatedPerson

                Expect.equal updatedHouseNumber 1 "Gets updated value"
            }
            
            test "Aether get" {
                let person: AetherPerson = {
                    Name = "Sherlock"
                    Address = {
                        Street = "Baker st."
                        HouseNumber = 221
                    }
                }
                let value =  Aether.Optic.get AetherTestLens.AetherPersonLenses.Address person
                Expect.equal value person.Address "Gets the address lens via Aether"
            }

            test "Aether set" {
                let person: AetherPerson = {
                    Name = "Sherlock"
                    Address = {
                        Street = "Baker st."
                        HouseNumber = 221
                    }
                }
                let updated = person |> Aether.Optic.set AetherTestLens.AetherPersonLenses.Address {
                    Street = "Baker st."
                    HouseNumber = 222
                }
                let value =  Aether.Optic.get AetherTestLens.AetherPersonLenses.Address updated
                Expect.equal value { Street = "Baker st."; HouseNumber = 222 } "Sets the address lens via Aether"
            }

            testList "F# 8 dot lambda syntax" [
                let r = { RecordWithDotLambdaMember.count = 42; value = "hello" }

                test "Getter works for record with dot lambda member" {
                    let getter = fst TestLens.RecordWithDotLambdaMemberLenses.count
                    Expect.equal 42 (getter r) "getter returns the count value"
                }

                test "Setter works for record with dot lambda member" {
                    let setter = snd TestLens.RecordWithDotLambdaMemberLenses.count
                    let updated = setter r 99
                    Expect.equal 99 updated.count "setter updates the count value"
                }

                test "String field getter works for record with dot lambda member" {
                    let getter = fst TestLens.RecordWithDotLambdaMemberLenses.value
                    Expect.equal "hello" (getter r) "getter returns the string value"
                }

                test "String field setter works for record with dot lambda member" {
                    let setter = snd TestLens.RecordWithDotLambdaMemberLenses.value
                    let updated = setter r "world"
                    Expect.equal "world" updated.value "setter updates the string value"
                }
            ]
        ]

        testList "DU Cases" [
            testList "toString" [
                test "CAD maps to 'CAD'" {
                    Expect.equal (TestDus.Currency.toString CAD) "CAD" "CAD maps to 'CAD'"
                }
                test "PLN maps to 'PLN'" {
                    Expect.equal (TestDus.Currency.toString PLN) "PLN" "PLN maps to 'PLN'"
                }
                test "EUR maps to 'EUR'" {
                    Expect.equal (TestDus.Currency.toString EUR) "EUR" "EUR maps to 'EUR'"
                }
                test "USD maps to 'USD'" {
                    Expect.equal (TestDus.Currency.toString USD) "USD" "USD maps to 'USD'"
                }
                test "Custom _ maps to 'Custom'" {
                    Expect.equal (TestDus.Currency.toString (Custom "CHF")) "Custom" "Custom _ maps to 'Custom'"
                }
            ]
            testList "fromString" [
                test "parses 'CAD' to Some CAD" {
                    Expect.equal (TestDus.Currency.fromString "CAD") (Some CAD) "parses 'CAD'"
                }
                test "parses 'PLN' to Some PLN" {
                    Expect.equal (TestDus.Currency.fromString "PLN") (Some PLN) "parses 'PLN'"
                }
                test "parses 'EUR' to Some EUR" {
                    Expect.equal (TestDus.Currency.fromString "EUR") (Some EUR) "parses 'EUR'"
                }
                test "parses 'USD' to Some USD" {
                    Expect.equal (TestDus.Currency.fromString "USD") (Some USD) "parses 'USD'"
                }
                test "returns None for unknown string" {
                    Expect.equal (TestDus.Currency.fromString "CHF") None "unknown string returns None"
                }
            ]
            testList "toTag" [
                test "CAD has tag 0" {
                    Expect.equal (TestDus.Currency.toTag CAD) 0 "CAD tag is 0"
                }
                test "PLN has tag 1" {
                    Expect.equal (TestDus.Currency.toTag PLN) 1 "PLN tag is 1"
                }
                test "EUR has tag 2" {
                    Expect.equal (TestDus.Currency.toTag EUR) 2 "EUR tag is 2"
                }
                test "USD has tag 3" {
                    Expect.equal (TestDus.Currency.toTag USD) 3 "USD tag is 3"
                }
                test "Custom has tag 4" {
                    Expect.equal (TestDus.Currency.toTag (Custom "CHF")) 4 "Custom tag is 4"
                }
            ]
            testList "predicates" [
                test "isCAD returns true for CAD" {
                    Expect.isTrue (TestDus.Currency.isCAD CAD) "isCAD is true for CAD"
                }
                test "isCAD returns false for PLN" {
                    Expect.isFalse (TestDus.Currency.isCAD PLN) "isCAD is false for PLN"
                }
                test "isCustom returns true for Custom value" {
                    Expect.isTrue (TestDus.Currency.isCustom (Custom "CHF")) "isCustom is true for Custom"
                }
                test "isCustom returns false for CAD" {
                    Expect.isFalse (TestDus.Currency.isCustom CAD) "isCustom is false for CAD"
                }
            ]
        ]

        literalBindingTests
    ]