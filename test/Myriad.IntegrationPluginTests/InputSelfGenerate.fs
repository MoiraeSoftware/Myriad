namespace InputSelfGenerated

open Myriad.Plugins

[<Generator.Lenses("selflens")>]
[<Generator.Fields "selffields">]
type Test1 = { ones: int; two: string; three: float; four: float32 }
type Test2 = { one: Test1; two: string }

[<Generator.Lenses("selflens", "Example.Lens")>]
type RecordWithWrappedLens =
    { one: int } 

[<Generator.Lenses "selflens">] 
type RecordWithEmptyWrapperName =
    { one_empty_wrapper_name: int } 

[<Generator.Lenses("selflens", typedefof<Example.Lens<_, _>>)>]
type RecordWithWrappedLensViaTypedefof =
    { one_typedefof: Option<int> }

[<Generator.Lenses("selflens", typeof<Example.Lens<_, _>>)>]
type RecordWithWrappedLensViaTypeof =
    { one_typeof: Option<int> }

[<Generator.Lenses("selflens")>]
type SingleCaseDU = Single of int

[<Generator.Lenses("selflens", typeof<Example.Lens<_, _>>)>]
type WrappedSingleCaseDU = SingleWrapped of int

[<RequireQualifiedAccess>]
[<Generator.Lenses("selflens")>]
type FullyQualifiedDU = FullyQualified of string

module ModuleWithDUs =
    [<Generator.Lenses("selflens")>]
    type Module_SingleCaseDU = Single of int

    [<Generator.Lenses("selflens", "Example.Lens")>]
    type Module_WrappedSingleCaseDU = SingleWrapped of int

    [<Generator.Lenses("selflens")>]
    [<RequireQualifiedAccess>]
    type Module_FullyQualifiedDU = FullyQualifiedCase of int

[<Generator.Lenses("selflens", "Example.Lens")>]
type Address = {
    Street : string
    HouseNumber : int
}

[<Generator.Lenses("selflens", "Example.Lens")>]
type Person = {
    Name : string
    Address : Address
}

[<Generator.DuCases "selfdus">]
type Currency =
    | CAD
    | PLN
    | EUR
    | USD
    | Custom of string
    
// Aether style
[<Generator.Lenses("selfpipedsetterlens")>]
type AetherAddress = {
    Street : string
    HouseNumber : int
}

[<Generator.Lenses("selfpipedsetterlens")>]
type AetherPerson = {
    Name : string
    Address : AetherAddress
}
//------------------------------------------------------------------------------
//        This code was generated by myriad.
//        Changes to this file will be lost when the code is regenerated.
//------------------------------------------------------------------------------
namespace rec SelfTestLens

module Test1Lenses =
    open InputSelfGenerated

    let ones =
        (fun (x: Test1) -> x.ones), (fun (x: Test1) (value: int) -> { x with ones = value })

    let two =
        (fun (x: Test1) -> x.two), (fun (x: Test1) (value: string) -> { x with two = value })

    let three =
        (fun (x: Test1) -> x.three), (fun (x: Test1) (value: float) -> { x with three = value })

    let four =
        (fun (x: Test1) -> x.four), (fun (x: Test1) (value: float32) -> { x with four = value })
namespace rec SelfTestLens

module RecordWithWrappedLensLenses =
    open InputSelfGenerated

    let one =
        Example.Lens(
            (fun (x: RecordWithWrappedLens) -> x.one),
            (fun (x: RecordWithWrappedLens) (value: int) -> { x with one = value })
        )
namespace rec SelfTestLens

module RecordWithEmptyWrapperNameLenses =
    open InputSelfGenerated

    let one_empty_wrapper_name =
        (fun (x: RecordWithEmptyWrapperName) -> x.one_empty_wrapper_name),
        (fun (x: RecordWithEmptyWrapperName) (value: int) ->
            { x with
                one_empty_wrapper_name = value })
namespace rec SelfTestLens

module RecordWithWrappedLensViaTypedefofLenses =
    open InputSelfGenerated

    let one_typedefof =
        Example.Lens(
            (fun (x: RecordWithWrappedLensViaTypedefof) -> x.one_typedefof),
            (fun (x: RecordWithWrappedLensViaTypedefof) (value: Option<int>) -> { x with one_typedefof = value })
        )
namespace rec SelfTestLens

module RecordWithWrappedLensViaTypeofLenses =
    open InputSelfGenerated

    let one_typeof =
        Example.Lens(
            (fun (x: RecordWithWrappedLensViaTypeof) -> x.one_typeof),
            (fun (x: RecordWithWrappedLensViaTypeof) (value: Option<int>) -> { x with one_typeof = value })
        )
namespace rec SelfTestLens

module AddressLenses =
    open InputSelfGenerated

    let Street =
        Example.Lens((fun (x: Address) -> x.Street), (fun (x: Address) (value: string) -> { x with Street = value }))

    let HouseNumber =
        Example.Lens(
            (fun (x: Address) -> x.HouseNumber),
            (fun (x: Address) (value: int) -> { x with HouseNumber = value })
        )
namespace rec SelfTestLens

module PersonLenses =
    open InputSelfGenerated

    let Name =
        Example.Lens((fun (x: Person) -> x.Name), (fun (x: Person) (value: string) -> { x with Name = value }))

    let Address =
        Example.Lens((fun (x: Person) -> x.Address), (fun (x: Person) (value: Address) -> { x with Address = value }))
namespace rec SelfAetherTestLens

module AetherAddressLenses =
    open InputSelfGenerated

    let Street =
        (fun (x: AetherAddress) -> x.Street), (fun (value: string) (x: AetherAddress) -> { x with Street = value })

    let HouseNumber =
        (fun (x: AetherAddress) -> x.HouseNumber),
        (fun (value: int) (x: AetherAddress) -> { x with HouseNumber = value })
namespace rec SelfAetherTestLens

module AetherPersonLenses =
    open InputSelfGenerated

    let Name =
        (fun (x: AetherPerson) -> x.Name), (fun (value: string) (x: AetherPerson) -> { x with Name = value })

    let Address =
        (fun (x: AetherPerson) -> x.Address),
        (fun (value: AetherAddress) (x: AetherPerson) -> { x with Address = value })
namespace rec SelfTestLens

module SingleCaseDULenses =
    open InputSelfGenerated

    let Lens' =
        let getter (x: SingleCaseDU) =
            match x with
            | Single x -> x

        getter, (fun (_: SingleCaseDU) (value: int) -> Single value)
namespace rec SelfTestLens

module WrappedSingleCaseDULenses =
    open InputSelfGenerated

    let Lens' =
        Example.Lens(
            let getter (x: WrappedSingleCaseDU) =
                match x with
                | SingleWrapped x -> x

            getter, (fun (_: WrappedSingleCaseDU) (value: int) -> SingleWrapped value)
        )
namespace rec SelfTestLens

module FullyQualifiedDULenses =
    open InputSelfGenerated

    let Lens' =
        let getter (x: FullyQualifiedDU) =
            match x with
            | FullyQualifiedDU.FullyQualified x -> x

        getter, (fun (_: FullyQualifiedDU) (value: string) -> FullyQualifiedDU.FullyQualified value)
namespace rec SelfTestLens

module Module_SingleCaseDULenses =
    open InputSelfGenerated.ModuleWithDUs

    let Lens' =
        let getter (x: Module_SingleCaseDU) =
            match x with
            | Single x -> x

        getter, (fun (_: Module_SingleCaseDU) (value: int) -> Single value)
namespace rec SelfTestLens

module Module_WrappedSingleCaseDULenses =
    open InputSelfGenerated.ModuleWithDUs

    let Lens' =
        Example.Lens(
            let getter (x: Module_WrappedSingleCaseDU) =
                match x with
                | SingleWrapped x -> x

            getter, (fun (_: Module_WrappedSingleCaseDU) (value: int) -> SingleWrapped value)
        )
namespace rec SelfTestLens

module Module_FullyQualifiedDULenses =
    open InputSelfGenerated.ModuleWithDUs

    let Lens' =
        let getter (x: Module_FullyQualifiedDU) =
            match x with
            | Module_FullyQualifiedDU.FullyQualifiedCase x -> x

        getter, (fun (_: Module_FullyQualifiedDU) (value: int) -> Module_FullyQualifiedDU.FullyQualifiedCase value)

namespace rec SelfTestFields

module Test1 =
    open InputSelfGenerated
    let ones (x: Test1) = x.ones
    let two (x: Test1) = x.two
    let three (x: Test1) = x.three
    let four (x: Test1) = x.four
    let create (ones: int) (two: string) (three: float) (four: float32) : Test1 = { }

    let map
        (mapones: int -> int)
        (maptwo: string -> string)
        (mapthree: float -> float)
        (mapfour: float32 -> float32)
        (record': Test1)
        : Test1 =
        { record' with }

namespace rec SelfTestDus

module Currency =
    open InputSelfGenerated

    let toString (x: Currency) : string =
        match x with
        | CAD -> "CAD"
        | PLN -> "PLN"
        | EUR -> "EUR"
        | USD -> "USD"
        | Custom _ -> "Custom"

    let fromString (x: string) : Option<Currency> =
        match x with
        | "CAD" -> Some CAD
        | "PLN" -> Some PLN
        | "EUR" -> Some EUR
        | "USD" -> Some USD
        | _ -> None

    let toTag (x: Currency) : int =
        match x with
        | CAD -> 0
        | PLN -> 1
        | EUR -> 2
        | USD -> 3
        | Custom _ -> 4

    let isCAD (x: Currency) : bool =
        match x with
        | CAD -> true
        | _ -> false

    let isPLN (x: Currency) : bool =
        match x with
        | PLN -> true
        | _ -> false

    let isEUR (x: Currency) : bool =
        match x with
        | EUR -> true
        | _ -> false

    let isUSD (x: Currency) : bool =
        match x with
        | USD -> true
        | _ -> false

    let isCustom (x: Currency) : bool =
        match x with
        | Custom _ -> true
        | _ -> false

