namespace JiraToXml.Conversion

module Conversion =
    open System.Xml
    open System.Xml.Linq
    open FSharp.Data

    let encode (name:string) =
        let sanitized = name.Replace(" ", "")
        XmlConvert.EncodeLocalName(sanitized)

    let toXml json =
        let element name (value:obj) =
            let encoded = encode name
            let xName = XName.Get encoded
            XElement(xName, value)

        let rec toXml name json =
            let mapRecord properties =
                properties |> Array.map (fun (name, json) -> toXml name json)

            let mapArray properties =
                properties |> Array.map (fun (json) -> toXml "item" json)

            match json with
            | JsonValue.Array items -> element "items" (mapArray items)
            | JsonValue.Record properties -> element name (mapRecord properties)
            | JsonValue.String string -> element name string    // prevent extra quotes around strings inside elements
            | field -> element name field

        let root = toXml "root" (JsonValue.Parse(json))
        XDocument(root)

namespace JiraToXml.Conversion.Test

open NUnit.Framework
open System.Xml.Linq
open FsUnit

open JiraToXml.Conversion

[<TestFixture>]
type ConvertFixture() =

    let child (xmlDoc:XDocument) name =
        XName.op_Implicit name |> xmlDoc.Root.Element

    [<Test>]
    member x.primitive_fields () =
        let json = """{ "number": 1.123, "string": "test", "boolean": true, "nullable" : null }"""
        let root = Conversion.toXml json |> child

        let number = root "number" |> XElement.op_Explicit : decimal
        number |> should equal 1.123

        let string = (root "string").Value
        string |> should equal "test"

        let boolean = root "boolean" |> XElement.op_Explicit : bool
        boolean |> should equal true

    [<Test>]
    member x.child_records () =
        let json = """{ "child": { "field": "test" } }"""
        let root = Conversion.toXml json |> child

        let element = root "child"
        let field = XName.Get "field" |> element.Element
        field.Value |> should equal "test"
