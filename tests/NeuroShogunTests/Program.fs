open Expecto
open NeuroFSharp

type TestType5 =
    { [<SkipSerializingIfEquals(0)>]
      x: int }

[<TagName "abcd">]
type TestType4 =
    | Abc_Def of test: string * test2: TestType5
    | XyzW

type TestType3 =
    | AbcDef
    | XyzW

type TestType2 = { w: TestType3 }

type TestType =
    { x: int option
      y: int array
      z: TestType2 }

let s1 = JsonValue.Parse """ {"y": [5,6,7], "z":{"w":"abc_def"}} """
let s2 = JsonValue.Parse """ {"command":"abc/def","test":"a","test2":{"x":0}} """

let s3 =
    (TypeInfo.serialize (TypeInfo.fromSystemType typeof<TestType4>) (Abc_Def("xyz", { x = 0 })))
        .ToString(JsonSaveOptions.DisableFormatting)

let s4 =
    (TypeInfo.serialize (TypeInfo.fromSystemType typeof<Schema>) (new NumberSchema()))
        .ToString(JsonSaveOptions.DisableFormatting)

let schema =
    (TypeInfo.fromSystemType typeof<TestType> |> TypeInfo.schema)
        .JsonValue()
        .ToString(JsonSaveOptions.DisableFormatting)

let v1 =
    TypeInfo.deserialize List.empty (TypeInfo.fromSystemType typeof<TestType>) s1

let v2 =
    TypeInfo.deserialize List.empty (TypeInfo.fromSystemType typeof<TestType4>) s2

[<Tests>]
let jsonTests =
    testList
        "JSON"
        [ test "deserialization" {
              Expect.equal
                  v1
                  (Ok
                      { x = None
                        y = [| 5; 6; 7 |]
                        z = { w = AbcDef } })
                  "Deserialization"
          }
          test "command deserialization" { Expect.equal v2 (Ok(Abc_Def("a", { x = 0 }))) "Command deserialization" }
          test "schema" {
              Expect.equal
                  schema
                  """{"type":"object","properties":{"x":{"type":"integer"},"y":{"type":"array","items":{"type":"integer"}},"z":{"type":"object","properties":{"w":{"type":"string","enum":["abc_def","xyz_w"]}},"required":["w"]}},"required":["y","z"]}"""
                  "Schema generation"
          }
          test "ser" { Expect.equal s3 """{"abcd":"abc/def","test":"xyz","test2":{}}""" "Serialization" }
          test "ser2" { Expect.equal s4 """{"type":"number"}""" "Serialization" } ]

[<Tests>]
let schemaTests =
    testList
        "schema"
        [ test "invalid" {
              Expect.isTrue
                  (let mutable sch1 = new StringSchema()
                   sch1.SetEnum([||])
                   let sch2 = new ObjectSchema([| "test", sch1 |])
                   let mutable sch3 = sch2.Clone() :?> ObjectSchema
                   sch3.Required <- Some([| "test" |])
                   not sch1.Valid && sch2.Valid && not sch3.Valid)
                  "validity"
          }
          test "equals" {
              let schema = TypeInfo.fromSystemType typeof<TestType> |> TypeInfo.schema
              Expect.isTrue (schema.EqualTo(schema.Clone())) "equals"
          } ]

type ActionsTest =
    | [<Action("test1", "Test 1")>] Test1
    | [<Action("test2", "Test 2")>] Test2 of a: string * b: int

type Game() =
    inherit Game<ActionsTest>()
    override _.Name = "test"
    override _.HandleAction(_: ActionsTest) = Error(Some("idk"))

    override this.ReregisterActions() = this.RegisterActions([ Test1; Test2 ])

    override _.LogError error = printfn $"{error}"

[<EntryPoint>]
let main argv =
    let ret = runTestsInAssemblyWithCLIArgs [] argv
    (*let game = new Game()

    let task =
        game.Start(Some("ws://127.0.0.1:8000"), System.Threading.CancellationToken.None)

    Async.RunSynchronously(Async.AwaitTask task)*)
    ret
