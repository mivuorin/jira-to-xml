open System
open System.Net
open System.Text
open System.IO
open System.Xml
open FSharp.Data
open System.Xml.Linq
open System.Threading

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
        | field -> element name field
    let root = toXml "root" (JsonValue.Parse(json))
    XDocument(root)

let loadJson (url:string) (login:string) (pass:string) =
    async {
        let uri = Uri(url)
        let request = WebRequest.Create(uri)
    
        let credentials = NetworkCredential(login, pass);
        do request.Credentials <- credentials
        do request.PreAuthenticate <- true

        let authHeader () =
            let basicAuth = sprintf "%s:%s" login pass
            let bytes = Encoding.ASCII.GetBytes(basicAuth)
            let base64 = Convert.ToBase64String(bytes)
            sprintf "Basic %s" base64

        do request.Headers.Set(HttpRequestHeader.Authorization, authHeader())

        use! response = request.AsyncGetResponse()
        use stream = response.GetResponseStream()
        use reader = new StreamReader(stream)
        let readTask = reader.ReadToEndAsync()
        return! Async.AwaitTask readTask
    }

let readLine () = 
    let readKey () = Console.ReadKey(true)

    let rec readLine chars (key:ConsoleKeyInfo) =
        match key.Key with
        | ConsoleKey.Enter -> chars
        | _ -> readLine (chars @ [key.KeyChar]) (readKey ())

    let chars = readLine [] (readKey ())
    String(chars |> List.toArray)

type ProgressEvent = Event<string>

let printProgressTask =
    let event = ProgressEvent()
    let task = 
        async {
            while true do
                do! Async.Sleep 500
                event.Trigger "."
        }
    (task, event.Publish)

let mainWorkTask (parameters:Parameters.T) =
    let event = ProgressEvent()
    let task =
        async {
            event.Trigger (sprintf "Loading json from url: %s" parameters.url)
            let! json = loadJson parameters.url parameters.user.Value parameters.password.Value
            event.Trigger "Converting xml"
            let xml = toXml json
            event.Trigger (sprintf "Writing file %s" parameters.fileName)
            xml.Save(parameters.fileName)
            event.Trigger "Complete."
        }
    (task, event.Publish)

let verifyCredentials (parameters:Parameters.T) =
    if Option.isNone parameters.user || Option.isNone parameters.password then
        do printfn "User:"
        let user = readLine ()
        do printfn "Pass:"
        let pass = readLine ()
        { parameters with user = Some user; password = Some pass }
    else
        parameters

[<EntryPoint>]
let main argv =
    match Parameters.parse argv with
    | None ->
        Parameters.printMan ()
        -1
    | Some parameters -> 
        let parameters = verifyCredentials parameters

        use tokenSource = new CancellationTokenSource()
        let printTask, printEvents = printProgressTask
        let mainTask, mainEvents = mainWorkTask parameters

        use subs =
            Observable.merge printEvents mainEvents
            |> Observable.subscribe (fun msg -> printfn "%s" msg)

        async {
            try
                Async.Start(printTask, tokenSource.Token)
                do! mainTask
            finally
                tokenSource.Cancel()
        } |> Async.RunSynchronously
        0
