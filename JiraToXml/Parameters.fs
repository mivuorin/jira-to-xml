module Parameters

    type T =  {
        url: string
        user: Option<string>
        password: Option<string>
        fileName: string
    }

    let parse (args:string[]) =
        let defaults url = {
            url = url
            user = None
            password = None
            fileName = "output.xml"
        }

        let parseArg (first, second) parameters =
            match first with
            | "-f" -> Some( { parameters with fileName = second } )
            | "-u" -> Some( { parameters with user = Some second } )
            | "-p" -> Some( { parameters with password = Some second } )
            | _ -> None

        let rec parse args = function
            | None -> None
            | Some options ->
                match args with
                | first :: second :: rest -> 
                    let newOpt = parseArg (first, second) options
                    parse rest newOpt
                | [] -> Some options
                | _ -> None

        let options, rest =
            match args |> Array.toList with
            | first :: rest -> (Some(defaults first), rest)
            | [] -> (None, [])

        parse rest options

    let printMan () =
        let manPage = [
            "Usage: JiraToXml.exe <url>"
            "\turl -  must be fully qualified resource url where get request can be send."
            ""
            "Options"
            "\t-f filename - (optional) filename where response json is saved as xml. default is output.xml"
            "\t-u username - (optional) username which is used when authenticating to jira"
            "\t-p password - (optional) password which is used when authenticating to jira"
            ""
            "\teg. JiraToXml.exe https://jira/rest/api/2/filter/22851 -f result.xml"
            ""
            "\tIf username is not given as option, provide username and password when prompted."
            "\tCredentials are base64 encoded to request header. Do not use without SSL connection."
        ]
        manPage |> List.iter (fun line -> System.Console.WriteLine(line))