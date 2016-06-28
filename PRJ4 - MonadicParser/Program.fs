open System
open Parser
open ParserMonad

let x = match SimpleLexer ',' ("sample,csv,line,with,commas,for,spaces" |> List.ofSeq) with
        | Success(a,s) -> Some(a.Head)
        | Fail(e) -> printfn "%A" e
                     None

List.iter (fun (x:CSVTOKEN) -> printfn "%A" x.getValueUNSAFE) x.Value