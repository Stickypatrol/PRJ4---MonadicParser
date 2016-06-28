module ParserMonad

type Parser<'a, 's> = 's -> Result<'a, 's>
and Result<'a, 's> =
  | Success of 'a* 's
  | Fail of string

let ret x = fun s -> Success(x, s)

let bind p k =
  fun s ->
    match p s with
    | Success(a, s') -> k a s'
    | Fail(e) -> Fail(e)

type ParserBuilder() =
  member this.Return(x) = ret x
  member this.ReturnFrom(p) = p
  member this.Bind(p,k) = bind p k
  member this.Zero() = ret ()
let parse = ParserBuilder()

let (.||) p k =
  fun s ->
    match p s with
    | Success(a, s') -> Success(a, s')
    | Fail(e) ->
      match k s with
      | Success(a, s') -> Success(a, s')
      | Fail(_) -> Fail(e)

let rec repeatParse p =
  parse{
    let! x = p
    let! xs = repeatParse p
    return x::xs
  }.||
  parse{
    return []
  }

let repeatMultiParse p =
  parse{
    let! x = p
    let! xs = repeatParse p
    return x::xs
  }

let getParState =
  fun s ->
    Success(s,s)