let (let*) = Result.bind
let (let+) x f = Result.map f x
let (and+) x y =
  match x, y with
  | Ok x, Ok y -> Ok (x, y)
  | (Error e, _ | _, Error e) -> Error e

let (>>=) = Result.bind
