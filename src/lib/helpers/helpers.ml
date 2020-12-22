let error fmt = Format.kasprintf (fun s -> Error s) fmt

let try_list_map f =
  let rec try_with_early_exit acc = function
    | [] -> Ok acc
    | x :: xs ->
      match f x with
      | Ok x -> try_with_early_exit (x :: acc) xs
      | Error _ as err -> err
  in
  try_with_early_exit []

let rec try_list_iter f = function
  | [] -> Ok ()
  | x :: xs ->
    match f x with
    | Ok () -> try_list_iter f xs
    | Error _ as err -> err

let rec try_seq_iter f s =
  match s () with
  | Seq.Nil -> Ok ()
  | Seq.Cons (x, s') ->
    match f x with
    | Ok () -> try_seq_iter f s'
    | Error _ as err -> err

module Syntax = struct
  let (let*) = Result.bind
  let (let+) x f = Result.map f x
  let (and+) x y =
    match x, y with
    | Ok x, Ok y -> Ok (x, y)
    | (Error e, _ | _, Error e) -> Error e

  let (>>=) = Result.bind
  let (<$>) = Result.map

  (* or_else *)
  let (<|>) x y =
    match x with
    | Ok _ -> x
    | Error _ -> y
end
