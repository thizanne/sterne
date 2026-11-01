type t = [ `Descent_exceeds_runtime ]

exception Exn of t

let ok_or_raise result = match result with Ok x -> x | Error e -> raise (Exn e)
