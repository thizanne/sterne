open Stdlib

module T : sig
  type +'a s
  type +'a zero = 'a * 'a
  type +'a one = 'a s * 'a
  type ('m, 'n) add = 'a * 'c constraint 'm = 'a * 'b constraint 'n = 'b * 'c
  type (+_, +_) t = private int
  type (+'a, +'b) len = ('a, 'b) t constraint 'a = 'd one constraint 'b = 'c zero
  type (+'a, +'b) mass = ('a, 'b) t constraint 'a = 'd zero constraint 'b = 'c one
  type (+'a, +'b) adim = ('a, 'b) t constraint 'a = 'd zero constraint 'b = 'c zero

  type (+'a, +'b) prod = ('l, 'm) t
    constraint 'l = 'lp * 'ln
    constraint 'm = 'mp * 'mn
    constraint 'a = ('lp * 'l, 'mp * 'm) t
    constraint 'b = ('l * 'ln, 'm * 'mn) t

  val ( + ) : ('l, 'm) t -> ('l, 'm) t -> ('l, 'm) t
  val ( * ) : ('l, 'm) t -> ('ll, 'mm) t -> ('ll, 'mm) t
  val fac : _ adim -> _ adim
  val adim : int -> _ adim
  val meters : int -> _ len
  val kg : int -> _ mass
end = struct
  type 'a s = S
  type 'a zero = 'a * 'a
  type 'a one = 'a s * 'a
  type ('m, 'n) add = 'a * 'c constraint 'm = 'a * 'b constraint 'n = 'b * 'c
  type ('l, 'm) t = int
  type ('l, 'm) len = ('l, 'm) t constraint 'l = _ one constraint 'm = _ zero
  type ('l, 'm) mass = ('l, 'm) t constraint 'l = _ zero constraint 'm = _ one
  type (+'a, +'b) adim = ('a, 'b) t constraint 'a = 'd zero constraint 'b = 'c zero

  type (+'a, +'b) prod = ('lp * 'ln, 'mp * 'mn) t
    constraint 'a = ('lp * 'l, 'mp * 'n) t constraint 'b = ('l * 'ln, 'm * 'mn) t

  let ( + ) = ( + )
  let ( * ) = ( * )
  let meters m = m
  let kg k = k
  let adim n = n
  let rec fac = function 0 -> 1 | n -> n * fac (n - 1)
end

open T
