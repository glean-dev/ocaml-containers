
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Equality Combinators} *)

type 'a t = 'a -> 'a -> bool

let poly = Pervasives.(=)
let physical = Pervasives.(==)

let int : int t = (=)
let string : string t = Pervasives.(=)
let bool : bool t = Pervasives.(=)
let float : float t = Pervasives.(=)
let unit () () = true

let list f l1 l2 = Belt.List.eqU l1 l2 (fun [@bs] x y -> f x y)

let array eq a b = Belt.Array.eqU a b (fun [@bs] x y -> eq x y)

let option f o1 o2 = Belt.Option.eqU o1 o2 (fun [@bs] x y -> f x y)

let pair f g (x1,y1)(x2,y2) = f x1 x2 && g y1 y2
let triple f g h (x1,y1,z1)(x2,y2,z2) = f x1 x2 && g y1 y2 && h z1 z2

let map ~f eq x y = eq (f x) (f y)

let always_eq _ _ = true
let never_eq _ _ = false

module Infix = struct
  let (>|=) x f = map ~f x
end

include Infix
