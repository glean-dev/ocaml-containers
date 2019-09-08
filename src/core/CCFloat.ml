
(* This file is free software, part of containers. See file "license" for more details. *)


type t = float
type fpclass = Pervasives.fpclass =
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan

module Infix = struct
  let (=) : t -> t -> bool = Pervasives.(=)
  let (<>) : t -> t -> bool = Pervasives.(<>)
  let (<) : t -> t -> bool = Pervasives.(<)
  let (>) : t -> t -> bool = Pervasives.(>)
  let (<=) : t -> t -> bool = Pervasives.(<=)
  let (>=) : t -> t -> bool = Pervasives.(>=)
  let (~-) : t -> t = Pervasives.(~-.)
  external ( + ) : float -> float -> float = "%addfloat"
  external ( - ) : float -> float -> float = "%subfloat"
  external ( * ) : float -> float -> float = "%mulfloat"
  external ( / ) : float -> float -> float = "%divfloat"
end
include Infix

let nan = Pervasives.nan

let infinity = Pervasives.infinity
let neg_infinity = Pervasives.neg_infinity

let max_value = infinity
let min_value = neg_infinity

let max_finite_value = Pervasives.max_float

let epsilon = Pervasives.epsilon_float

let is_nan x = (Pervasives.(=) (classify_float x) Pervasives.FP_nan)

let add = (+.)
let sub = (-.)
let mul = ( *. )
let div = (/.)
let neg = (~-.)
let abs = Pervasives.abs_float
let scale = ( *. )

let min (x : t) y =
  match Pervasives.classify_float x, Pervasives.classify_float y with
    | FP_nan, _ -> y
    | _, FP_nan -> x
    | _ -> if x < y then x else y

let max (x : t) y =
  match Pervasives.classify_float x, Pervasives.classify_float y with
    | FP_nan, _ -> y
    | _, FP_nan -> x
    | _ -> if x > y then x else y

(*$T
  max nan 1. = 1.
  min nan 1. = 1.
  max 1. nan = 1.
  min 1. nan = 1.
*)

(*$Q
  Q.(pair float float) (fun (x,y) -> \
    is_nan x || is_nan y || (min x y <= x && min x y <= y))
  Q.(pair float float) (fun (x,y) -> \
    is_nan x || is_nan y || (max x y >= x && max x y >= y))
  *)

let equal (a:float) b = a=b

let hash : t -> int = Hashtbl.hash
let compare (a:float) b = Pervasives.compare a b

type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

let pp = Format.pp_print_float

let fsign a =
  if is_nan a then nan
  else if a = 0. then a
  else Pervasives.copysign 1. a

exception TrapNaN of string

let sign_exn (a:float) =
  if is_nan a then raise (TrapNaN "sign_exn")
  else compare a 0.

let round x = Js.Math.round x

(*$=
  2. (round 1.6)
  1. (round 1.4)
  0. (round 0.)
*)

external to_int: float -> int = "%intoffloat"
external of_int: int -> float = "%identity"

external to_string: float -> string = "String" [@@bs.val]

let of_string_exn (a:string) = Pervasives.float_of_string a

external of_string: string -> float = "parseFloat" [@@bs.val]

external is_nan : float -> bool = "isNaN" [@@bs.val]

let of_string i =
  match (of_string i) with
  | i when is_nan i -> None
  | i -> Some i

let random n st = Random.State.float st n
let random_small = random 100.0
let random_range i j st = i +. random (j-.i) st

let equal_precision ~epsilon a b = abs_float (a-.b) < epsilon

let classify = Pervasives.classify_float
