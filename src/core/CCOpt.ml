
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Options} *)

type 'a t = 'a option

let map f o = Belt.Option.mapU o (fun[@bs] x -> f x)

let map_or ~default f o = Belt.Option.mapWithDefaultU o default (fun[@bs] x -> f x)

let map_lazy default_fn f = function
  | None -> default_fn ()
  | Some x -> f x

let is_some = function
  | None -> false
  | Some _ -> true

let is_none = function
  | None -> true
  | Some _ -> false

let compare f o1 o2 = Belt.Option.cmpU o1 o2 (fun[@bs] x y -> f x y)

let equal f o1 o2 = Belt.Option.eqU o1 o2 (fun[@bs] x y -> f x y)

let return x = Some x

let (>|=) x f = Belt.Option.mapU x (fun[@bs] x -> f x)

let (>>=) o f = Belt.Option.flatMapU o (fun[@bs] x -> f x)

let flat_map f o = Belt.Option.flatMapU o (fun[@bs] x -> f x)

let pure x = Some x

let (<*>) f x = match f, x with
  | None, _
  | _, None -> None
  | Some f, Some x -> Some (f x)

let (<$>) = map

let or_ ~else_ a = match a with
  | None -> else_
  | Some _ -> a

let or_lazy ~else_ a = match a with
  | None -> else_ ()
  | Some _ -> a

let (<+>) a b = or_ ~else_:b a

let choice l = CCListLabels.fold_left ~f:(<+>) ~init:None l

let map2 f o1 o2 = match o1, o2 with
  | None, _
  | _, None -> None
  | Some x, Some y -> Some (f x y)

let filter p = function
  | Some x as o when p x -> o
  | _ -> None

(*$=
  None (filter ((=) 0) (Some 1))
  (Some 0) (filter ((=) 0) (Some 0))
  None (filter (fun _ -> true) None)
*)

let if_ p x = if p x then Some x else None

let exists p = function
  | None -> false
  | Some x -> p x

let for_all p = function
  | None -> true
  | Some x -> p x

let iter f o = match o with
  | None -> ()
  | Some x -> f x

let fold f acc o = match o with
  | None -> acc
  | Some x -> f acc x

let get_or ~default x = match x with
  | None -> default
  | Some y -> y

let get_exn = function
  | Some x -> x
  | None -> invalid_arg "CCOpt.get_exn"

let get_lazy default_fn x = match x with
  | None -> default_fn ()
  | Some y -> y

let sequence_l l =
  let rec aux acc l = match l with
    | [] -> Some (CCListLabels.rev acc)
    | Some x :: l' -> aux (x::acc) l'
    | None :: _ -> raise Exit
  in
  try aux [] l with Exit -> None

(*$T
  sequence_l [None; Some 1; Some 2] = None
  sequence_l [Some 1; Some 2; Some 3] = Some [1;2;3]
  sequence_l [] = Some []
*)

let wrap ?(handler=fun _ -> true) f x =
  try Some (f x)
  with e ->
    if handler e then None else raise e

let wrap2 ?(handler=fun _ -> true) f x y =
  try Some (f x y)
  with e ->
    if handler e then None else raise e

let to_list o = match o with
  | None -> []
  | Some x -> [x]

let of_list = function
  | x::_ -> Some x
  | [] -> None

let to_result err = function
  | None -> Belt.Result.Error err
  | Some x -> Belt.Result.Ok x

let to_result_lazy err_fn = function
  | None -> Belt.Result.Error (err_fn ())
  | Some x -> Belt.Result.Ok x

let of_result = function
  | Belt.Result.Error _ -> None
  | Belt.Result.Ok x -> Some x

module Infix = struct
  let (>|=) = (>|=)
  let (>>=) = (>>=)
  let (<*>) = (<*>)
  let (<$>) = (<$>)
  let (<+>) = (<+>)
end

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

let random g st =
  if Random.State.bool st then Some (g st) else None

exception ExitChoice

let choice_seq s =
  let r = ref None in
  begin try
      s (function
        | None -> ()
        | (Some _) as o -> r := o; raise ExitChoice
      )
    with ExitChoice -> ()
  end;
  !r

(*$T
  choice_seq (Iter.of_list [None; Some 1; Some 2]) = Some 1
  choice_seq Iter.empty = None
  choice_seq (Iter.repeat None |> Iter.take 100) = None
*)

let to_gen o =
  match o with
    | None -> (fun () -> None)
    | Some _ ->
      let first = ref true in
      fun () -> if !first then (first:=false; o) else None

let to_seq o k = match o with
  | None -> ()
  | Some x -> k x

let pp ppx out = function
  | None -> Format.pp_print_string out "None"
  | Some x -> Format.fprintf out "@[Some %a@]" ppx x

let flatten = function
  | Some x -> x
  | None -> None

(*$T
  flatten None = None
  flatten (Some None) = None
  flatten (Some (Some 1)) = Some 1
*)

let return_if b x =
  if b then
    Some x
  else
    None

(*$T
  return_if false 1 = None
  return_if true 1 = Some 1
*)
