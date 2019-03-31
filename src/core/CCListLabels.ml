(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Complements to list} *)

(*$inject
  let lsort l = List.sort Pervasives.compare l
*)

include ListLabels

type 'a t = 'a list

let ( <$> ) f l = Belt.List.map l f

let ( >|= ) = Belt.List.map

let append = Belt.List.concat

let combine = Belt.List.zip

let compare f l1 l2 = Belt.List.cmpU l1 l2 (fun [@bs] x y -> f x y)

let concat = Belt.List.flatten

let exists ~f l = Belt.List.someU l (fun [@bs] x -> f x)

let exists2 ~f l1 l2 = Belt.List.some2U l1 l2 (fun [@bs] a b -> f a b)

let filter ~f l = Belt.List.keep l f

let filter_map ~f l = Belt.List.keepMapU l (fun [@bs] x -> f x)

let find_opt ~f l = Belt.List.getByU l (fun [@bs] a -> f a)

let flatten = Belt.List.flatten

let fold_left ~f ~init l =
  Belt.List.reduceU l init (fun [@bs] acc x -> f acc x)

let fold_left2 ~f ~init l1 l2 =
  Belt.List.reduce2U l1 l2 init (fun [@bs] a b c -> f a b c)

let fold_right ~f l ~init =
  Belt.List.reduceReverseU l init (fun [@bs] acc x -> f x acc)

let fold_right2 ~f l1 l2 ~init =
  Belt.List.reduceReverse2U l1 l2 init (fun [@bs] a b c -> f b c a)

let foldi ~f ~init l =
  Belt.List.reduceWithIndexU l init (fun [@bs] acc x i -> f acc i x)

let for_all ~f l = Belt.List.everyU l (fun [@bs] x -> f x)

let for_all2 ~f l1 l2 = Belt.List.every2U l1 l2 (fun [@bs] a b -> f a b)

let hd = Belt.List.headExn

let head = Belt.List.head

let head_opt = Belt.List.head

let init len ~f = Belt.List.makeByU len (fun [@bs] x -> f x)

let iter ~f l = Belt.List.forEachU l (fun [@bs] x -> f x)

let iter2 ~f l1 l2 = Belt.List.forEach2U l1 l2 (fun [@bs] a b -> f a b)

let iteri ~f l = Belt.List.forEachWithIndexU l (fun [@bs] i x -> f i x)

let length = Belt.List.length

let map ~f l = Belt.List.map l f

let mapi ~f l = Belt.List.mapWithIndexU l (fun [@bs] i x -> f i x)

let nth = Belt.List.getExn

let nth_opt = Belt.List.get

let partition ~f l = Belt.List.partitionU l (fun [@bs] x -> f x)

let rev = Belt.List.reverse

let rev_append = Belt.List.reverseConcat

let rev_map ~f l = Belt.List.mapReverseU l (fun [@bs] x -> f x)

let rev_map2 ~f l1 l2 = Belt.List.mapReverse2U l1 l2 (fun [@bs] a b -> f a b)

let size = Belt.List.size

let sort ~cmp l = Belt.List.sortU l (fun [@bs] x y -> cmp x y)

let fast_sort = sort

let find_all = filter

let stable_sort = sort

let split = Belt.List.unzip

let tail_opt = Belt.List.tail

let tl = Belt.List.tailExn

let empty = []

let is_empty = function [] -> true | _ :: _ -> false

(* max depth for direct recursion *)
let direct_depth_default_ = 1000

let tail_map f l =
  (* Unwind the list of tuples, reconstructing the full list front-to-back.
     @param tail_acc a suffix of the final list; we append tuples' content
     at the front of it *)
  let rec rebuild tail_acc = function
    | [] ->
        tail_acc
    | (y0, y1, y2, y3, y4, y5, y6, y7, y8) :: bs ->
        rebuild
          (y0 :: y1 :: y2 :: y3 :: y4 :: y5 :: y6 :: y7 :: y8 :: tail_acc)
          bs
  in
  (* Create a compressed reverse-list representation using tuples
     @param tuple_acc a reverse list of chunks mapped with [f] *)
  let rec dive tuple_acc = function
    | x0 :: x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: xs ->
        let y0 = f x0 in
        let y1 = f x1 in
        let y2 = f x2 in
        let y3 = f x3 in
        let y4 = f x4 in
        let y5 = f x5 in
        let y6 = f x6 in
        let y7 = f x7 in
        let y8 = f x8 in
        dive ((y0, y1, y2, y3, y4, y5, y6, y7, y8) :: tuple_acc) xs
    | xs ->
        (* Reverse direction, finishing off with a direct map *)
        let tail = map ~f xs in
        rebuild tail tuple_acc
  in
  dive [] l

(*$Q
  (Q.list Q.small_int) (fun l -> \
    let f x = x+1 in \
    rev (rev_map f l) = map f l)
*)

let direct_depth_append_ = 10_000

let cons x l = x :: l

let ( @ ) = append

(*$T
  [1;2;3] @ [4;5;6] = [1;2;3;4;5;6]
  (1-- 10_000) @ (10_001 -- 20_000) = 1 -- 20_000
*)

let cons_maybe o l = match o with Some x -> x :: l | None -> l

(*$T
  cons_maybe (Some 1) [2;3] = [1;2;3]
  cons_maybe None [2;3] = [2;3]
*)

(*$= & ~printer:CCInt.to_string
  500 (filter (fun x->x mod 2 = 0) (1 -- 1000) |> List.length)
  50_000 (filter (fun x->x mod 2 = 0) (1 -- 100_000) |> List.length)
  500_000 (filter (fun x->x mod 2 = 0) (1 -- 1_000_000) |> List.length)
*)

(*$T
  fold_right (+) (1 -- 1_000_000) 0 = \
    List.fold_left (+) 0 (1 -- 1_000_000)
*)

(*$Q
  (Q.list Q.small_int) (fun l -> \
    l = fold_right (fun x y->x::y) l [])
*)

let rec fold_while ~f ~init = function
  | [] ->
      init
  | e :: l -> (
      let acc, cont = f init e in
      match cont with `Stop -> acc | `Continue -> fold_while ~f ~init l )

(*$T
  fold_while (fun acc b -> if b then acc+1, `Continue else acc, `Stop) 0 [true;true;false;true] = 2
*)

let fold_map ~f ~init l =
  let rec aux f acc map_acc l =
    match l with
    | [] ->
        (acc, rev map_acc)
    | x :: l' ->
        let acc, y = f acc x in
        aux f acc (y :: map_acc) l'
  in
  aux f init [] l

(*$=
  (6, ["1"; "2"; "3"]) \
    (fold_map (fun acc x->acc+x, string_of_int x) 0 [1;2;3])
*)

(*$Q
  Q.(list int) (fun l -> \
    fold_map (fun acc x -> x::acc, x) [] l = (rev l, l))
*)

let scan_left ~f ~init l =
  let rec aux f acc l_acc l =
    match l with
    | [] ->
        rev l_acc
    | x :: tail ->
        let acc = f acc x in
        let l_acc = acc :: l_acc in
        aux f acc l_acc tail
  in
  aux f init [init] l

(*$= & ~printer:Q.Print.(list int)
  [0;1;3;6] (scan_left (+) 0 [1;2;3])
  [0] (scan_left (+) 0 [])
*)

(*$Q
  Q.(list int) (fun l -> \
    List.length l + 1 = List.length (scan_left (+) 0 l))
*)

let fold_map2 ~f ~init l1 l2 =
  let rec aux f acc map_acc l1 l2 =
    match (l1, l2) with
    | [], [] ->
        (acc, rev map_acc)
    | [], _ | _, [] ->
        invalid_arg "fold_map2"
    | x1 :: l1', x2 :: l2' ->
        let acc, y = f acc x1 x2 in
        aux f acc (y :: map_acc) l1' l2'
  in
  aux f init [] l1 l2

(*$=
  (310, ["1 10"; "2 0"; "3 100"]) \
    (fold_map2 (fun acc x y->acc+x*y, string_of_int x ^ " " ^ string_of_int y) \
    0 [1;2;3] [10;0;100])
*)

(*$T
  (try ignore (fold_map2 (fun _ _ _ -> assert false) 42 [] [1]); false \
   with Invalid_argument _ -> true)
*)

let fold_filter_map ~f ~init l =
  let rec aux f acc map_acc l =
    match l with
    | [] ->
        (acc, rev map_acc)
    | x :: l' ->
        let acc, y = f acc x in
        aux f acc (cons_maybe y map_acc) l'
  in
  aux f init [] l

(*$= & ~printer:Q.Print.(pair int (list int))
  (List.fold_left (+) 0 (1--10), [2;4;6;8;10]) \
  (fold_filter_map (fun acc x -> acc+x, if x mod 2 = 0 then Some x else None) \
    0 (1--10))
*)

let fold_flat_map ~f ~init l =
  let rec aux f acc map_acc l =
    match l with
    | [] ->
        (acc, rev map_acc)
    | x :: l' ->
        let acc, y = f acc x in
        aux f acc (rev_append y map_acc) l'
  in
  aux f init [] l

(*$=
  (6, ["1"; "a1"; "2"; "a2"; "3"; "a3"]) \
    (let pf = Printf.sprintf in \
      fold_flat_map (fun acc x->acc+x, [pf "%d" x; pf "a%d" x]) 0 [1;2;3])
*)

(*$Q
  Q.(list int) (fun l -> \
    fold_flat_map (fun acc x -> x::acc, [x;x+10]) [] l = \
      (rev l, flat_map (fun x->[x;x+10]) l) )
*)

(*$T
  init 0 (fun _ -> 0) = []
  init 1 (fun x->x) = [0]
  init 1000 (fun x->x) = 0--999
*)

let compare_length_with l len = ListLabels.compare_length_with l ~len

let rec equal f l1 l2 =
  match (l1, l2) with
  | [], [] ->
      true
  | [], _ | _, [] ->
      false
  | x1 :: l1', x2 :: l2' ->
      f x1 x2 && equal f l1' l2'

(*$T
  equal CCInt.equal (1--1_000_000) (1--1_000_000)
*)

let flat_map ~f l =
  let rec aux f l kont =
    match l with
    | [] ->
        kont []
    | x :: l' ->
        let y = f x in
        let kont' tail =
          match y with
          | [] ->
              kont tail
          | [x] ->
              kont (x :: tail)
          | [x; y] ->
              kont (x :: y :: tail)
          | l ->
              kont (append l tail)
        in
        aux f l' kont'
  in
  aux f l (fun l -> l)

(*$T
  flat_map (fun x -> [x+1; x*2]) [10;100] = [11;20;101;200]
  List.length (flat_map (fun x->[x]) (1--300_000)) = 300_000
*)

(*$T
  flatten [[1]; [2;3;4]; []; []; [5;6]] = 1--6
  flatten (init 300_001 (fun x->[x])) = 0--300_000
*)

let count ~f l = fold_left ~f:(fun n x -> if f x then succ n else n) ~init:0 l

(*$T
  count (fun x -> x mod 2 = 0) [] = 0
  count (fun x -> x mod 2 = 0) [0; 0; 2; 4] = 4
  count (fun x -> x mod 2 = 0) [1; 3; 5; 7] = 0
  count (fun x -> x mod 2 = 0) [2; 6; 9; 4] = 3
*)

let count_true_false ~f l =
  fold_left
    ~f:(fun (ok, ko) x -> if f x then (ok + 1, ko) else (ok, ko + 1))
    ~init:(0, 0) l

(*$T
  count_true_false (fun x -> x mod 2 = 0) [] = (0, 0)
  count_true_false (fun x -> x mod 2 = 0) [0; 0; 2; 4] = (4, 0)
  count_true_false (fun x -> x mod 2 = 0) [1; 3; 5; 7] = (0, 4)
  count_true_false (fun x -> x mod 2 = 0) [2; 6; 9; 4] = (3, 1)
*)

let product ~f l1 l2 = flat_map ~f:(fun x -> map ~f:(fun y -> f x y) l2) l1

let fold_product ~f ~init l1 l2 =
  fold_left
    ~f:(fun acc x1 -> fold_left ~f:(fun acc x2 -> f acc x1 x2) ~init:acc l2)
    ~init l1

let diagonal l =
  let rec gen acc l =
    match l with
    | [] ->
        acc
    | x :: l' ->
        let acc = fold_left ~f:(fun acc y -> (x, y) :: acc) ~init:acc l' in
        gen acc l'
  in
  gen [] l

(*$T
  diagonal [] = []
  diagonal [1] = []
  diagonal [1;2] = [1,2]
  diagonal [1;2;3] |> List.sort Pervasives.compare = [1, 2; 1, 3; 2, 3]
*)

let partition_map ~f l =
  let rec iter f l1 l2 l =
    match l with
    | [] ->
        (rev l1, rev l2)
    | x :: tl -> (
      match f x with
      | `Left y ->
          iter f (y :: l1) l2 tl
      | `Right y ->
          iter f l1 (y :: l2) tl
      | `Drop ->
          iter f l1 l2 tl )
  in
  iter f [] [] l

(*$R
  let l1, l2 =
    partition_map (function
      | n when n = 0 -> `Drop
      | n when n mod 2 = 0 -> `Left n
      | n -> `Right n
    ) [0;1;2;3;4]
  in
  assert_equal [2;4] l1;
  assert_equal [1;3] l2
*)

let combine l1 l2 =
  let rec direct i l1 l2 =
    match (l1, l2) with
    | [], [] ->
        []
    | _ when i = 0 ->
        safe l1 l2 []
    | x1 :: l1', x2 :: l2' ->
        (x1, x2) :: direct (i - 1) l1' l2'
    | _, _ ->
        invalid_arg "CCList.combine"
  and safe l1 l2 acc =
    match (l1, l2) with
    | [], [] ->
        rev acc
    | x1 :: l1', x2 :: l2' ->
        safe l1' l2' @@ ((x1, x2) :: acc)
    | _, _ ->
        invalid_arg "CCList.combine"
  in
  direct direct_depth_default_ l1 l2

(*$T
  try ignore (combine [1] []); false with Invalid_argument _ -> true
  try ignore (combine (1--1001) (1--1002)); false with Invalid_argument _ -> true
  combine [1;2;3] [3;2;1] = List.combine [1;2;3] [3;2;1]
  combine (1 -- 100_000) (1 -- 100_000) = List.combine (1 -- 100_000) (1 -- 100_000)
*)

(*$Q
  Q.(let p = small_list int in pair p p)(fun (l1,l2) -> \
    if List.length l1=List.length l2 \
    then CCList.combine l1 l2 = List.combine l1 l2 \
    else Q.assume_fail() )
*)

let combine_gen l1 l2 =
  let l1 = ref l1 in
  let l2 = ref l2 in
  fun () ->
    match (!l1, !l2) with
    | [], _ | _, [] ->
        None
    | x1 :: tail1, x2 :: tail2 ->
        l1 := tail1 ;
        l2 := tail2 ;
        Some (x1, x2)

(*$Q
  Q.(let p = small_list int in pair p p)(fun (l1,l2) -> \
    let n = min (List.length l1) (List.length l2) in \
    let res1 = combine (take n l1) (take n l2) in \
    let res2 = combine_gen l1 l2 |> of_gen in \
    res1 = res2)
*)

let split l =
  let rec direct i l =
    match l with
    | [] ->
        ([], [])
    | [(x1, y1)] ->
        ([x1], [y1])
    | [(x1, y1); (x2, y2)] ->
        ([x1; x2], [y1; y2])
    | [(x1, y1); (x2, y2); (x3, y3)] ->
        ([x1; x2; x3], [y1; y2; y3])
    | [(x1, y1); (x2, y2); (x3, y3); (x4, y4)] ->
        ([x1; x2; x3; x4], [y1; y2; y3; y4])
    | _ when i = 0 ->
        split_slow [] [] l
    | (x1, y1) :: (x2, y2) :: (x3, y3) :: (x4, y4) :: (x5, y5) :: l' ->
        let rx, ry = direct (i - 1) l' in
        (x1 :: x2 :: x3 :: x4 :: x5 :: rx, y1 :: y2 :: y3 :: y4 :: y5 :: ry)
  and split_slow acc1 acc2 l =
    match l with
    | [] ->
        (rev acc1, rev acc2)
    | (x1, x2) :: tail ->
        let acc1 = x1 :: acc1 and acc2 = x2 :: acc2 in
        split_slow acc1 acc2 tail
  in
  direct direct_depth_default_ l

(*$Q
  (Q.(list_of_size Gen.(0--10_000) (pair small_int small_string))) (fun l -> \
    let (l1, l2) = split l in \
    List.length l1 = List.length l \
    && List.length l2 = List.length l)

  Q.(list_of_size Gen.(0--10_000) (pair small_int small_int)) (fun l -> \
    split l = List.split l)
*)

let return x = [x]

let ( >>= ) l f = flat_map ~f l

let pure = return

let ( <*> ) funs l = product ~f:(fun f x -> f x) funs l

let cartesian_product l =
  (* [left]: elements picked so far
     [right]: sets to pick elements from
     [acc]: accumulator for the result, to pass to continuation
     [k]: continuation *)
  let rec prod_rec left right k acc =
    match right with
    | [] ->
        k acc (rev left)
    | l1 :: tail ->
        fold_left
          ~f:(fun acc x -> prod_rec (x :: left) tail k acc)
          ~init:acc l1
  in
  prod_rec [] l (fun acc l' -> l' :: acc) []

(*$inject
  let cmp_lii_unord l1 l2 : bool =
    List.sort CCOrd.compare l1 = List.sort CCOrd.compare l2
*)

(*$= & ~printer:Q.Print.(list (list int)) ~cmp:cmp_lii_unord
  [[1;3;4];[1;3;5];[1;3;6];[2;3;4];[2;3;5];[2;3;6]] \
    (cartesian_product [[1;2];[3];[4;5;6]])
  [] (cartesian_product [[1;2];[];[4;5;6]])
  [[]] (cartesian_product [])
  [[1;3;4;5;6];[2;3;4;5;6]] \
    (cartesian_product [[1;2];[3];[4];[5];[6]])
*)

(* cartesian product of lists of lists *)
let map_product_l ~f l =
  let l = map ~f l in
  cartesian_product l

(*$Q
  Q.(list_of_size Gen.(1--4) (list_of_size Gen.(0--4) small_int)) (fun l-> \
    cmp_lii_unord (cartesian_product l) (map_product_l CCFun.id l))
*)

let sorted_merge ~cmp l1 l2 =
  let rec recurse cmp acc l1 l2 =
    match (l1, l2) with
    | [], _ ->
        rev_append acc l2
    | _, [] ->
        rev_append acc l1
    | x1 :: l1', x2 :: l2' ->
        let c = cmp x1 x2 in
        if c < 0 then recurse cmp (x1 :: acc) l1' l2
        else if c > 0 then recurse cmp (x2 :: acc) l1 l2'
        else recurse cmp (x1 :: x2 :: acc) l1' l2'
  in
  recurse cmp [] l1 l2

(*$T
  equal CCInt.equal (List.sort CCInt.compare ([(( * )2); ((+)1)] <*> [10;100])) \
    [11; 20; 101; 200]
  equal CCInt.equal (sorted_merge ~cmp:CCInt.compare [1;1;2] [1;2;3]) [1;1;1;2;2;3]
*)

(*$Q
  Q.(pair (list int) (list int)) (fun (l1,l2) -> \
    List.length (sorted_merge ~cmp:CCInt.compare l1 l2) = List.length l1 + List.length l2)
*)

(*$T
  sort_uniq ~cmp:CCInt.compare [1;2;5;3;6;1;4;2;3] = [1;2;3;4;5;6]
  sort_uniq ~cmp:CCInt.compare [] = []
  sort_uniq ~cmp:CCInt.compare [10;10;10;10;1;10] = [1;10]
*)

let is_sorted ~cmp l =
  let rec aux cmp = function
    | [] | [_] ->
        true
    | x :: (y :: _ as tail) ->
        cmp x y <= 0 && aux cmp tail
  in
  aux cmp l

(*$Q
  Q.(list small_int) (fun l -> \
    is_sorted ~cmp:CCInt.compare (List.sort Pervasives.compare l))
*)

let sorted_insert ~cmp ?(uniq = false) x l =
  let rec aux cmp uniq x left l =
    match l with
    | [] ->
        rev_append left [x]
    | y :: tail -> (
      match cmp x y with
      | 0 ->
          let l' = if uniq then l else x :: l in
          rev_append left l'
      | n when n < 0 ->
          rev_append left (x :: l)
      | _ ->
          aux cmp uniq x (y :: left) tail )
  in
  aux cmp uniq x [] l

(*$Q
    Q.(pair small_int (list small_int)) (fun (x,l) -> \
      let l = List.sort Pervasives.compare l in \
      is_sorted ~cmp:CCInt.compare (sorted_insert ~cmp:CCInt.compare x l))
    Q.(pair small_int (list small_int)) (fun (x,l) -> \
      let l = List.sort Pervasives.compare l in \
      is_sorted ~cmp:CCInt.compare (sorted_insert ~cmp:CCInt.compare ~uniq:true x l))
    Q.(pair small_int (list small_int)) (fun (x,l) -> \
      let l = List.sort Pervasives.compare l in \
      is_sorted ~cmp:CCInt.compare (sorted_insert ~cmp:CCInt.compare ~uniq:false x l))
    Q.(pair small_int (list small_int)) (fun (x,l) -> \
      let l = List.sort Pervasives.compare l in \
      let l' = sorted_insert ~cmp:CCInt.compare ~uniq:false x l in \
      List.length l' = List.length l + 1)
    Q.(pair small_int (list small_int)) (fun (x,l) -> \
      let l = List.sort Pervasives.compare l in \
      List.mem x (sorted_insert ~cmp:CCInt.compare x l))
*)

let uniq_succ ~eq l =
  let rec f acc l =
    match l with
    | [] ->
        rev acc
    | [x] ->
        rev (x :: acc)
    | x :: (y :: _ as tail) when eq x y ->
        f acc tail
    | x :: tail ->
        f (x :: acc) tail
  in
  f [] l

(*$T
  uniq_succ ~eq:CCInt.equal [1;1;2;3;1;6;6;4;6;1] = [1;2;3;1;6;4;6;1]
*)

let group_succ ~eq l =
  let rec f ~eq acc cur l =
    match (cur, l) with
    | [], [] ->
        rev acc
    | _ :: _, [] ->
        rev (rev cur :: acc)
    | [], x :: tl ->
        f ~eq acc [x] tl
    | y :: _, x :: tl when eq x y ->
        f ~eq acc (x :: cur) tl
    | _, x :: tl ->
        f ~eq (rev cur :: acc) [x] tl
  in
  f ~eq [] [] l

(*$T
  group_succ ~eq:CCInt.equal [1;2;3;1;1;2;4] = [[1]; [2]; [3]; [1;1]; [2]; [4]]
  group_succ ~eq:CCInt.equal [] = []
  group_succ ~eq:CCInt.equal [1;1;1] = [[1;1;1]]
  group_succ ~eq:CCInt.equal [1;2;2;2] = [[1]; [2;2;2]]
  group_succ ~eq:(fun (x,_)(y,_)-> x=y) [1, 1; 1, 2; 1, 3; 2, 0] \
    = [[1, 1; 1, 2; 1, 3]; [2, 0]]
*)

let sorted_merge_uniq ~cmp l1 l2 =
  let push ~cmp acc x =
    match acc with [] -> [x] | y :: _ when cmp x y > 0 -> x :: acc | _ -> acc
    (* duplicate, do not yield *)
  in
  let rec recurse ~cmp acc l1 l2 =
    match (l1, l2) with
    | [], l | l, [] ->
        let acc = fold_left ~f:(push ~cmp) ~init:acc l in
        rev acc
    | x1 :: l1', x2 :: l2' ->
        let c = cmp x1 x2 in
        if c < 0 then recurse ~cmp (push ~cmp acc x1) l1' l2
        else if c > 0 then recurse ~cmp (push ~cmp acc x2) l1 l2'
        else recurse ~cmp acc l1 l2'
    (* drop one of the [x] *)
  in
  recurse ~cmp [] l1 l2

(*$T
  sorted_merge_uniq ~cmp:CCInt.compare [1; 1; 2; 3; 5; 8] [1; 2; 3; 4; 6; 8; 9; 9] = [1;2;3;4;5;6;8;9]
*)

(*$Q
  Q.(list int) (fun l -> \
    let l = List.sort Pervasives.compare l in \
    sorted_merge_uniq ~cmp:CCInt.compare l [] = uniq_succ ~eq:CCInt.equal l)
  Q.(list int) (fun l -> \
    let l = List.sort Pervasives.compare l in \
    sorted_merge_uniq ~cmp:CCInt.compare [] l = uniq_succ ~eq:CCInt.equal l)
  Q.(pair (list int) (list int)) (fun (l1, l2) -> \
    let l1 = List.sort Pervasives.compare l1 \
    and l2 = List.sort Pervasives.compare l2 in \
    let l3 = sorted_merge_uniq ~cmp:CCInt.compare l1 l2 in \
    uniq_succ ~eq:CCInt.equal l3 = l3)
*)

let take n l =
  let rec direct i n l =
    match l with
    | [] ->
        []
    | _ when i = 0 ->
        safe n [] l
    | x :: l' ->
        if n > 0 then x :: direct (i - 1) (n - 1) l' else []
  and safe n acc l =
    match l with
    | [] ->
        Belt.List.reverse acc
    | _ when n = 0 ->
        Belt.List.reverse acc
    | x :: l' ->
        safe (n - 1) (x :: acc) l'
  in
  direct direct_depth_default_ n l

(*$T
  take 2 [1;2;3;4;5] = [1;2]
  take 10_000 (range 0 100_000) |> List.length = 10_000
  take 10_000 (range 0 2_000) = range 0 2_000
  take 300_000 (1 -- 400_000) = 1 -- 300_000
*)

(*$Q
  (Q.pair (Q.list Q.small_int) Q.int) (fun (l,i) -> \
    let i = abs i in \
    let l1 = take i l in \
    List.length l1 <= i && ((List.length l1 = i) = (List.length l >= i)))
*)

let rec drop n l =
  match l with [] -> [] | _ when n = 0 -> l | _ :: l' -> drop (n - 1) l'

let hd_tl = function [] -> failwith "hd_tl" | x :: l -> (x, l)

(*$T
  try ignore (hd_tl []); false with Failure _ -> true
  hd_tl [1;2;3] = (1, [2;3])
*)

let take_drop n l = (take n l, drop n l)

(*$Q
  (Q.pair (Q.list Q.small_int) Q.int) (fun (l,i) -> \
    let i = abs i in \
    let l1, l2 = take_drop i l in \
    l1 @ l2 = l )
*)

let sublists_of_len ?(last = fun _ -> None) ?offset ~len:n l =
  if n < 1 then invalid_arg "sublists_of_len: n must be > 0" ;
  let offset =
    match offset with
    | None ->
        n
    | Some o when o < 1 ->
        invalid_arg "sublists_of_len: offset must be > 0"
    | Some o ->
        o
  in
  (* add sub-lists of [l] to [acc] *)
  let rec aux acc l =
    let group = take n l in
    if is_empty group then acc (* this was the last group, we are done *)
    else if length group < n (* last group, with missing elements *) then
      match last group with None -> acc | Some group' -> group' :: acc
    else
      let l' = drop offset l in
      aux (group :: acc) l'
    (* continue *)
  in
  rev (aux [] l)

(*$= sublists_of_len as subs & ~printer:Q.Print.(list (list int))
  [[1;2;3]] (subs 3 [1;2;3;4])
  [[1;2]; [3;4]; [5;6]] (subs 2 [1;2;3;4;5;6])
  [] (subs 3 [1;2])
  [[1;2];[3;4]] (subs ~offset:2 2 [1;2;3;4])
  [[1;2];[2;3]] (subs ~offset:1 2 [1;2;3])
  [[1;2];[4;5]] (subs ~offset:3 2 [1;2;3;4;5;6])
  [[1;2;3];[4]] (subs ~last:CCOpt.return 3 [1;2;3;4])
  [[1;2]; [3;4]] (subs 2 [1;2;3;4;5])
*)

let intersperse ~x l =
  let rec aux_direct i x l =
    match l with
    | [] ->
        []
    | [_] ->
        l
    | _ when i = 0 ->
        aux_tailrec [] x l
    | y :: tail ->
        y :: x :: aux_direct (i - 1) x tail
  and aux_tailrec acc x l =
    match l with
    | [] ->
        rev acc
    | [y] ->
        rev (y :: acc)
    | y :: tail ->
        aux_tailrec (x :: y :: acc) x tail
  in
  aux_direct 1_000 x l

(*$=
  [] (intersperse 0 [])
  [1] (intersperse 0 [1])
  [1;0;2;0;3;0;4] (intersperse 0 [1;2;3;4])
*)

(*$Q
  Q.(pair int (list int)) (fun (x,l) -> \
    length (intersperse x l) = (if length l <= 1 then length l else 2 * length l-1))
  Q.(pair int (list int)) (fun (x,l) -> \
    rev (intersperse x l) = intersperse x (rev l))
*)

let interleave l1 l2 : _ list =
  let rec aux acc l1 l2 =
    match (l1, l2) with
    | [], [] ->
        rev acc
    | [], _ ->
        rev (rev_append l2 acc)
    | _, [] ->
        rev (rev_append l1 acc)
    | x1 :: tl1, x2 :: tl2 ->
        aux (x2 :: x1 :: acc) tl1 tl2
  in
  aux [] l1 l2

(*$=
  [1;2;3;4;5]  (interleave [1;3] [2;4;5])
  [1;2;3] (interleave [1] [2;3])
*)

(*$Q
  Q.(pair (small_list int)(small_list int)) (fun (l1,l2) -> \
    length (interleave l1 l2) = length l1 + length l2)
  Q.(small_list int) (fun l -> l = interleave [] l)
  Q.(small_list int) (fun l -> l = interleave l [])
*)

let take_while ~f:p l =
  let rec direct i p l =
    match l with
    | [] ->
        []
    | _ when i = 0 ->
        safe p [] l
    | x :: l' ->
        if p x then x :: direct (i - 1) p l' else []
  and safe p acc l =
    match l with
    | [] ->
        rev acc
    | x :: l' ->
        if p x then safe p (x :: acc) l' else rev acc
  in
  direct direct_depth_default_ p l

(*$T
  take_while (fun x->x<10) (1 -- 20) = (1--9)
  take_while (fun x->x <> 0) [0;1;2;3] = []
  take_while (fun _ -> true) [] = []
  take_while (fun _ -> true) (1--10) = (1--10)
*)

(*$Q
  Q.(pair (fun1 Observable.int bool) (list small_int)) (fun (f,l) -> \
    let l1 = take_while (Q.Fn.apply f) l in \
    List.for_all (Q.Fn.apply f) l1)
*)

let rec drop_while ~f l =
  match l with [] -> [] | x :: l' -> if f x then drop_while ~f l' else l

(*$Q
  Q.(pair (fun1 Observable.int bool) (list small_int)) (fun (f,l) -> \
    take_while (Q.Fn.apply f) l @ drop_while (Q.Fn.apply f) l = l)
*)

let take_drop_while ~f:p l =
  let rec direct i p l =
    match l with
    | [] ->
        ([], [])
    | _ when i = 0 ->
        safe p [] l
    | x :: tail ->
        if p x then
          let l1, l2 = direct (i - 1) p tail in
          (x :: l1, l2)
        else ([], l)
  and safe p acc l =
    match l with
    | [] ->
        (rev acc, [])
    | x :: tail ->
        if p x then safe p (x :: acc) tail else (rev acc, l)
  in
  direct direct_depth_default_ p l

(*$Q
  Q.(pair (fun1 Observable.int bool) (list small_int)) (fun (f,l) -> \
    let l1,l2 = take_drop_while (Q.Fn.apply f) l in \
    (l1 = take_while (Q.Fn.apply f) l) && (l2 = drop_while (Q.Fn.apply f) l))
*)

let last n l =
  let len = length l in
  if len < n then l else drop (len - n) l

(*$= & ~printer:Q.Print.(option (list int))
  (Some [2;3]) (tail_opt [1;2;3])
  (Some []) (tail_opt [1])
  None (tail_opt [])
*)

let rec last_opt = function
  | [] ->
      None
  | [x] ->
      Some x
  | _ :: tail ->
      last_opt tail

(*$= & ~printer:Q.Print.(option int)
  (Some 1) (head_opt [1;2;3])
  (Some 1) (head_opt [1])
  None (head_opt [])
  (Some 3) (last_opt [1;2;3])
  (Some 1) (last_opt [1])
  None (last_opt [])
*)

let find_pred = find_opt

let find_pred_exn ~f l = ListLabels.find ~f l

(*$T
  find_pred ((=) 4) [1;2;5;4;3;0] = Some 4
  find_pred (fun _ -> true) [] = None
  find_pred (fun _ -> false) (1 -- 10) = None
  find_pred (fun x -> x < 10) (1 -- 9) = Some 1
*)

let find_mapi ~f l =
  let rec aux f i = function
    | [] ->
        None
    | x :: l' -> (
      match f i x with Some _ as res -> res | None -> aux f (i + 1) l' )
  in
  aux f 0 l

let find_map ~f l = find_mapi ~f:(fun _ -> f) l

let find_idx ~f l =
  find_mapi ~f:(fun i x -> if f x then Some (i, x) else None) l

(*$T
  find_map (fun x -> if x=3 then Some "a" else None) [1;2;3;4] = Some "a"
  find_map (fun x -> if x=3 then Some "a" else None) [1;2;4;5] = None
*)

let remove ~eq ~key:x l =
  let rec remove' eq x acc l =
    match l with
    | [] ->
        rev acc
    | y :: tail when eq x y ->
        remove' eq x acc tail
    | y :: tail ->
        remove' eq x (y :: acc) tail
  in
  remove' eq x [] l

(*$T
  remove ~eq:CCInt.equal 1 [2;1;3;3;2;1] = [2;3;3;2]
  remove ~eq:CCInt.equal 10 [1;2;3] = [1;2;3]
*)

(*$=
  ["2"; "4"] \
    (filter_map (fun x -> if x mod 2 = 0 then Some (string_of_int x) else None) \
      [1;2;3;4;5])
  [ "2"; "4"; "6" ] \
    (filter_map (fun x -> if x mod 2 = 0 then Some (string_of_int x) else None) \
      [ 1; 2; 3; 4; 5; 6 ])
*)

let keep_some l = filter_map ~f:(fun x -> x) l

let keep_ok l =
  filter_map
    ~f:(function Belt.Result.Ok x -> Some x | Belt.Result.Error _ -> None)
    l

let all_some l =
  try Some (map ~f:(function Some x -> x | None -> raise Exit) l)
  with Exit -> None

(*$=
  (Some []) (all_some [])
  (Some [1;2;3]) (all_some [Some 1; Some 2; Some 3])
  None (all_some [Some 1; None; None; Some 4])
*)

let all_ok l =
  let err = ref None in
  try
    Belt.Result.Ok
      (map
         ~f:(function
           | Belt.Result.Ok x ->
               x
           | Belt.Result.Error e ->
               err := Some e ;
               raise Exit )
         l)
  with Exit -> (
    match !err with None -> assert false | Some e -> Belt.Result.Error e )

let group_by (type k) ?(hash = Hashtbl.hash) ?(eq = Pervasives.( = )) l =
  let module Tbl = Hashtbl.Make (struct
    type t = k

    let equal = eq

    let hash = hash
  end) in
  (* compute group table *)
  let tbl = Tbl.create 32 in
  iter
    ~f:(fun x ->
      let l = try Tbl.find tbl x with Not_found -> [] in
      Tbl.replace tbl x (x :: l) )
    l ;
  Tbl.fold (fun _ x acc -> x :: acc) tbl []

let join ~join_row s1 s2 : _ t =
  flat_map ~f:(fun a -> filter_map ~f:(join_row a) s2) s1

(*$R
  let s1 = (1 -- 3) in
  let s2 = ["1"; "2"] in
  let join_row i j =
    if string_of_int i = j then Some (string_of_int i ^ " = " ^ j) else None
  in
  let s = join ~join_row s1 s2 in
  OUnit.assert_equal ["1 = 1"; "2 = 2"] s;
*)

let join_by (type a) ?(eq = Pervasives.( = )) ?(hash = Hashtbl.hash) f1 f2
    ~merge c1 c2 =
  let module Tbl = Hashtbl.Make (struct
    type t = a

    let equal = eq

    let hash = hash
  end) in
  let tbl = Tbl.create 32 in
  iter
    ~f:(fun x ->
      let key = f1 x in
      Tbl.add tbl key x )
    c1 ;
  let res = ref [] in
  iter
    ~f:(fun y ->
      let key = f2 y in
      let xs = Tbl.find_all tbl key in
      iter
        ~f:(fun x ->
          match merge key x y with None -> () | Some z -> res := z :: !res )
        xs )
    c2 ;
  !res

type ('a, 'b) join_all_cell =
  {mutable ja_left: 'a list; mutable ja_right: 'b list}

let join_all_by (type a) ?(eq = Pervasives.( = )) ?(hash = Hashtbl.hash) f1 f2
    ~merge c1 c2 =
  let module Tbl = Hashtbl.Make (struct
    type t = a

    let equal = eq

    let hash = hash
  end) in
  let tbl = Tbl.create 32 in
  (* build the map [key -> cell] *)
  iter
    ~f:(fun x ->
      let key = f1 x in
      try
        let c = Tbl.find tbl key in
        c.ja_left <- x :: c.ja_left
      with Not_found -> Tbl.add tbl key {ja_left= [x]; ja_right= []} )
    c1 ;
  iter
    ~f:(fun y ->
      let key = f2 y in
      try
        let c = Tbl.find tbl key in
        c.ja_right <- y :: c.ja_right
      with Not_found -> Tbl.add tbl key {ja_left= []; ja_right= [y]} )
    c2 ;
  Tbl.fold
    (fun key cell res ->
      match merge key cell.ja_left cell.ja_right with
      | None ->
          res
      | Some z ->
          z :: res )
    tbl []

let group_join_by (type a) ?(eq = Pervasives.( = )) ?(hash = Hashtbl.hash) f c1
    c2 =
  let module Tbl = Hashtbl.Make (struct
    type t = a

    let equal = eq

    let hash = hash
  end) in
  let tbl = Tbl.create 32 in
  iter ~f:(fun x -> Tbl.replace tbl x []) c1 ;
  iter
    ~f:(fun y ->
      (* project [y] into some element of [c1] *)
      let key = f y in
      try
        let l = Tbl.find tbl key in
        Tbl.replace tbl key (y :: l)
      with Not_found -> () )
    c2 ;
  Tbl.fold (fun k v l -> (k, v) :: l) tbl []

(*$=
  ['a', ["abc"; "attic"]; \
   'b', ["barbary"; "boom"; "bop"]; \
   'c', []] \
  (group_join_by (fun s->s.[0]) \
    (CCString.to_list "abc") \
    ["abc"; "boom"; "attic"; "deleted"; "barbary"; "bop"] \
  |> map (fun (c,l)->c,List.sort Pervasives.compare l) \
  |> sort Pervasives.compare)
*)

(*$inject
  open Result
*)

(*$=
  (Ok []) (all_ok [])
  (Ok [1;2;3]) (all_ok [Ok 1; Ok 2; Ok 3])
  (Error "e2") (all_ok [Ok 1; Error "e2"; Error "e3"; Ok 4])
*)

let mem ~eq x l =
  let rec search eq x l =
    match l with [] -> false | y :: l' -> eq x y || search eq x l'
  in
  search eq x l

let add_nodup ~eq x l = if mem ~eq x l then l else x :: l

let remove_one ~eq x l =
  let rec remove_one ~eq x acc l =
    match l with
    | [] ->
        assert false
    | y :: tl when eq x y ->
        rev_append acc tl
    | y :: tl ->
        remove_one ~eq x (y :: acc) tl
  in
  if mem ~eq x l then remove_one ~eq x [] l else l

(*$Q
  Q.(pair int (list int)) (fun (x,l) -> \
    remove_one ~eq:CCInt.equal x (add_nodup ~eq:CCInt.equal x l) = l)
  Q.(pair int (list int)) (fun (x,l) -> \
    mem ~eq:CCInt.equal x l || List.length (add_nodup ~eq:CCInt.equal x l) = List.length l + 1)
  Q.(pair int (list int)) (fun (x,l) -> \
    not (mem ~eq:CCInt.equal x l) || List.length (remove_one ~eq:CCInt.equal x l) = List.length l - 1)
*)

let subset ~eq l1 l2 = for_all ~f:(fun t -> mem ~eq t l2) l1

let uniq ~eq l =
  let rec uniq eq acc l =
    match l with
    | [] ->
        rev acc
    | x :: xs when exists ~f:(eq x) xs ->
        uniq eq acc xs
    | x :: xs ->
        uniq eq (x :: acc) xs
  in
  uniq eq [] l

(*$T
  uniq ~eq:CCInt.equal [1;2;3] |> List.sort Pervasives.compare = [1;2;3]
  uniq ~eq:CCInt.equal [1;1;2;2;3;4;4;2;4;1;5] |> List.sort Pervasives.compare = [1;2;3;4;5]
*)

(*$Q
  Q.(small_list small_int) (fun l -> \
    sort_uniq ~cmp:CCInt.compare l = (uniq ~eq:CCInt.equal l |> sort Pervasives.compare))
*)

let union ~eq l1 l2 =
  let rec union eq acc l1 l2 =
    match l1 with
    | [] ->
        rev_append acc l2
    | x :: xs when mem ~eq x l2 ->
        union eq acc xs l2
    | x :: xs ->
        union eq (x :: acc) xs l2
  in
  union eq [] l1 l2

(*$T
  union ~eq:CCInt.equal [1;2;4] [2;3;4;5] = [1;2;3;4;5]
*)

let inter ~eq l1 l2 =
  let rec inter eq acc l1 l2 =
    match l1 with
    | [] ->
        rev acc
    | x :: xs when mem ~eq x l2 ->
        inter eq (x :: acc) xs l2
    | _ :: xs ->
        inter eq acc xs l2
  in
  inter eq [] l1 l2

(*$T
  inter ~eq:CCInt.equal [1;2;4] [2;3;4;5] = [2;4]
*)

(*$T
  mapi (fun i x -> i*x) [10;10;10] = [0;10;20]
*)

let iteri2 ~f l1 l2 =
  let rec aux f i l1 l2 =
    match (l1, l2) with
    | [], [] ->
        ()
    | [], _ | _, [] ->
        invalid_arg "iteri2"
    | x1 :: l1', x2 :: l2' ->
        f i x1 x2 ;
        aux f (i + 1) l1' l2'
  in
  aux f 0 l1 l2

let foldi2 ~f ~init:acc l1 l2 =
  let rec foldi f acc i l1 l2 =
    match (l1, l2) with
    | [], [] ->
        acc
    | [], _ | _, [] ->
        invalid_arg "foldi2"
    | x1 :: l1', x2 :: l2' ->
        let acc = f acc i x1 x2 in
        foldi f acc (i + 1) l1' l2'
  in
  foldi f acc 0 l1 l2

let rec get_at_idx_rec i l =
  match l with
  | [] ->
      raise Not_found
  | x :: _ when i = 0 ->
      x
  | _ :: l' ->
      get_at_idx_rec (i - 1) l'

let get_at_idx_exn i l =
  let i = if i < 0 then length l + i else i in
  get_at_idx_rec i l

let get_at_idx i l = try Some (get_at_idx_exn i l) with Not_found -> None

(*$T
  get_at_idx 0 (range 0 10) = Some 0
  get_at_idx 5 (range 0 10) = Some 5
  get_at_idx 11 (range 0 10) = None
  get_at_idx (-1) (range 0 10) = Some 10
  get_at_idx 0 [] = None
  get_at_idx (-1) [] = None
*)

let set_at_idx i x l0 =
  let rec aux l acc i =
    match l with
    | [] ->
        l0
    | _ :: l' when i = 0 ->
        rev_append acc (x :: l')
    | y :: l' ->
        aux l' (y :: acc) (i - 1)
  in
  let i = if i < 0 then length l0 + i else i in
  aux l0 [] i

(*$T
  set_at_idx 0 10 [1;2;3] = [10;2;3]
  set_at_idx 4 10 [1;2;3] = [1;2;3]
  set_at_idx 1 10 [1;2;3] = [1;10;3]
  set_at_idx (-2) 10 [1;2;3] = [1;10;3]
*)

let insert_at_idx i x l =
  let rec aux l acc i x =
    match l with
    | [] ->
        rev_append acc [x]
    | y :: l' when i = 0 ->
        rev_append acc (x :: y :: l')
    | y :: l' ->
        aux l' (y :: acc) (i - 1) x
  in
  let i = if i < 0 then length l + i else i in
  aux l [] i x

(*$T
  insert_at_idx 0 10 [1;2;3] = [10;1;2;3]
  insert_at_idx 4 10 [1;2;3] = [1;2;3;10]
  insert_at_idx 1 10 [1;2;3] = [1;10;2;3]
  insert_at_idx (-2) 10 [1;2;3] = [1;10;2;3]
*)

let remove_at_idx i l0 =
  let rec aux l acc i =
    match l with
    | [] ->
        l0
    | _ :: l' when i = 0 ->
        rev_append acc l'
    | y :: l' ->
        aux l' (y :: acc) (i - 1)
  in
  let i = if i < 0 then length l0 + i else i in
  aux l0 [] i

(*$T
  remove_at_idx 0 [1;2;3;4] = [2;3;4]
  remove_at_idx 3 [1;2;3;4] = [1;2;3]
  remove_at_idx 5 [1;2;3;4] = [1;2;3;4]
  remove_at_idx (-1) [1;2;3;4] = [1;2;3]
  remove_at_idx (-2) [1;2;3;4] = [1;2;4]
  remove_at_idx (-3) [1;2;3;4] = [1;3;4]
  remove_at_idx (-4) [1;2;3;4] = [2;3;4]
*)

let range_by ~step i j =
  let rec range i j acc =
    if i = j then i :: acc else range i (j - step) (j :: acc)
  in
  if step = 0 then raise (Invalid_argument "CCList.range_by")
  else if if step > 0 then i > j else i < j then []
  else range i (((j - i) / step * step) + i) []

(* note: the last test checks that no error occurs due to overflows. *)
(*$T
  range_by ~step:1   0 0 = [0]
  range_by ~step:1   5 0 = []
  range_by ~step:2   1 0 = []
  range_by ~step:2   0 4 = [0;2;4]
  range_by ~step:2   0 5 = [0;2;4]
  range_by ~step:~-1 0 0 = [0]
  range_by ~step:~-1 0 5 = []
  range_by ~step:~-2 0 1 = []
  range_by ~step:~-2 5 1 = [5;3;1]
  range_by ~step:~-2 5 0 = [5;3;1]
  range_by ~step:max_int 0 2 = [0]
*)

(*$Q
  Q.(pair small_int small_int) (fun (i,j) -> \
    let i = min i j and j = max i j in \
    range_by ~step:1 i j = range i j)
*)

let range i j =
  let rec up i j acc = if i = j then i :: acc else up i (j - 1) (j :: acc)
  and down i j acc = if i = j then i :: acc else down i (j + 1) (j :: acc) in
  if i <= j then up i j [] else down i j []

(*$T
  range 0 5 = [0;1;2;3;4;5]
  range 0 0 = [0]
  range 5 2 = [5;4;3;2]
*)

let range' i j =
  if i < j then range i (j - 1) else if i = j then [] else range i (j + 1)

(*$T
  range' 0 0 = []
  range' 0 5 = [0;1;2;3;4]
  range' 5 2 = [5;4;3]
*)

let ( -- ) = range

let ( --^ ) = range'

(*$T
  append (range 0 100) (range 101 1000) = range 0 1000
  append (range 1000 501) (range 500 0) = range 1000 0
*)

(*$Q
  Q.(pair small_int small_int) (fun (a,b) -> \
    let l = (a--^b) in not (List.mem b l))
*)

let replicate i x =
  let rec aux acc i = if i = 0 then acc else aux (x :: acc) (i - 1) in
  aux [] i

(*$T
  repeat 2 [1;2;3] = [1;2;3;1;2;3]
*)

(*$Q
  Q.(pair small_int (small_list int)) (fun (n,l) -> \
    if n>0 then repeat n l = flat_map (fun _ -> l) (1--n) \
    else Q.assume_fail())
*)

let repeat i l =
  let rec aux acc i =
    if i = 0 then rev acc else aux (rev_append l acc) (i - 1)
  in
  aux [] i

module Assoc = struct
  type ('a, 'b) t = ('a * 'b) list

  let get ~eq x l = Belt.List.getAssocU l x (fun [@bs] a b -> eq a b)

  let get_exn ~eq x l =
    match get ~eq x l with Some x -> x | None -> raise Not_found

  (*$T
    Assoc.get ~eq:CCInt.equal 1 [1, "1"; 2, "2"] = Some "1"
    Assoc.get ~eq:CCInt.equal 2 [1, "1"; 2, "2"] = Some "2"
    Assoc.get ~eq:CCInt.equal 3 [1, "1"; 2, "2"] = None
    Assoc.get ~eq:CCInt.equal 42 [] = None
  *)

  (* search for a binding for [x] in [l], and calls [f x (Some v) rest]
     or [f x None rest] depending on whether it finds the binding.
     [rest] is the list of the other bindings *)
  let rec search_set eq acc l x ~f =
    match l with
    | [] ->
        f x None acc
    | (x', y') :: l' ->
        if eq x x' then f x (Some y') (rev_append acc l')
        else search_set eq ((x', y') :: acc) l' x ~f

  let set ~eq x y l = Belt.List.setAssocU l x y (fun [@bs] a b -> eq a b)

  (*$T
    Assoc.set ~eq:CCInt.equal 2 "two" [1,"1"; 2, "2"] |> List.sort Pervasives.compare \
      = [1, "1"; 2, "two"]
    Assoc.set ~eq:CCInt.equal 3 "3" [1,"1"; 2, "2"] |> List.sort Pervasives.compare \
      = [1, "1"; 2, "2"; 3, "3"]
  *)

  let mem ~eq x l = Belt.List.hasAssocU l x (fun [@bs] a b -> eq a b)

  (*$T
    Assoc.mem ~eq:CCInt.equal 1 [1,"1"; 2,"2"; 3, "3"]
    not (Assoc.mem ~eq:CCInt.equal 4 [1,"1"; 2,"2"; 3, "3"])
  *)
  let update ~eq ~f x l =
    search_set eq [] l x ~f:(fun x opt_y rest ->
        match f opt_y with
        | None ->
            rest (* drop *)
        | Some y' ->
            (x, y') :: rest )

  (*$=
    [1,"1"; 2,"22"] \
      (Assoc.update ~eq:CCInt.equal \
        (function Some "2" -> Some "22" | _ -> assert false) 2 [1,"1"; 2,"2"] |> lsort)
    [1,"1"; 3,"3"] \
      (Assoc.update ~eq:CCInt.equal \
        (function Some "2" -> None | _ -> assert false) 2 [1,"1"; 2,"2"; 3,"3"] |> lsort)
    [1,"1"; 2,"2"; 3,"3"] \
      (Assoc.update ~eq:CCInt.equal \
        (function None -> Some "3" | _ -> assert false) 3 [1,"1"; 2,"2"] |> lsort)
  *)

  let remove ~eq x l = Belt.List.removeAssocU l x (fun [@bs] a b -> eq a b)

  (*$=
    [1,"1"] \
      (Assoc.remove ~eq:CCInt.equal 2 [1,"1"; 2,"2"] |> lsort)
    [1,"1"; 3,"3"] \
      (Assoc.remove ~eq:CCInt.equal 2 [1,"1"; 2,"2"; 3,"3"] |> lsort)
    [1,"1"; 2,"2"] \
      (Assoc.remove ~eq:CCInt.equal 3 [1,"1"; 2,"2"] |> lsort)
  *)
end

let assoc = Assoc.get_exn

let assoc_opt = Assoc.get

let mem_assoc = Assoc.mem

let remove_assoc = Assoc.remove

(** {2 References on Lists} *)

module Ref = struct
  type 'a t = 'a list ref

  let push l x = l := x :: !l

  let pop l =
    match !l with
    | [] ->
        None
    | x :: tail ->
        l := tail ;
        Some x

  let pop_exn l =
    match !l with
    | [] ->
        failwith "CCList.Ref.pop_exn"
    | x :: tail ->
        l := tail ;
        x

  let create () = ref []

  let clear l = l := []

  let lift f l = f !l

  let push_list r l = r := rev_append l !r

  (*$T
    let l = Ref.create() in Ref.push l 1; Ref.push_list l [2;3]; !l = [3;2;1]
  *)
end

(** {2 Monadic Operations} *)
module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Traverse (M : MONAD) = struct
  open! M

  let map_m ~f l =
    let rec aux f acc l =
      match l with
      | [] ->
          return (rev acc)
      | x :: tail ->
          f x >>= fun x' -> aux f (x' :: acc) tail
    in
    aux f [] l

  let rec map_m_par ~f l =
    match l with
    | [] ->
        M.return []
    | x :: tl ->
        let x' = f x in
        let tl' = map_m_par ~f tl in
        x' >>= fun x' -> tl' >>= fun tl' -> M.return (x' :: tl')

  let sequence_m l = map_m ~f:(fun x -> x) l

  let rec fold_m ~f ~init l =
    match l with
    | [] ->
        return init
    | x :: l' ->
        f init x >>= fun acc' -> fold_m ~f ~init:acc' l'
end

(** {2 Conversions} *)

type 'a sequence = ('a -> unit) -> unit

type 'a gen = unit -> 'a option

type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]

type 'a printer = Format.formatter -> 'a -> unit

type 'a random_gen = Random.State.t -> 'a

let random_len len g st = init len ~f:(fun _ -> g st)

(*$T
  random_len 10 CCInt.random_small (Random.State.make [||]) |> List.length = 10
*)

let random g st =
  let len = Random.State.int st 1_000 in
  random_len len g st

let random_non_empty g st =
  let len = 1 + Random.State.int st 1_000 in
  random_len len g st

let random_choose l =
  match l with
  | [] ->
      raise Not_found
  | _ :: _ ->
      let len = length l in
      fun st ->
        let i = Random.State.int st len in
        nth l i

let random_sequence l st = map ~f:(fun g -> g st) l

let to_seq l k = iter ~f:k l

let of_seq seq =
  let l = ref [] in
  seq (fun x -> l := x :: !l) ;
  rev !l

(*$Q
  Q.(list int) (fun l -> of_seq (to_seq l) = l)
*)

let to_gen l =
  let l = ref l in
  fun () ->
    match !l with
    | [] ->
        None
    | x :: l' ->
        l := l' ;
        Some x

let of_gen g =
  let rec direct i g =
    if i = 0 then safe [] g
    else match g () with None -> [] | Some x -> x :: direct (i - 1) g
  and safe acc g =
    match g () with None -> rev acc | Some x -> safe (x :: acc) g
  in
  direct direct_depth_default_ g

(*$Q
  Q.(list int) (fun l -> of_gen(to_gen l) = l)
*)

let to_klist l =
  let rec make l () =
    match l with [] -> `Nil | x :: l' -> `Cons (x, make l')
  in
  make l

let of_klist l =
  let rec direct i g =
    if i = 0 then safe [] g
    else match l () with `Nil -> [] | `Cons (x, l') -> x :: direct (i - 1) l'
  and safe acc l =
    match l () with `Nil -> rev acc | `Cons (x, l') -> safe (x :: acc) l'
  in
  direct direct_depth_default_ l

module Infix = struct
  let ( >|= ) = ( >|= )

  let ( @ ) = ( @ )

  let ( <*> ) = ( <*> )

  let ( <$> ) = ( <$> )

  let ( >>= ) = ( >>= )

  let ( -- ) = ( -- )

  let ( --^ ) = ( --^ )
end

(** {2 IO} *)

let pp ?(start = "") ?(stop = "") ?(sep = ", ") pp_item fmt l =
  let rec print fmt l =
    match l with
    | x :: (_ :: _ as l) ->
        pp_item fmt x ;
        Format.pp_print_string fmt sep ;
        Format.pp_print_cut fmt () ;
        print fmt l
    | [x] ->
        pp_item fmt x
    | [] ->
        ()
  in
  Format.pp_print_string fmt start ;
  print fmt l ;
  Format.pp_print_string fmt stop

(*$= & ~printer:(fun s->s)
  "[1, 2, 3]" \
      (CCFormat.to_string \
        (CCFormat.hbox(CCList.pp ~start:"[" ~stop:"]" CCFormat.int)) \
        [1;2;3])
*)
