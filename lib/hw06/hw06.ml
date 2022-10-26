(** Problem 1 *)

(* unzip: ('a * 'b) list -> 'a list * 'b list *)
let unzip xys = 
  List.fold_right (fun (x, y) (xs, ys) -> (x :: xs, y :: ys)) xys ([], [])

(* fold_right: ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
let fold_right op xs z =
  List.fold_left (fun xs x -> x :: xs) [] xs |>
  List.fold_left (fun acc x -> op x acc) z

(* in_relation: ('a -> bool) -> 'a list -> bool *)
let in_relation p xs = 
  match xs with
  | x :: xs1 ->
      let _, res = List.fold_left (fun (x, acc) y -> (y, acc && p x y)) (x, true) xs1 in
      res
  | [] -> true

    
(** Problem 2 *)

(** An ADT for nested lists *)
type 'a nlist =
  | NList of ('a nlist) list
  | Atom of 'a

(* Pretty printing functions for nested lists *)
let rec pr_nlist_list pe ppf = function
  | [] -> ()
  | [xs] -> pr_nlist pe ppf xs
  | xs :: xss ->
      Format.fprintf ppf "%a;@ %a" (pr_nlist pe) xs (pr_nlist_list pe) xss 
  
and pr_nlist pe ppf = function
  | Atom x -> Format.fprintf ppf "Atom@ @[%a@]" pe x
  | NList xss -> Format.fprintf ppf "NList@[<2>@ [@[%a@]]@]" (pr_nlist_list pe) xss

(* Pretty printer for values xxs of type int nlist *)
let string_of_int_nlist xss =
  pr_nlist (fun ppf -> Format.fprintf ppf "%d") Format.str_formatter xss;
  Format.flush_str_formatter ()
        
(* to_list: 'a nlist -> 'a list *)
let to_list xss =
  let rec to_list_helper acc = function
    | NList xs :: stack -> to_list_helper acc (xs @ stack) (* Can also use: List.rev_append (List.rev xs) stack *)
    | Atom x :: stack -> to_list_helper (x :: acc) stack
    | [] -> List.rev acc
  in
  to_list_helper [] [xss]

(** An ADT for binary search trees *)
type tree =
  | Leaf
  | Node of int * tree * tree

(* Pretty printing functions for trees *)
let rec pr_tree ppf = function
  | Leaf -> Format.fprintf ppf "Leaf"
  | Node (x, left, right) ->
      Format.fprintf ppf "@[<2>Node@ (%d,@ %a,@ %a)@]" x pr_tree left pr_tree right

let string_of_tree t =
  pr_tree Format.str_formatter t;
  Format.flush_str_formatter ()

        
(* fold: ('a -> int -> 'a) -> 'a -> tree -> 'a *) 
let rec fold op z t =
  match t with
  | Leaf -> z
  | Node (x, left, right) ->
      let lres = fold op z left in
      fold op (op lres x) right

(* list_of_tree: tree -> int list *)
let list_of_tree t =
  fold (fun xs x -> x :: xs) [] t |> List.rev

(* is_sorted: tree -> bool *)
let is_sorted t =
  let res, _ =
    fold (fun (acc, prev) x ->
      (acc && prev < x, x)) (true, min_int) t
  in
  res

