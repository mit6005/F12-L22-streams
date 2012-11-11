(** * Lists and options *)

let my_list = 1 :: 2 :: 3 :: []

let rec map f ls =
  match ls with
    [] -> []
  | x :: ls -> f x :: map f ls

let my_list' = map (fun x -> x + 1) my_list
let my_list' = map ((+) 1) my_list

let rec nth ls n =
  match ls with
    [] -> None
  | x :: ls ->
      if n = 0 then
        Some x
      else
        nth ls (n-1)

let nth_default ls n default =
  match nth ls n with
    None -> default
  | Some v -> v


(** * Recursive datatypes *)

type formula =
    Var of string
  | Not of formula
  | And of formula * formula
  | Or of formula * formula

let rec eval env f =
  match f with
    Var x -> env x
  | Not f1 -> not (eval env f1)
  | And (f1, f2) -> eval env f1 && eval env f2
  | Or (f1, f2) -> eval env f1 || eval env f2

let example = And (Var "P", Or (Var "Q", Not (Var "R")))

let shouldBeTrue = eval (function "P" -> true | _ -> false) example
let shouldBeFalse = eval (function "R" -> true | _ -> false) example
