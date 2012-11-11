open Streams


let rec allZero () =
  cons 0 (allZero ())

let rec firstn s n =
  if n = 0 then
    []
  else
    match force s with
      None -> []
    | Some (x, s) -> x :: firstn s (n - 1)

let tail s =

let naturals =
let positives =

let rec map f s =
  delay (fun () ->
    match force s with
      None -> nil ()
    | Some (x, s) -> cons (f x) (map f s))

let allOne =

let rec map2 f s1 s2 =
  delay (fun () ->
    match force s1, force s2 with
      None, None -> nil ()
    | Some (x1, s1), Some (x2, s2) -> cons (f x1 x2) (map2 f s1 s2)
    | _, _ -> failwith "List length mismatch")

let positivesAgain =

let rec fact n =
  if n <= 1 then
    1
  else
    n * fact (n - 1)

let facts =

let fibs =

let rec filter f s =
  delay (fun () ->
    match force s with
      None -> nil ()
    | Some (x, s) -> if f x then cons x (filter f s) else filter f s)

let rec sieve ns =

let primes =
