open Streams


(* Danger: Infinite loop here! *)
let rec allZero () =
  cons 0 (allZero ())

let rec firstn s n =
  if n = 0 then
    []
  else
    match force s with
      None -> []
    | Some (x, s) -> x :: firstn s (n - 1)

let allZero =
  let rec allZero' () =
    cons 0 (delay allZero')
  in delay allZero'

let _ = firstn allZero 5

let my_stream = cons 1 (cons 2 (cons 3 (nil ())))
let _ = firstn my_stream 0
let _ = firstn my_stream 1
let _ = firstn my_stream 2
let _ = firstn my_stream 3
let _ = firstn my_stream 4

let tail s =
  delay (fun () ->
    match force s with
      None -> failwith "tail: Stream ends!"
    | Some (_, s) -> s)

let rec intsStartingAt n =
  delay (fun () -> cons n (intsStartingAt (n + 1)))

let naturals = intsStartingAt 0
let positives = tail naturals

let _ = firstn naturals 10
let _ = firstn positives 10

let rec map f s =
  delay (fun () ->
    match force s with
      None -> nil ()
    | Some (x, s) -> cons (f x) (map f s))

let allOne = map (fun n -> n + 1) allZero

let _ = firstn allOne 5

let rec map2 f s1 s2 =
  delay (fun () ->
    match force s1, force s2 with
      None, None -> nil ()
    | Some (x1, s1), Some (x2, s2) -> cons (f x1 x2) (map2 f s1 s2)
    | _, _ -> failwith "List length mismatch")

let positivesAgain = map2 (+) naturals allOne

let _ = firstn positivesAgain 10

let rec fact n =
  if n <= 1 then
    1
  else
    n * fact (n - 1)

let facts = map fact naturals

let _ = firstn facts 10000

let allZero = recursive (fun allZero -> cons 0 allZero)

let _ = firstn allZero 5

let facts =
  recursive (fun facts -> cons 1 (map2 ( * ) positives facts))

let _ = firstn facts 10000

let fibs = recursive (fun fibs -> cons 0 (cons 1 (map2 (+) fibs (tail fibs))))

let rec filter f s =
  delay (fun () ->
    match force s with
      None -> nil ()
    | Some (x, s) -> if f x then cons x (filter f s) else filter f s)

let rec sieve ns =
  delay (fun () ->
    match force ns with
      None -> failwith "Stream ended!"
    | Some (p, xs) -> cons p (sieve (filter (fun n -> n mod p > 0) xs)))

let primes =
  sieve (intsStartingAt 2)

let _ = firstn primes 100
