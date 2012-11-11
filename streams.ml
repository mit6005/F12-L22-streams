type 'a prestream = Nil | Cons of 'a * (unit -> 'a prestream)

type 'a stream = unit -> 'a prestream

let nil () = fun () -> Nil

let cons x s = fun () -> Cons (x, s)

let delay f =
  let r = ref None in
  fun () ->
    match !r with
      Some v -> v
    | None ->
        let x = f () () in
        r := Some x;
        x

let force f =
  match f () with
    Nil -> None
  | Cons (x, s) -> Some (x, s)

let recursive f =
  let r = ref None in
  let rec recursive' () =
    match !r with
      Some s -> s
    | None ->
        let x = f (delay recursive') in
        r := Some x;
        x
  in delay recursive'
