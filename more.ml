open Streams


(** * Functional programming with streams *)

let rec map (f : 'a -> 'b) (s : 'a stream) : 'b stream =
  delay (fun () ->
    match force s with
      None -> nil ()
    | Some (x, s) -> cons (f x) (map f s))

let rec iter (f : 'a -> unit) (s : 'a stream) : unit =
  match force s with
    None -> ()
  | Some (x, s) -> f x; iter f s


(** * Producing streams from various data sources *)

let keyboard : char stream =
  let rec loop () =
    try
      cons (input_char stdin) (delay loop)
    with End_of_file -> nil ()
  in delay loop

let of_file (filename : string) : char stream =
  let handle = open_in filename in
  let rec loop () =
    try
      cons (input_char handle) (delay loop)
    with End_of_file -> close_in handle; nil ()
  in delay loop

let of_string (s : string) : char stream =
  let rec loop i () =
    if i < String.length s then
      cons s.[i] (delay (loop (i+1)))
    else
      nil ()
  in delay (loop 0)


(** * Converting a finite stream into a list *)

let to_list (s : 'a stream) : 'a list =
  let rec loop (s : 'a stream) (acc : 'a list) =
    match force s with
      None -> List.rev acc
    | Some (x, s') -> loop s' (x :: acc)
  in loop s []


(** * Pulling off the longest prefix of a stream matching a predicate *)

let prefix (test : 'a -> bool) (s : 'a stream) : 'a list * 'a stream =
  let rec loop (s : 'a stream) (acc : 'a list) =
    match force s with
      None -> (List.rev acc, s)
    | Some (x, s') ->
        if test x then
          loop s' (x :: acc)
        else
          (List.rev acc, s)
  in loop s []


(** * An extra useful string conversion function *)

let implode (l : char list) : string =
  let res = String.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l
