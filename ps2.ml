open Streams
open More


(* Details redacted to avoid giving away PS2 solution for future 005 classes. *)


(** * Part 1: lexer *)

let rec lex (chars : char stream) : token stream = ...

(** * Part 2: parser *)

let rec parse (ts : token stream) : exp stream = ...

(** * Part 3: "visitors" *)

let rec to_string (e : exp) : string = ...
let rec differentiate (x : string) (e : exp) : exp = ...


(** * Part 4, putting it all together: main function of program, differentiating many expressions w.r.t. "x" *)

let main () =
  iter print_endline (map to_string
                        (map (differentiate "x") (parse (lex keyboard))))

let () = main ()
