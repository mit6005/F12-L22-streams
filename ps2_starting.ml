open Streams
open More


(** * Part 1: lexer *)

type token = PLUS | TIMES | NUM of int | VAR of string | LPAREN | RPAREN

let isspace (ch : char) = ch = ' ' || ch = '\n'
let isdigit (ch : char) = '0' <= ch && ch <= '9'
let isletter (ch : char) = ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z')

let rec lex (chars : char stream) : token stream =
  let rec lex' (chars : char stream) : token stream =
    match force chars with
      None -> nil ()
    | Some (ch, chars') ->
        let answerIs v = cons v (lex chars') in
        
        match ch with
          '+' -> answerIs PLUS
        | '*' -> answerIs TIMES
        | '(' -> answerIs LPAREN
        | ')' -> answerIs RPAREN
        | _ ->
            if isspace ch then
              lex' chars'
            else match prefix isdigit chars with
              ([], _) -> begin
                match prefix isletter chars with
                | ([], _) -> failwith "Invalid character to lex"
                | (ls, chars'') -> cons (VAR (implode ls)) (lex chars'')
              end
            | (ls, chars'') -> cons (NUM (int_of_string (implode ls))) (lex chars'')

  in delay (fun () -> lex' chars)


(** * Part 2: parser *)

type exp = Num of int | Var of string | Plus of exp * exp | Times of exp * exp

let rec parse (ts : token stream) : exp stream =
  let rec parse' (ts : token stream) : (exp * token stream) option =
    match force ts with
      None -> None
    | Some (t, ts) ->
        match t with
          NUM n -> Some (Num n, ts)
        | VAR s -> Some (Var s, ts)
        | LPAREN -> begin
            match parse' ts with
              None -> failwith "Nothing after LPAREN"
            | Some (e1, ts) ->
                match force ts with
                  None -> failwith "Missing operator"
                | Some (op, ts) ->
                    let oper = match op with
                      PLUS -> Some (fun x y -> Plus (x, y))
                    | TIMES -> Some (fun x y -> Times (x, y))
                    | RPAREN -> None
                    | _ -> failwith "Bad character during parsing"
                    in

                    match oper with
                      None -> Some (e1, ts)
                    | Some oper ->
                        match parse' ts with
                          None -> failwith "Missing second operand"
                        | Some (e2, ts) ->
                            match force ts with
                              Some (RPAREN, ts) -> Some (oper e1 e2, ts)
                            | _ -> failwith "Missing RPAREN"
        end
        | _ -> failwith "Bad token"
  in

  delay (fun () ->
    match parse' ts with
      None -> nil ()
    | Some (e, ts) -> cons e (parse ts))


(** * Part 3: "visitors" *)

let rec to_string (e : exp) : string =
  match e with
    Num n -> string_of_int n
  | Var s -> s
  | Plus (e1, e2) -> "(" ^ to_string e1 ^ "+" ^ to_string e2 ^ ")"
  | Times (e1, e2) -> "(" ^ to_string e1 ^ "*" ^ to_string e2 ^ ")"

let rec differentiate (x : string) (e : exp) : exp =
  match e with
    Num _ -> Num 0
  | Var s -> Num (if s = x then 1 else 0)
  | Plus (e1, e2) -> Plus (differentiate x e1, differentiate x e2)
  | Times (e1, e2) -> Plus (Times (differentiate x e1, e2), Times (e1, differentiate x e2))


(** * Part 4, putting it all together: main function of program, differentiating many expressions w.r.t. "x" *)

let main () = ()

let () = main ()
