type 'a stream
(* Abstract data type for possibly infinite sequences of values *)

val nil : unit -> 'a stream
(* Create a length-0 stream. *)

val cons : 'a -> 'a stream -> 'a stream
(* Create stream by prepending an element to another stream. *)

val delay : (unit -> 'a stream) -> 'a stream
(* Create a stream that, when examined, determines its value by running a function. *)

val force : 'a stream -> ('a * 'a stream) option
(* Examine a stream to determine the first value it contains, if any. *)

val recursive : ('a stream -> 'a stream) -> 'a stream
(* Define a stream by a function that is passed the stream itself as argument. *)
