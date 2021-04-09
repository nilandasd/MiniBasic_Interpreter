(* $Id: etc.mli,v 1.5 2020-10-22 12:33:57-07 - - $ *)

(*
* Main program and system access.
*)

val warn : string list -> unit

val die : string list -> unit

val syntax_error : Lexing.position -> string list -> unit

val usage_exit : unit -> unit

val read_number : unit -> float

val int_of_round_float : float -> int

