(* $Id: interp.mli,v 1.9 2020-10-29 10:22:11-07 - - $ *)

(*
* Interpreter for Mini Basic
*)

val want_dump : bool ref

val source_filename : string ref

val interpret_program : Absyn.program -> unit

