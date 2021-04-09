(* $Id: main.ml,v 1.6 2020-10-29 10:21:37-07 - - $ *)

(*
* Main program reads a file and prints to stdout.
*)

let interpret_source filename =
    try (Interp.source_filename := filename;
         let sourcefile =
             if filename = "-" then stdin
                               else open_in filename
         in let lexbuf = Lexing.from_channel sourcefile
         in let abstract_syntax = Parser.program Scanner.token lexbuf
         in Interp.interpret_program abstract_syntax)
    with Sys_error (string) -> Etc.die [string]

let _ = if !Sys.interactive
        then ()
        else match Array.length Sys.argv with
             | 1 -> interpret_source "-"
             | 2 -> if Sys.argv.(1) = "-d"
                    then (Interp.want_dump := true;
                          interpret_source "-")
                    else interpret_source Sys.argv.(1)
             | 3 -> if Sys.argv.(1) = "-d"
                    then (Interp.want_dump := true;
                          interpret_source Sys.argv.(2))
                    else Etc.usage_exit ()
             | _ -> Etc.usage_exit ()

