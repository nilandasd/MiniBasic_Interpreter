(* $Id: interp.ml,v 1.17 2020-10-29 10:22:11-07 - - $ *)

open Absyn

let want_dump = ref false

let source_filename = ref ""

let curr_line_num = ref 0

let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> eval_memref memref
    | Unary (oper, expr) -> 
      (try let unary_op = Hashtbl.find Tables.unary_fn_table oper
           in unary_op (eval_expr expr)
       with Not_found -> 
           (Printf.printf "ERROR: Invalid unary operator\n
                           LINE: %d\n" !curr_line_num;
            exit 1))
    | Binary (oper, expr1, expr2) -> 
      (try let binary_op = Hashtbl.find Tables.binary_fn_table oper 
           in binary_op (eval_expr expr1) (eval_expr expr2)
       with Not_found ->
           (Printf.printf "ERROR: Invalid binary operator\n
                           LINE: %d\n" !curr_line_num;
            exit 1))

and eval_memref (memref : Absyn.memref) : float = match memref with
    | Arrayref (ident, expr) -> 
        (try let arr = Hashtbl.find Tables.array_table ident
             in Array.get arr (Float.to_int (eval_expr expr))
         with Not_found ->
            (Printf.printf "ERROR: Array not defined\n
                            LINE: %d\n" !curr_line_num;
             exit 1))
    | Variable ident -> try Hashtbl.find Tables.variable_table ident
                        with Not_found -> 0.0

and eval_STUB reason = (
    print_string ("(" ^ reason ^ ")");
    nan)

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::continue -> match firstline with
       | _, _, None -> interpret continue
       | linenr, _, Some stmt -> 
           (curr_line_num := linenr;
            interp_stmt stmt continue)

and interp_stmt (stmt : Absyn.stmt) (continue : Absyn.program) =
    match stmt with
    | Dim (ident, expr) -> interp_dim (ident, expr) continue
    | Let (memref, expr) -> interp_let (memref, expr) continue
    | Goto label -> interp_goto label continue
    | If (expr, label) -> interp_if (expr, label) continue
    | Print print_list -> interp_print print_list continue
    | Input memref_list -> interp_input memref_list continue

and interp_dim (ident, expr) (continue : Absyn.program) =
    Hashtbl.replace Tables.array_table ident
                    (Array.make (Float.to_int (eval_expr expr)) 0.0);
    interpret continue

and interp_let (memref, expr) (continue : Absyn.program) =
   (match memref with
    | Arrayref (ident, index) ->
       (try let arr = Hashtbl.find Tables.array_table ident
            in Array.set arr (Float.to_int (eval_expr index))
                                           (eval_expr expr)
        with
           | Not_found ->
              (Printf.printf "ERROR: Array not defined\n
                              LINE: %d\n" !curr_line_num;
               exit 1)
           | Invalid_argument (string) ->
              (print_string string;
               print_newline();
               Printf.printf "LINE: %d\n" !curr_line_num;
               exit 1))
    | Variable ident ->
         Hashtbl.replace Tables.variable_table ident (eval_expr expr));
    interpret continue

and interp_goto label (continue : Absyn.program) =
   (try let new_continuation = Hashtbl.find Tables.label_table label
        in interpret new_continuation
    with Not_found -> 
       (Printf.printf "ERROR: Label not defined\n
                       LINE: %d\n" !curr_line_num;
        exit 1))

and interp_if (relexpr, label) (continue : Absyn.program) =
    match relexpr with 
    | Relexpr (oper, expr1, expr2) ->
       (try let relop = Hashtbl.find Tables.bool_fn_table oper
            in if relop (eval_expr expr1) (eval_expr expr2)
               then interp_goto label continue
               else interpret continue
        with Not_found ->
           (Printf.printf "ERROR: Array undefined\n
                           LINE: %d\n" !curr_line_num;
            exit 1))

and interp_print (print_list : Absyn.printable list)
                 (continue : Absyn.program) =
    let print_item item =
        match item with
        | String string ->
          let regex = Str.regexp "\"\\(.*\\)\""
          in print_string (Str.replace_first regex "\\1" string)
        | Printexpr expr ->
          print_string " "; print_float (eval_expr expr)
    in (List.iter print_item print_list; print_newline ());
    interpret continue

and interp_input (memref_list : Absyn.memref list)
                 (continue : Absyn.program)  =
    let input_number memref =
        try  let number = Etc.read_number ()
             in match memref with
                | Arrayref (ident, index) ->
                   (try let arr = 
                     Hashtbl.find Tables.array_table ident
                        in Array.set arr
                    (Float.to_int (eval_expr index)) number
                    with 
                    | Not_found ->
                       (Printf.printf "ERROR: Array undefined\n
                                       LINE: %d\n" !curr_line_num)
                    | Invalid_argument (string) ->
                       (print_string string;
                        print_newline();
                        Printf.printf "LINE: %d\n"
                                      !curr_line_num))
                | Variable ident ->
                   (try Hashtbl.replace Tables.variable_table 
                        ident number
                    with Not_found ->
                       (Printf.printf "ERROR: Varaible undefined\n
                                       LINE: %d\n" !curr_line_num))
        with End_of_file -> 
             (Hashtbl.replace Tables.variable_table "eof" 1.;
              print_string "End_of_file";
              print_newline ())
    in List.iter input_number memref_list;
    interpret continue

and interp_STUB reason continue = (
    print_string "Unimplemented: ";
    print_string reason;
    print_newline();
    interpret continue)

let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program)

