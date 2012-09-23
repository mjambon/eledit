(***********************************************************************)
(*                                                                     *)
(*                               Ledit                                 *)
(*                                                                     *)
(*                Daniel de Rauglaudre, INRIA Rocquencourt             *)
(*                                                                     *)
(*  Copyright 2001-2010 Institut National de Recherche en Informatique *)
(*  et Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

open Ledit
open Sys

let version = "2.03"

let usage () =
  prerr_string "Usage: ";
  prerr_string argv.(0);
  prerr_endline " [options] [comm [args]]";
  prerr_endline " -a : ascii encoding";
  prerr_endline " -h file : history file";
  prerr_endline " -x  : don't remove old contents of history";
  prerr_endline " -l len : line max length";
  prerr_endline " -t  : trace sequences (for debugging)";
  prerr_endline " -u : utf-8 encoding";
  prerr_endline " -v : prints ledit version and exit";
  prerr_endline "Exec comm [args] as child process"

let get_arg i =
  if i >= Array.length argv then begin usage (); exit 1 end else argv.(i)

let histfile = ref ""
let trunc = ref true
let comm = ref "cat"
let args = ref [| "cat" |]

let _ =
  let rec arg_loop i =
    if i < Array.length argv then
      arg_loop
        (match argv.(i) with
           "-a" -> set_ascii (); i + 1
         | "-h" -> histfile := get_arg (i + 1); i + 2
         | "-help" -> usage (); exit 0
         | "-l" ->
             let x = get_arg (i + 1) in
             (try set_max_len (int_of_string x) with _ -> usage (); exit 1);
             i + 2
         | "-x" -> trunc := false; i + 1
         | "-t" -> trace_sequences := true; i + 1
         | "-u" -> set_utf8 (); unset_meta_as_escape (); i + 1
         | "-v" ->
             Printf.printf "Ledit version %s\n" version; flush stdout; exit 0
         | _ ->
             if i < Array.length argv then
               if argv.(i).[0] = '-' then
                 begin
                   prerr_endline ("Illegal option " ^ argv.(i));
                   prerr_endline "Use option -help for usage";
                   exit 1
                 end
               else
                 begin
                   comm := argv.(i);
                   args := Array.sub argv i (Array.length argv - i);
                   Array.length argv
                 end
             else Array.length argv)
  in
  arg_loop 1

let string_of_signal =
  function
    2 -> "Interrupted"
  | 3 -> "Quit"
  | 10 -> "Bus error"
  | 11 -> "Segmentation fault"
  | x -> "Signal " ^ string_of_int x

let rec read_loop () =
  begin try
    let c = input_char stdin in
    if c = "\n" then print_newline () else print_string c
  with Break -> ()
  end;
  read_loop ()

let stupid_hack_to_avoid_sys_error_at_exit () =
  Unix.dup2 (Unix.openfile "/dev/null" [Unix.O_WRONLY] 0) Unix.stdout

let go () =
  let (id, od) = Unix.pipe () in
  let pid = Unix.fork () in
  if pid < 0 then failwith "fork"
  else if pid > 0 then
    begin
      Unix.dup2 od Unix.stdout;
      Unix.close id;
      Unix.close od;
      set_son_pid pid;
      let _ =
        (signal sigchld
           (Signal_handle
              (fun _ ->
                 match snd (Unix.waitpid [Unix.WNOHANG] pid) with
                   Unix.WSIGNALED sign ->
                     prerr_endline (string_of_signal sign);
                     flush stderr;
                     raise End_of_file
                 | _ -> raise End_of_file)) :
         signal_behavior)
      in
      try
        if !histfile <> "" then open_histfile !trunc !histfile;
        catch_break true;
        read_loop ()
      with x ->
        let _ = (signal sigchld Signal_ignore : signal_behavior) in
        begin try Unix.close Unix.stdout; let _ = Unix.wait () in () with
          Unix.Unix_error (_, _, _) -> ()
        end;
        stupid_hack_to_avoid_sys_error_at_exit ();
        match x with
          End_of_file -> ()
        | _ -> prerr_string "(ledit) "; flush stderr; raise x
    end
  else
    begin
      Unix.dup2 id Unix.stdin;
      Unix.close id;
      Unix.close od;
      Unix.execvp !comm !args
    end

let handle f a =
  try f a with
    Unix.Unix_error (code, fname, param) ->
      Printf.eprintf "Unix error: %s\nOn function %s %s\n"
        (Unix.error_message code) fname param;
      flush stderr;
      exit 2
  | e -> Printexc.catch raise e

let _ = handle go ()
