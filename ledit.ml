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

(* $Id: ledit.ml,v 1.68 2012-02-08 09:03:40 deraugla Exp $ *)

(* #load "pa_local.cmo" *)
(* #load "pa_def.cmo" *)
(* #load "pa_fstream.cmo" *)

open Printf

type encoding = Ascii | Iso_8859 | Utf_8

module A :
  sig
    val encoding : encoding ref
    module Char :
      sig
        type t
        val of_ascii : char -> t
        val to_ascii : t -> char option
        val is_word_char : t -> bool
        val ctrl_val : t -> t option
        val meta_ctrl_val : t -> t option
        val not_ascii_val : t -> (t * t * t) option
        val uppercase : t -> t
        val lowercase : t -> t
        val parse : char Stream.t -> t
        val to_string : t -> string
        val input : in_channel -> t
        val read : unit -> t
        val print : t -> unit
        val prerr : t -> unit
        val prerr_backsp : t -> unit
      end
    module String :
      sig
        type t
        val empty : t
        val of_char : Char.t -> t
        val of_ascii : string -> t
        val length : t -> int
        val set : t -> int -> Char.t -> unit
        val get : t -> int -> Char.t
        val sub : t -> int -> int -> t
        val concat : t -> t -> t
        val input_line : in_channel -> t
        val output : out_channel -> t -> unit
      end
  end =
  struct
    let encoding = ref Iso_8859
    let char_code = Char.code
    let nbc c =
      if Char.code c < 0b10000000 then 1
      else if Char.code c < 0b11000000 then -1
      else if Char.code c < 0b11100000 then 2
      else if Char.code c < 0b11110000 then 3
      else if Char.code c < 0b11111000 then 4
      else if Char.code c < 0b11111100 then 5
      else if Char.code c < 0b11111110 then 6
      else -1
    module Char =
      struct
        type t = string
        let of_ascii c = String.make 1 c
        let to_ascii c =
          if String.length c = 1 && Char.code c.[0] < 128 then Some c.[0]
          else None
        let is_word_char c =
          if String.length c = 1 then
            match c.[0] with
              'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true
            | x ->
                if Char.code x < 128 then false
                else
                  match !encoding with
                    Ascii -> false
                  | Iso_8859 -> Char.code x >= 160
                  | Utf_8 -> assert false
          else true
        let ctrl_val c =
          if String.length c = 1 then
            let c = c.[0] in
            if Char.code c < 32 || Char.code c == 127 then
              Some (String.make 1 (Char.chr (127 land (Char.code c + 64))))
            else None
          else None
        let meta_ctrl_val c =
          if String.length c = 1 then
            let c = c.[0] in
            if Char.code c >= 128 && Char.code c < 160 then
              Some (String.make 1 (Char.chr (127 land (Char.code c + 64))))
            else None
          else None
        let not_ascii_val c =
          if String.length c = 1 then
            let c = c.[0] in
            if Char.code c >= 128 then
              let c1 = Char.chr (Char.code c / 100 + Char.code '0') in
              let c2 = Char.chr (Char.code c mod 100 / 10 + Char.code '0') in
              let c3 = Char.chr (Char.code c mod 10 + Char.code '0') in
              Some (String.make 1 c1, String.make 1 c2, String.make 1 c3)
            else None
          else None
        let uppercase c =
          match !encoding with
            Ascii | Iso_8859 -> String.uppercase c
          | Utf_8 ->
              if String.length c = 1 then
                if Char.code c.[0] < 128 then String.uppercase c else c
              else c
        let lowercase c =
          match !encoding with
            Ascii | Iso_8859 -> String.lowercase c
          | Utf_8 ->
              if String.length c = 1 then
                if Char.code c.[0] < 128 then String.lowercase c else c
              else c
        let to_string s = s
        let get_char f =
          match !encoding with
            Ascii | Iso_8859 -> String.make 1 (f ())
          | Utf_8 ->
              let c = f () in
              let nbc = nbc c in
              if nbc < 0 then "?"
              else if nbc = 1 then String.make 1 c
              else
                let rec loop s n =
                  if n = 0 then s
                  else
                    let c = f () in
                    if Char.code c < 0b10000000 then "?"
                    else if Char.code c > 0b10111111 then "?"
                    else loop (s ^ String.make 1 c) (n - 1)
                in
                loop (String.make 1 c) (nbc - 1)
        let input ic = get_char (fun () -> input_char ic)
        let read =
          let buff = " " in
          fun () ->
            get_char
              (fun () ->
                 let len = Unix.read Unix.stdin buff 0 1 in
                 if len == 0 then raise End_of_file else buff.[0])
        let parse s = get_char (fun () -> Stream.next s)
        let print c =
          if String.length c = 1 && c.[0] = '\n' then print_newline ()
          else print_string c
        let prerr c = output_string stderr c
        let prerr_backsp c =
          if !encoding = Utf_8 && Char.code c.[0] >= 228 &&
             Char.code c.[0] <= 233
          then
            output_char stderr '\b';
          output_char stderr '\b'
      end
    module String =
      struct
        type t = string array
        let empty = [| |]
        let of_char c = [| c |]
        let of_ascii s =
          Array.init (String.length s)
            (fun i ->
               if char_code s.[i] < 128 then String.make 1 s.[i]
               else invalid_arg "A.String.of_ascii")
        let length = Array.length
        let set = Array.set
        let get = Array.get
        let sub = Array.sub
        let concat = Array.append
        let input_line ic =
          let s = input_line ic in
          match !encoding with
            Ascii | Iso_8859 ->
              Array.init (String.length s) (fun i -> String.make 1 s.[i])
          | Utf_8 ->
              let rec loop list i =
                if i >= String.length s then Array.of_list (List.rev list)
                else
                  let n = nbc s.[i] in
                  if n < 0 then loop ("?" :: list) (i + 1)
                  else loop (String.sub s i n :: list) (i + n)
              in
              loop [] 0
        let output oc s = Array.iter (output_string oc) s
      end
  end

(* *)
(* *)
(* *)
(* *)

let max_len = ref 70
let set_max_len x = max_len := if x > 3 then x else failwith "set_max_len"

let son = ref None
let set_son_pid pid = son := Some pid

type command =
    Abort
  | Accept_line
  | Backward_char
  | Backward_delete_char
  | Backward_kill_word
  | Backward_word
  | Beginning_of_history
  | Beginning_of_line
  | Capitalize_word
  | Complete_file_name
  | Delete_char
  | Delete_char_or_end_of_file
  | Downcase_word
  | End_of_history
  | End_of_line
  | Expand_abbrev
  | Forward_char
  | Forward_word
  | Insert of string
  | Interrupt
  | Kill_line
  | Kill_word
  | Next_history
  | Operate_and_get_next
  | Previous_history
  | Quit
  | Quoted_insert
  | Redraw_current_line
  | Reverse_search_history
  | Self_insert
  | Sequence of string
  | Suspend
  | Transpose_chars
  | Unix_line_discard
  | Upcase_word
  | Yank

type istate =
    Normal of string
  | Quote

let meta_as_escape = ref true
let unset_meta_as_escape () = meta_as_escape := false

let set_utf8 () = A.encoding := Utf_8
let set_ascii () = A.encoding := Ascii

(* key binding tree *)

type kb_tree =
    KB_tree of kb_node list
  | KB_comm of command
  | KB_none
and kb_node = { char : char; son : kb_tree }

let hex_value c =
  match c with
    '0'..'9' -> Some (Char.code c - Char.code '0')
  | 'a'..'f' -> Some (Char.code c - Char.code 'a' + 10)
  | 'A'..'F' -> Some (Char.code c - Char.code 'A' + 10)
  | _ -> None

let oct_value c =
  match c with
    '0'..'7' -> Some (Char.code c - Char.code '0')
  | _ -> None

let rec next_char s i =
  if i < String.length s then
    let (c, i) =
      if s.[i] = '\\' then
        if i + 1 < String.length s then
          match s.[i+1] with
            'C' ->
              if i + 3 < String.length s && s.[i+2] = '-' then
                let c = s.[i+3] in
                if Char.code c >= 64 && Char.code c <= 95 then
                  Char.chr (Char.code c - 64), i + 4
                else if Char.code c >= 96 && Char.code c <= 127 then
                  Char.chr (Char.code c - 96), i + 4
                else s.[i], i + 1
              else s.[i], i + 1
          | 'M' ->
              if i + 2 < String.length s && s.[i+2] = '-' then
                match next_char s (i + 3) with
                  Some (c, j) ->
                    if Char.code c < 128 then Char.chr (Char.code c + 128), j
                    else s.[i], i + 1
                | None -> s.[i], i + 1
              else s.[i], i + 1
          | 'e' -> '\027', i + 2
          | '\\' -> '\\', i + 2
          | '"' -> '"', i + 2
          | '\'' -> '\'', i + 2
          | 'a' -> '\007', i + 2
          | 'b' -> '\b', i + 2
          | 'd' -> '\255', i + 2
          | 'f' -> '\012', i + 2
          | 'n' -> '\n', i + 2
          | 'r' -> '\r', i + 2
          | 't' -> '\009', i + 2
          | 'v' -> '\011', i + 2
          | 'x' ->
              if i + 2 < String.length s then
                match hex_value s.[i+2] with
                  Some v ->
                    if i + 3 < String.length s then
                      match hex_value s.[i+3] with
                        Some v1 -> Char.chr (16 * v + v1), i + 4
                      | None -> Char.chr v, i + 3
                    else Char.chr v, i + 3
                | None -> s.[i], i + 1
              else s.[i], i + 1
          | c ->
              match oct_value s.[i+1] with
                Some v ->
                  if i + 2 < String.length s then
                    match oct_value s.[i+2] with
                      Some v1 ->
                        let v = 8 * v + v1 in
                        if i + 3 < String.length s then
                          match oct_value s.[i+3] with
                            Some v1 ->
                              let v1 = 8 * v + v1 in
                              if v1 <= 255 then Char.chr v1, i + 4
                              else Char.chr v, i + 3
                          | None -> Char.chr v, i + 3
                        else Char.chr v, i + 3
                    | None -> Char.chr v, i + 2
                  else Char.chr v, i + 2
              | None -> s.[i], i + 1
        else s.[i], i + 1
      else s.[i], i + 1
    in
    Some (c, i)
  else None

let insert_command s comm kb =
  let rec insert_in_tree i kb =
    match next_char s i with
      Some (c, i) ->
        let cnl =
          match kb with
            KB_tree cnl -> cnl
          | KB_comm _ | KB_none -> []
        in
        KB_tree (insert_in_node_list (c, i) cnl)
    | None -> KB_comm comm
  and insert_in_node_list (c, i) =
    function
      [] -> [{char = c; son = insert_in_tree i (KB_tree [])}]
    | n :: nl ->
        if c < n.char then
          {char = c; son = insert_in_tree i (KB_tree [])} :: n :: nl
        else if c > n.char then n :: insert_in_node_list (c, i) nl
        else {char = n.char; son = insert_in_tree i n.son} :: nl
  in
  insert_in_tree 0 kb

let init_default_commands kb =
  List.fold_left (fun kb (key, bind) -> insert_command key bind kb) kb
    (("\\C-a", Beginning_of_line) :: ("\\C-e", End_of_line) ::
     ("\\C-f", Forward_char) :: ("\\C-b", Backward_char) ::
     ("\\C-p", Previous_history) :: ("\\C-n", Next_history) ::
     ("\\C-r", Reverse_search_history) ::
     ("\\C-d", Delete_char_or_end_of_file) ::
     ("\\C-h", Backward_delete_char) :: ("\\177", Backward_delete_char) ::
     ("\\C-i", Complete_file_name) :: ("\\C-t", Transpose_chars) ::
     ("\\C-q", Quoted_insert) :: ("\\C-k", Kill_line) :: ("\\C-y", Yank) ::
     ("\\C-u", Unix_line_discard) :: ("\\C-l", Redraw_current_line) ::
     ("\\C-g", Abort) :: ("\\C-c", Interrupt) :: ("\\C-z", Suspend) ::
     ("\\C-\\", Quit) :: ("\\n", Accept_line) ::
     ("\\C-x", Operate_and_get_next) :: ("\\ef", Forward_word) ::
     ("\\eb", Backward_word) :: ("\\ec", Capitalize_word) ::
     ("\\eu", Upcase_word) :: ("\\el", Downcase_word) ::
     ("\\e<", Beginning_of_history) :: ("\\e>", End_of_history) ::
     ("\\ed", Kill_word) :: ("\\e\\C-h", Backward_kill_word) ::
     ("\\e\\177", Backward_kill_word) :: ("\\e/", Expand_abbrev) ::
     ("\\e[A", Previous_history) :: ("\\e[B", Next_history) ::
     ("\\e[C", Forward_char) :: ("\\e[D", Backward_char) ::
     ("\\e[3~", Delete_char) :: ("\\e[H", Beginning_of_line) ::
     ("\\e[F", End_of_line) :: ("\\e[5~", Previous_history) ::
     ("\\e[6~", Next_history) :: ("\\e[2H", Beginning_of_history) ::
     ("\\e[2F", End_of_history) :: ("\\e[OA", Previous_history) ::
     ("\\e[OC", Forward_char) :: ("\\e[OD", Backward_char) ::
     ("\\e[OH", Beginning_of_line) ::
     (if !meta_as_escape then
        ["\\M-b", Backward_word; "\\M-c", Capitalize_word; "\\M-d", Kill_word;
         "\\M-f", Forward_word; "\\M-l", Downcase_word; "\\M-u", Upcase_word;
         "\\M-<", Beginning_of_history; "\\M->", End_of_history;
         "\\M-/", Expand_abbrev; "\\M-\\C-h", Backward_kill_word;
         "\\M-\\127", Backward_kill_word]
      else []))

(* Reading the leditrc file *)

let rev_implode l =
  let s = String.create (List.length l) in
  let rec loop i =
    function
      c :: l -> String.unsafe_set s i c; loop (i - 1) l
    | [] -> s
  in
  loop (String.length s - 1) l

let rec parse_string rev_cl (strm__ : _ Fstream.t) =
  match
    match Fstream.next strm__ with
      Some ('"', strm__) -> Some (rev_implode rev_cl, strm__)
    | _ -> None
  with
    Some _ as x -> x
  | None ->
      match
        match Fstream.next strm__ with
          Some ('\\', strm__) ->
            begin match Fstream.next strm__ with
              Some (c, strm__) -> parse_string (c :: '\\' :: rev_cl) strm__
            | _ -> None
            end
        | _ -> None
      with
        Some _ as x -> x
      | None ->
          match Fstream.next strm__ with
            Some (c, strm__) -> parse_string (c :: rev_cl) strm__
          | _ -> None

let rec skip_to_eos (strm__ : _ Fstream.t) =
  match
    match Fstream.next strm__ with
      Some (_, strm__) -> skip_to_eos strm__
    | _ -> None
  with
    Some _ as x -> x
  | None -> Some ((), strm__)

let rec skip_spaces (strm__ : _ Fstream.t) =
  match
    match Fstream.next strm__ with
      Some ((' ' | '\t'), strm__) -> skip_spaces strm__
    | _ -> None
  with
    Some _ as x -> x
  | None ->
      match
        match Fstream.next strm__ with
          Some ('#', strm__) -> skip_to_eos strm__
        | _ -> None
      with
        Some _ as x -> x
      | None -> Some ((), strm__)

let rec parse_command rev_cl (strm__ : _ Fstream.t) =
  match
    match Fstream.next strm__ with
      Some (('a'..'z' | 'A'..'Z' | '-' as c), strm__) ->
        parse_command (c :: rev_cl) strm__
    | _ -> None
  with
    Some _ as x -> x
  | None -> Some (rev_implode rev_cl, strm__)

type binding =
    B_string of string
  | B_comm of string

let parse_binding (strm__ : _ Fstream.t) =
  match
    match Fstream.next strm__ with
      Some ('"', strm__) ->
        begin match parse_string [] strm__ with
          Some (s, strm__) -> Some (B_string s, strm__)
        | _ -> None
        end
    | _ -> None
  with
    Some _ as x -> x
  | None ->
      match parse_command [] strm__ with
        Some (c, strm__) -> Some (B_comm c, strm__)
      | _ -> None

let parse_line (strm__ : _ Fstream.t) =
  match Fstream.next strm__ with
    Some ('"', strm__) ->
      begin match parse_string [] strm__ with
        Some (key, strm__) ->
          begin match skip_spaces strm__ with
            Some (_, strm__) ->
              begin match Fstream.next strm__ with
                Some (':', strm__) ->
                  begin match skip_spaces strm__ with
                    Some (_, strm__) ->
                      begin match parse_binding strm__ with
                        Some (binding, strm__) ->
                          begin match skip_spaces strm__ with
                            Some (_, strm__) ->
                              begin match Fstream.empty strm__ with
                                Some (_, strm__) ->
                                  Some ((key, binding), strm__)
                              | _ -> None
                              end
                          | _ -> None
                          end
                      | _ -> None
                      end
                  | _ -> None
                  end
              | _ -> None
              end
          | _ -> None
          end
      | _ -> None
      end
  | _ -> None

let command_of_name =
  let ht = Hashtbl.create 1 in
  let add = Hashtbl.add ht in
  add "abort" Abort;
  add "accept-line" Accept_line;
  add "backward-char" Backward_char;
  add "backward-delete-char" Backward_delete_char;
  add "backward-kill-word" Backward_kill_word;
  add "backward-word" Backward_word;
  add "beginning-of-history" Beginning_of_history;
  add "beginning-of-line" Beginning_of_line;
  add "capitalize-word" Capitalize_word;
  add "delete-char" Delete_char;
  add "delete-char-or-end-of-file" Delete_char_or_end_of_file;
  add "downcase-word" Downcase_word;
  add "end-of-history" End_of_history;
  add "end-of-line" End_of_line;
  add "expand-abbrev" Expand_abbrev;
  add "complete-file-name" Complete_file_name;
  add "forward-char" Forward_char;
  add "forward-word" Forward_word;
  add "interrupt" Interrupt;
  add "kill-line" Kill_line;
  add "kill-word" Kill_word;
  add "next-history" Next_history;
  add "operate-and-get-next" Operate_and_get_next;
  add "previous-history" Previous_history;
  add "quit" Quit;
  add "quoted-insert" Quoted_insert;
  add "redraw-current-line" Redraw_current_line;
  add "reverse-search-history" Reverse_search_history;
  add "self-insert" Self_insert;
  add "suspend" Suspend;
  add "transpose-chars" Transpose_chars;
  add "unix-line-discard" Unix_line_discard;
  add "upcase-word" Upcase_word;
  add "yank" Yank;
  fun name -> try Some (Hashtbl.find ht name) with Not_found -> None

let init_file_commands kb fname =
  let ic = open_in fname in
  let rec loop kb =
    match try Some (input_line ic) with End_of_file -> None with
      Some s ->
        let kb =
          match parse_line (Fstream.of_string s) with
            Some ((key, B_string s), _) ->
              let s =
                let rec loop rev_cl i =
                  match next_char s i with
                    Some (c, i) -> loop (c :: rev_cl) i
                  | None -> rev_implode rev_cl
                in
                loop [] 0
              in
              insert_command key (Insert s) kb
          | Some ((key, B_comm comm_name), _) ->
              begin match command_of_name comm_name with
                Some comm -> insert_command key comm kb
              | None -> kb
              end
          | None -> kb
        in
        loop kb
    | None -> close_in ic; kb
  in
  loop kb

type line = { mutable buf : A.String.t; mutable cur : int; mutable len : int }
type abbrev_data =
  { hist : A.String.t list;
    rpos : int;
    clen : int;
    abbr : A.String.t;
    found : A.String.t list }

type state =
  { od : line;
    nd : line;
    line : line;
    leditrc_name : string;
    mutable leditrc_mtime : float;
    mutable last_line : A.String.t;
    mutable istate : istate;
    mutable shift : int;
    mutable cut : A.String.t;
    mutable init_kb : kb_tree option;
    mutable total_kb : kb_tree option;
    mutable last_comm : command;
    mutable histfile : out_channel option;
    mutable history : A.String.t Cursor.t;
    mutable abbrev : abbrev_data option;
    mutable complete_fn : int;
    mutable complete_fn_screen : int }

let eval_comm s st =
  let rec search_in_tree i kb =
    if i = String.length s then
      match kb with
        KB_tree _ -> Some None
      | KB_comm comm -> Some (Some comm)
      | KB_none -> None
    else
      let c = s.[i] in
      match kb with
        KB_tree cnl -> search_in_node_list c (i + 1) cnl
      | KB_comm _ | KB_none -> None
  and search_in_node_list c i =
    function
      [] -> None
    | n :: nl ->
        if c < n.char then None
        else if c > n.char then search_in_node_list c i nl
        else search_in_tree i n.son
  in
  let kb =
    let leditrc_mtime =
      if Sys.file_exists st.leditrc_name then
        let stat = Unix.stat st.leditrc_name in
        if stat.Unix.st_mtime > st.leditrc_mtime then Some stat.Unix.st_mtime
        else None
      else None
    in
    match leditrc_mtime, st.total_kb with
      None, Some kb -> kb
    | _ ->
        let init_kb =
          match st.init_kb with
            Some kb -> kb
          | None ->
              let kb = init_default_commands KB_none in
              st.init_kb <- Some kb; kb
        in
        let total_kb =
          match leditrc_mtime with
            Some mtime ->
              st.leditrc_mtime <- mtime;
              init_file_commands init_kb st.leditrc_name
          | None -> init_kb
        in
        st.total_kb <- Some total_kb; total_kb
  in
  search_in_tree 0 kb

let put_bs st c = A.Char.prerr_backsp c
let put_space st = output_char stderr ' '
let put_newline st = prerr_endline ""
let flush_out st = flush stderr
let bell () = prerr_string "\007"; flush stderr

let saved_tcio =
  try Some (Unix.tcgetattr Unix.stdin) with Unix.Unix_error (_, _, _) -> None
let edit_tcio = ref None

let set_edit () =
  match saved_tcio with
    Some _ ->
      let tcio =
        match !edit_tcio with
          Some e -> e
        | None ->
            let tcio = Unix.tcgetattr Unix.stdin in
            tcio.Unix.c_echo <- false;
            tcio.Unix.c_icanon <- false;
            tcio.Unix.c_vmin <- 1;
            tcio.Unix.c_isig <- false;
            tcio.Unix.c_ixon <- false;
            edit_tcio := Some tcio;
            tcio
      in
      Unix.tcsetattr Unix.stdin Unix.TCSADRAIN tcio
  | None -> ()
and unset_edit () =
  match saved_tcio with
    Some tcio -> Unix.tcsetattr Unix.stdin Unix.TCSADRAIN tcio
  | None -> ()

let line_set_nth_char line i c =
  if i == A.String.length line.buf then
    line.buf <- A.String.concat line.buf (A.String.of_char c)
  else A.String.set line.buf i c

let line_to_nd st =
  let rec line_rec i =
    if i == st.line.cur then st.nd.cur <- st.nd.len;
    if i < st.line.len then
      let c = A.String.get st.line.buf i in
      if c = A.Char.of_ascii '\t' then
        for i = st.nd.len + 1 to (st.nd.len + 8) / 8 * 8 do
          line_set_nth_char st.nd st.nd.len (A.Char.of_ascii ' ');
          st.nd.len <- st.nd.len + 1
        done
      else if
        match A.Char.ctrl_val c with
          Some c ->
            line_set_nth_char st.nd st.nd.len (A.Char.of_ascii '^');
            line_set_nth_char st.nd (st.nd.len + 1) c;
            st.nd.len <- st.nd.len + 2;
            true
        | None -> false
      then
        ()
      else
        begin match !(A.encoding) with
          Ascii ->
            begin match A.Char.not_ascii_val c with
              Some (c1, c2, c3) ->
                line_set_nth_char st.nd st.nd.len (A.Char.of_ascii '\\');
                line_set_nth_char st.nd (st.nd.len + 1) c1;
                line_set_nth_char st.nd (st.nd.len + 2) c2;
                line_set_nth_char st.nd (st.nd.len + 3) c3;
                st.nd.len <- st.nd.len + 4
            | None ->
                line_set_nth_char st.nd st.nd.len c;
                st.nd.len <- st.nd.len + 1
            end
        | Iso_8859 ->
            begin match A.Char.meta_ctrl_val c with
              Some c ->
                line_set_nth_char st.nd st.nd.len (A.Char.of_ascii 'M');
                line_set_nth_char st.nd (st.nd.len + 1) (A.Char.of_ascii '-');
                line_set_nth_char st.nd (st.nd.len + 2) (A.Char.of_ascii '^');
                line_set_nth_char st.nd (st.nd.len + 3) c;
                st.nd.len <- st.nd.len + 4
            | None ->
                line_set_nth_char st.nd st.nd.len c;
                st.nd.len <- st.nd.len + 1
            end
        | Utf_8 ->
            line_set_nth_char st.nd st.nd.len c; st.nd.len <- st.nd.len + 1
        end;
      line_rec (i + 1)
    else if st.nd.len > !max_len then
      let shift =
        if st.nd.cur - st.shift >= 0 && st.nd.cur - st.shift < !max_len - 2
        then
          st.shift
        else if st.nd.cur < !max_len - 3 then 0
        else st.nd.cur - !max_len / 2
      in
      for i = 0 to !max_len - 3 do
        let ni = i + shift in
        A.String.set st.nd.buf i
          (if ni < st.nd.len then A.String.get st.nd.buf ni
           else A.Char.of_ascii ' ')
      done;
      A.String.set st.nd.buf (!max_len - 2) (A.Char.of_ascii ' ');
      A.String.set st.nd.buf (!max_len - 1)
        (if shift = 0 then A.Char.of_ascii '>'
         else if st.nd.len - shift < !max_len - 2 then A.Char.of_ascii '<'
         else A.Char.of_ascii '*');
      st.nd.cur <- st.nd.cur - shift;
      st.nd.len <- !max_len;
      st.shift <- shift
    else st.shift <- 0
  in
  st.nd.len <- 0; line_rec 0

let display st =
  let rec disp_rec i =
    if i < st.nd.len then
      begin
        if i >= st.od.len ||
           A.String.get st.od.buf i <> A.String.get st.nd.buf i
        then
          begin
            while i < st.od.cur do
              st.od.cur <- st.od.cur - 1;
              put_bs st (A.String.get st.od.buf i)
            done;
            while st.od.cur < i do
              begin let c = A.String.get st.nd.buf st.od.cur in
                st.od.cur <- st.od.cur + 1; A.Char.prerr c
              end
            done;
            let c = A.String.get st.nd.buf i in
            line_set_nth_char st.od i c;
            st.od.cur <- st.od.cur + 1;
            A.Char.prerr c
          end;
        disp_rec (i + 1)
      end
    else
      begin
        if st.od.len > st.nd.len then
          begin
            while st.od.cur < st.od.len do
              begin let c =
                if st.od.cur < st.nd.len then A.String.get st.nd.buf st.od.cur
                else A.Char.of_ascii ' '
              in
                A.Char.prerr c; st.od.cur <- st.od.cur + 1
              end
            done;
            while st.od.cur > st.nd.len do
              st.od.cur <- st.od.cur - 1;
              put_bs st (A.String.get st.od.buf st.od.cur);
              put_space st;
              put_bs st (A.Char.of_ascii ' ')
            done
          end;
        st.od.len <- st.nd.len;
        while st.od.cur < st.nd.cur do
          A.Char.prerr (A.String.get st.nd.buf st.od.cur);
          st.od.cur <- st.od.cur + 1
        done;
        while st.od.cur > st.nd.cur do
          st.od.cur <- st.od.cur - 1;
          put_bs st (A.String.get st.nd.buf st.od.cur)
        done;
        flush_out st
      end
  in
  disp_rec 0

let update_output st = line_to_nd st; display st

let balance_paren st c =
  match A.Char.to_ascii c with
    Some (')' | ']' | '}' as c) ->
      let i =
        let rec find_lparen r i =
          if i < 0 then i
          else
            match A.Char.to_ascii (A.String.get st.line.buf i) with
              Some (')' | ']' | '}' as c) ->
                find_lparen r (find_lparen c (i - 1) - 1)
            | Some '(' -> if r == ')' then i else -1
            | Some '[' -> if r == ']' then i else -1
            | Some '{' -> if r == '}' then i else -1
            | Some '"' ->
                let rec skip_string i =
                  if i < 0 then i
                  else if
                    A.String.get st.line.buf i = A.Char.of_ascii '"'
                  then
                    i - 1
                  else skip_string (i - 1)
                in
                find_lparen r (skip_string (i - 1))
            | _ -> find_lparen r (i - 1)
        in
        find_lparen c (st.line.cur - 2)
      in
      if i >= 0 then
        let c = st.line.cur in
        st.line.cur <- i;
        update_output st;
        st.line.cur <- c;
        let _ = Unix.select [Unix.stdin] [] [] 1.0 in ()
  | Some _ | None -> ()

let delete_char st =
  st.line.len <- st.line.len - 1;
  for i = st.line.cur to st.line.len - 1 do
    A.String.set st.line.buf i (A.String.get st.line.buf (i + 1))
  done

let insert_char st x =
  for i = st.line.len downto st.line.cur + 1 do
    line_set_nth_char st.line i (A.String.get st.line.buf (i - 1))
  done;
  st.line.len <- st.line.len + 1;
  line_set_nth_char st.line st.line.cur x

let move_in_word buf e f g =
  let rec move_rec i =
    if e i then i
    else if A.Char.is_word_char (A.String.get buf i) then f move_rec i
    else g move_rec i
  in
  move_rec

let forward_move line = move_in_word line.buf (fun i -> i == line.len)
let backward_move line = move_in_word line.buf (fun i -> i == -1)

let forward_word line =
  let i = line.cur in
  let i = forward_move line (fun _ i -> i) (fun mv i -> mv (i + 1)) i in
  forward_move line (fun mv i -> mv (i + 1)) (fun _ i -> i) i

let backward_word line =
  let i = line.cur - 1 in
  let i = backward_move line (fun _ i -> i) (fun mv i -> mv (i - 1)) i in
  backward_move line (fun mv i -> mv (i - 1)) (fun _ i -> i) i + 1

let get_word_len st =
  let i = st.line.cur - 1 in
  i - backward_move st.line (fun mv i -> mv (i - 1)) (fun _ i -> i) i

let kill_word st =
  let i = st.line.cur in
  let i =
    forward_move st.line (fun _ i -> i) (fun mv i -> delete_char st; mv i) i
  in
  forward_move st.line (fun mv i -> delete_char st; mv i) (fun _ i -> i) i

let backward_kill_word st =
  let k = backward_word st.line in
  let sh = st.line.cur - k in
  st.line.len <- st.line.len - sh;
  for i = k to st.line.len - 1 do
    A.String.set st.line.buf i (A.String.get st.line.buf (i + sh))
  done;
  k

let capitalize_word st =
  let i = st.line.cur in
  let i0 = forward_move st.line (fun _ i -> i) (fun mv i -> mv (i + 1)) i in
  forward_move st.line
    (fun mv i ->
       let f = if i == i0 then A.Char.uppercase else A.Char.lowercase in
       A.String.set st.line.buf i (f (A.String.get st.line.buf i));
       mv (i + 1))
    (fun _ i -> i) i0

let upcase_word st =
  let i = st.line.cur in
  let i = forward_move st.line (fun _ i -> i) (fun mv i -> mv (i + 1)) i in
  forward_move st.line
    (fun mv i ->
       let f = A.Char.uppercase in
       A.String.set st.line.buf i (f (A.String.get st.line.buf i));
       mv (i + 1))
    (fun _ i -> i) i

let downcase_word st =
  let i = st.line.cur in
  let i = forward_move st.line (fun _ i -> i) (fun mv i -> mv (i + 1)) i in
  forward_move st.line
    (fun mv i ->
       let f = A.Char.lowercase in
       A.String.set st.line.buf i (f (A.String.get st.line.buf i));
       mv (i + 1))
    (fun _ i -> i) i

let transpose_chars st =
  if st.line.cur == st.line.len then
    let c = A.String.get st.line.buf (st.line.cur - 1) in
    A.String.set st.line.buf (st.line.cur - 1)
      (A.String.get st.line.buf (st.line.cur - 2));
    A.String.set st.line.buf (st.line.cur - 2) c
  else
    let c = A.String.get st.line.buf st.line.cur in
    A.String.set st.line.buf st.line.cur
      (A.String.get st.line.buf (st.line.cur - 1));
    A.String.set st.line.buf (st.line.cur - 1) c;
    st.line.cur <- st.line.cur + 1

let set_line st str =
  st.line.len <- 0;
  st.line.cur <- 0;
  for i = 0 to A.String.length str - 1 do
    insert_char st (A.String.get str i);
    st.line.cur <- st.line.len
  done

let save_if_last st =
  if Cursor.is_last_line st.history then
    st.last_line <- A.String.sub st.line.buf 0 st.line.len

let previous_history st =
  try
    save_if_last st;
    Cursor.before st.history;
    set_line st (Cursor.peek st.history)
  with Cursor.Failure -> bell ()

let next_history st =
  try Cursor.after st.history; set_line st (Cursor.peek st.history) with
    Cursor.Failure -> set_line st st.last_line

let reverse_search_history st =
  let question str =
    List.fold_left A.String.concat (A.String.of_ascii "(reverse-i-search)'")
      [str; A.String.of_ascii "': "]
  in
  let make_line str fstr =
    st.line.cur <- 0;
    st.line.len <- 0;
    let len = A.String.length str in
    for i = 0 to len - 1 do
      insert_char st (A.String.get str i);
      st.line.cur <- st.line.cur + 1
    done;
    let len = A.String.length fstr in
    for i = 0 to len - 1 do
      insert_char st (A.String.get fstr i);
      st.line.cur <- st.line.cur + 1
    done
  in
  let initial_str = A.String.sub st.line.buf 0 st.line.len in
  let rec find_line (cnt, fstr) str =
    let rec find_rec ifstr istr =
      if istr == A.String.length str then cnt, fstr
      else if ifstr == A.String.length fstr then
        if try Cursor.before st.history; true with Cursor.Failure -> false
        then
          find_line (cnt + 1, Cursor.peek st.history) str
        else begin bell (); cnt, fstr end
      else if A.String.get str istr <> A.String.get fstr ifstr then
        find_rec (ifstr + 1) 0
      else find_rec (ifstr + 1) (istr + 1)
    in
    find_rec 0 0
  in
  let rec incr_search (cnt, fstr) str =
    let q = question str in
    make_line q fstr;
    st.line.cur <- A.String.length q - 3;
    update_output st;
    let c = A.Char.read () in
    let s = A.Char.to_string c in
    match eval_comm s st with
      Some (Some comm) ->
        begin match comm with
          Backward_delete_char ->
            if A.String.length str == 0 then incr_search (cnt, fstr) str
            else
              let str = A.String.sub str 0 (A.String.length str - 1) in
              for i = 1 to cnt do Cursor.after st.history done;
              incr_search (find_line (0, initial_str) str) str
        | Reverse_search_history ->
            let (cnt, fstr) =
              try
                Cursor.before st.history;
                find_line (cnt + 1, Cursor.peek st.history) str
              with Cursor.Failure -> bell (); cnt, initial_str
            in
            incr_search (cnt, fstr) str
        | Abort ->
            for i = 1 to cnt do Cursor.after st.history done;
            bell ();
            initial_str
        | Self_insert ->
            let str = A.String.concat str (A.String.of_char c) in
            incr_search (find_line (cnt, fstr) str) str
        | _ -> fstr
        end
    | Some None ->
        if s = "\027" then fstr
        else
          let str = A.String.concat str (A.String.of_char c) in
          incr_search (find_line (cnt, fstr) str) str
    | None ->
        let str = A.String.concat str (A.String.of_char c) in
        incr_search (find_line (cnt, fstr) str) str
  in
  let fstr = incr_search (0, initial_str) A.String.empty in
  make_line A.String.empty fstr

let rec beginning_of_history st =
  save_if_last st;
  Cursor.goto_first st.history;
  try set_line st (Cursor.peek st.history) with Cursor.Failure -> bell ()

let rec end_of_history st =
  Cursor.goto_last st.history; set_line st st.last_line

let rec back_search st ad hist rpos =
  match hist with
    [] ->
      for i = 0 to A.String.length ad.abbr - 1 do
        insert_char st (A.String.get ad.abbr i);
        st.line.cur <- st.line.cur + 1
      done;
      bell ()
  | l :: ll ->
      let i = A.String.length l - rpos in
      if i <= 0 then back_search st ad ll 0
      else
        let i = backward_word {buf = l; cur = i; len = A.String.length l} in
        if A.String.length l - i < A.String.length ad.abbr then
          back_search st ad (l :: ll) (A.String.length l - i)
        else if A.String.sub l i (A.String.length ad.abbr) = ad.abbr then
          let i1 = forward_word {buf = l; cur = i; len = A.String.length l} in
          let f = A.String.sub l i (i1 - i) in
          if List.mem f ad.found then
            back_search st ad (l :: ll) (A.String.length l - i)
          else
            let ad =
              {hist = l :: ll; rpos = A.String.length l - i1; clen = i1 - i;
               abbr = ad.abbr; found = f :: ad.found}
            in
            st.abbrev <- Some ad;
            for i = 0 to A.String.length f - 1 do
              insert_char st (A.String.get f i);
              st.line.cur <- st.line.cur + 1
            done
        else back_search st ad (l :: ll) (A.String.length l - i)

let expand_abbrev st abbrev =
  let ad =
    match abbrev with
      Some x -> x
    | None ->
        let len = get_word_len st in
        let abbr = A.String.sub st.line.buf (st.line.cur - len) len in
        let line_beg = A.String.sub st.line.buf 0 (st.line.cur - len) in
        let line_end =
          A.String.sub st.line.buf st.line.cur (st.line.len - st.line.cur)
        in
        {hist = line_beg :: (Cursor.get_all st.history @ [line_end]);
         rpos = 0; clen = len; abbr = abbr; found = [abbr]}
  in
  for i = 1 to ad.clen do st.line.cur <- st.line.cur - 1; delete_char st done;
  back_search st ad ad.hist ad.rpos;
  update_output st

let start_with s s_ini =
  let len = String.length s_ini in
  String.length s >= len && String.sub s 0 len = s_ini

let insert_string st s =
  String.iter
    (fun c ->
       insert_char st (A.Char.of_ascii c); st.line.cur <- st.line.cur + 1)
    s

let is_directory fn = try Sys.is_directory fn with Sys_error _ -> false

let print_file_list st max_flen nb_by_line dirname files =
  let rec loop n =
    function
      file :: files ->
        if n = nb_by_line then
          begin prerr_endline ""; loop 0 (file :: files) end
        else
          begin
            prerr_string file;
            let fn = Filename.concat dirname file in
            if is_directory fn then prerr_string Filename.dir_sep
            else prerr_string " ";
            if n < nb_by_line - 1 then
              prerr_string
                (String.make (max_flen + 1 - String.length file) ' ');
            loop (n + 1) files
          end
    | [] -> ()
  in
  loop 0 files

let rev_take n list =
  let rec loop rev_list n =
    function
      x :: l ->
        if n = 0 then rev_list, x :: l else loop (x :: rev_list) (n - 1) l
    | [] -> rev_list, []
  in
  loop [] n list

let max_lines_in_screen = ref 24

let complete_file_name st =
  let s =
    let rec loop s i =
      if i < 0 then s
      else
        match A.Char.to_ascii (A.String.get st.line.buf i) with
          Some c ->
            begin match c with
              'a'..'z' | 'A'..'Z' | '0'..'9' | '.' | '_' | '-' | '/' | '#' |
              '~' ->
                loop (String.make 1 c ^ s) (i - 1)
            | _ -> s
            end
        | None -> s
    in
    loop "" (st.line.cur - 1)
  in
  let dirname = Filename.dirname s in
  let basename = Filename.basename s in
  if is_directory dirname then
    let files = Array.to_list (Sys.readdir dirname) in
    let (files, basename) =
      if s = "" then files, ""
      else if s = Filename.current_dir_name ^ Filename.dir_sep then files, ""
      else if s = Filename.parent_dir_name ^ Filename.dir_sep then files, ""
      else if
        basename = Filename.current_dir_name &&
        s <> Filename.concat dirname basename
      then
        files, ""
      else
        Filename.current_dir_name :: Filename.parent_dir_name :: files,
        basename
    in
    let files = List.filter (fun fn -> start_with fn basename) files in
    let files = List.sort compare files in
    let max_flen =
      List.fold_left (fun max_len fn -> max max_len (String.length fn)) 0
        files
    in
    let nb_by_line = max 1 ((!max_len + 2) / (max_flen + 2)) in
    let nfiles = List.length files in
    let nb_lines = (nfiles + nb_by_line - 1) / nb_by_line in
    let mlis = !max_lines_in_screen - 1 in
    let nb_screens = (nb_lines + mlis - 1) / mlis in
    let (files_to_display, screen_nb) =
      if nb_screens <= 1 then files, 0
      else
        let nb_files_by_screen = nb_by_line * mlis in
        let screen_nb = st.complete_fn_screen mod nb_screens in
        let rev_files =
          let rec loop n files =
            let (rev_files, rest) = rev_take nb_files_by_screen files in
            if n = 0 then rev_files else loop (n - 1) rest
          in
          loop screen_nb files
        in
        st.complete_fn_screen <- st.complete_fn_screen + 1;
        List.rev
          (if screen_nb < nb_screens - 1 then "..." :: rev_files
           else rev_files),
        screen_nb
    in
    match files with
      [] -> ()
    | [fname] ->
        let len = String.length basename in
        let s = String.sub fname len (String.length fname - len) in
        insert_string st s;
        let fn = Filename.concat dirname fname in
        if is_directory fn then insert_string st Filename.dir_sep
        else insert_string st " ";
        update_output st
    | file :: files ->
        let common =
          let rec loop common =
            function
              file :: files ->
                if start_with file common then loop common files
                else if common = "" then ""
                else
                  loop (String.sub common 0 (String.length common - 1))
                    (file :: files)
            | [] -> common
          in
          loop file files
        in
        let len = String.length basename in
        if String.length common > len then
          begin let s = String.sub common len (String.length common - len) in
            insert_string st s; update_output st
          end;
        put_newline st;
        st.od.cur <- 0;
        st.od.len <- 0;
        prerr_string "*** files";
        if nb_screens > 1 then eprintf " (%d/%d)" (screen_nb + 1) nb_screens;
        prerr_endline " ***";
        print_file_list st max_flen nb_by_line dirname files_to_display;
        put_newline ();
        flush stderr;
        update_output st

let rec update_line st comm c =
  let abbrev = st.abbrev in
  st.abbrev <- None;
  if comm = Complete_file_name then st.complete_fn <- st.complete_fn + 1
  else begin st.complete_fn <- 0; st.complete_fn_screen <- 0 end;
  match comm with
    Beginning_of_line ->
      if st.line.cur > 0 then begin st.line.cur <- 0; update_output st end
  | End_of_line ->
      if st.line.cur < st.line.len then
        begin st.line.cur <- st.line.len; update_output st end
  | Forward_char ->
      if st.line.cur < st.line.len then
        begin st.line.cur <- st.line.cur + 1; update_output st end
  | Backward_char ->
      if st.line.cur > 0 then
        begin st.line.cur <- st.line.cur - 1; update_output st end
  | Forward_word ->
      if st.line.cur < st.line.len then
        begin st.line.cur <- forward_word st.line; update_output st end
  | Backward_word ->
      if st.line.cur > 0 then
        begin st.line.cur <- backward_word st.line; update_output st end
  | Capitalize_word ->
      if st.line.cur < st.line.len then
        begin st.line.cur <- capitalize_word st; update_output st end
  | Upcase_word ->
      if st.line.cur < st.line.len then
        begin st.line.cur <- upcase_word st; update_output st end
  | Downcase_word ->
      if st.line.cur < st.line.len then
        begin st.line.cur <- downcase_word st; update_output st end
  | Previous_history -> previous_history st; update_output st
  | Next_history -> next_history st; update_output st
  | Beginning_of_history -> beginning_of_history st; update_output st
  | End_of_history -> end_of_history st; update_output st
  | Reverse_search_history -> reverse_search_history st; update_output st
  | Delete_char_or_end_of_file ->
      if st.line.len = 0 then raise End_of_file;
      if st.line.cur < st.line.len then
        begin delete_char st; update_output st end
  | Delete_char ->
      if st.line.cur < st.line.len then
        begin delete_char st; update_output st end
  | Backward_delete_char ->
      if st.line.cur > 0 then
        begin
          st.line.cur <- st.line.cur - 1;
          delete_char st;
          update_output st
        end
  | Transpose_chars ->
      if st.line.len > 1 && st.line.cur > 0 then
        begin transpose_chars st; update_output st end
  | Kill_word ->
      if st.line.cur < st.line.len then
        begin st.line.cur <- kill_word st; update_output st end
  | Backward_kill_word ->
      if st.line.cur > 0 then
        begin st.line.cur <- backward_kill_word st; update_output st end
  | Quoted_insert -> st.istate <- Quote
  | Sequence s -> st.istate <- Normal s
  | Self_insert ->
      insert_char st c;
      st.line.cur <- st.line.cur + 1;
      balance_paren st c;
      update_output st
  | Insert s ->
      let strm = Stream.of_string s in
      begin try
        while true do
          insert_char st (A.Char.parse strm);
          st.line.cur <- st.line.cur + 1
        done
      with Stream.Failure -> ()
      end;
      update_output st
  | Expand_abbrev -> expand_abbrev st abbrev
  | Complete_file_name -> complete_file_name st
  | Redraw_current_line ->
      put_newline st; st.od.cur <- 0; st.od.len <- 0; update_output st
  | Kill_line ->
      st.cut <-
        A.String.sub st.line.buf st.line.cur (st.line.len - st.line.cur);
      if st.line.len > st.line.cur then
        begin st.line.len <- st.line.cur; update_output st end
  | Unix_line_discard ->
      if st.line.cur > 0 then
        let len = st.line.len - st.line.cur in
        for i = 0 to len - 1 do
          A.String.set st.line.buf i
            (A.String.get st.line.buf (st.line.cur + i))
        done;
        st.line.cur <- 0;
        st.line.len <- len;
        update_output st
  | Yank ->
      if A.String.length st.cut > 0 then
        begin
          for i = 0 to A.String.length st.cut - 1 do
            insert_char st (A.String.get st.cut i);
            st.line.cur <- st.line.cur + 1
          done;
          update_output st
        end
  | Abort -> bell ()
  | Interrupt ->
      if st.line.cur > 0 then
        begin st.line.cur <- 0; st.line.len <- 0; update_output st end;
      begin match !son with
        Some pid -> Unix.kill pid Sys.sigint
      | _ -> ()
      end
  | Suspend ->
      unset_edit ();
      Unix.kill (Unix.getpid ()) Sys.sigtstp;
      set_edit ();
      st.od.cur <- 0;
      st.od.len <- 0;
      update_output st
  | Quit ->
      begin match !son with
        Some pid -> Unix.kill pid Sys.sigquit
      | _ -> ()
      end
  | _ -> ()

let save_history st line =
  let last_line =
    try Cursor.peek_last st.history with Cursor.Failure -> A.String.empty
  in
  if line <> last_line && line <> A.String.empty then
    begin
      Cursor.insert_last st.history line;
      match st.histfile with
        Some fdo -> A.String.output fdo line; output_char fdo '\n'; flush fdo
      | None -> ()
    end

let trace_sequences = ref false

let (edit_line, open_histfile, close_histfile) =
  let st =
    {od = {buf = A.String.empty; cur = 0; len = 0};
     nd = {buf = A.String.empty; cur = 0; len = 0};
     line = {buf = A.String.empty; cur = 0; len = 0};
     leditrc_name =
       begin try Sys.getenv "LEDITRC" with
         Not_found ->
           begin try Filename.concat (Sys.getenv "HOME") ".leditrc" with
             Not_found -> ".leditrc"
           end
       end;
     leditrc_mtime = 0.0; last_line = A.String.empty; istate = Normal "";
     shift = 0; init_kb = None; total_kb = None; cut = A.String.empty;
     last_comm = Accept_line; histfile = None; history = Cursor.create ();
     abbrev = None; complete_fn = 0; complete_fn_screen = 0}
  in
  let edit_line () =
    let rec edit_loop () =
      let c = A.Char.read () in
      if !trace_sequences then
        begin
          put_newline st;
          eprintf "<%s>\n" (String.escaped (A.Char.to_string c));
          st.od.cur <- 0;
          st.od.len <- 0;
          update_output st
        end;
      let comm =
        match st.istate with
          Quote -> Self_insert
        | Normal s ->
            let s = s ^ A.Char.to_string c in
            match eval_comm s st with
              Some (Some comm) -> comm
            | Some None -> Sequence s
            | None -> Self_insert
      in
      st.istate <- Normal "";
      st.last_comm <- comm;
      match comm with
        Accept_line | Operate_and_get_next ->
          let v_max_len = !max_len in
          max_len := 10000;
          update_output st;
          max_len := v_max_len;
          put_newline st;
          let line = A.String.sub st.line.buf 0 st.line.len in
          st.abbrev <- None; save_history st line; line
      | _ -> update_line st comm c; edit_loop ()
    in
    st.od.len <- 0;
    st.od.cur <- 0;
    st.line.len <- 0;
    st.line.cur <- 0;
    if st.last_comm = Operate_and_get_next then
      try
        let _ = Unix.select [] [] [] 0.001 in
        Cursor.after st.history;
        set_line st (Cursor.peek st.history);
        update_output st
      with Cursor.Failure -> ()
    else Cursor.goto_last st.history;
    edit_loop ()
  and open_histfile trunc file =
    if not trunc then
      begin match (try Some (open_in file) with _ -> None) with
        Some fi ->
          begin try
            while true do
              Cursor.insert st.history (A.String.input_line fi)
            done
          with End_of_file -> ()
          end;
          close_in fi
      | _ -> ()
      end;
    let fd =
      Unix.openfile file
        ([Unix.O_WRONLY; Unix.O_CREAT] @
         (if trunc then [Unix.O_TRUNC] else []))
        0o666
    in
    let fdo = Unix.out_channel_of_descr fd in
    if not trunc then seek_out fdo (out_channel_length fdo);
    st.histfile <- Some fdo
  and close_histfile () =
    match st.histfile with
      Some fdo -> close_out fdo
    | None -> ()
  in
  edit_line, open_histfile, close_histfile

let (set_prompt, get_prompt, input_a_char) =
  let prompt = ref ""
  and buff = ref A.String.empty
  and ind = ref 1 in
  let set_prompt x = prompt := x
  and get_prompt () = !prompt
  and input_a_char ic =
    if ic != stdin then A.Char.input ic
    else
      begin
        if !ind > A.String.length !buff then
          begin
            prerr_string !prompt;
            flush stderr;
            begin try set_edit (); buff := edit_line (); unset_edit () with
              e -> unset_edit (); raise e
            end;
            ind := 0
          end;
        let c =
          if !ind == A.String.length !buff then A.Char.of_ascii '\n'
          else A.String.get !buff !ind
        in
        ind := !ind + 1; c
      end
  in
  set_prompt, get_prompt, input_a_char

let input_char ic = A.Char.to_string (input_a_char ic)
