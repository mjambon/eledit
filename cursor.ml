(***********************************************************************)
(*                                                                     *)
(*                               Ledit                                 *)
(*                                                                     *)
(*                Daniel de Rauglaudre, INRIA Rocquencourt             *)
(*                                                                     *)
(*  Copyright 2001-2006 Institut National de Recherche en Informatique *)
(*  et Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cursor.ml,v 1.8 2008-01-01 18:46:28 deraugla Exp $ *)

type 'a t =
  { mutable before : 'a list;
    mutable current : 'a option;
    mutable after : 'a list }

exception Failure

let create () = {before = []; current = None; after = []}

let before c =
  match c.before with
    [] -> raise Failure
  | x :: l ->
      begin match c.current with
        Some y -> c.after <- y :: c.after
      | _ -> ()
      end;
      c.current <- Some x;
      c.before <- l

let after c =
  match c.current with
    None -> raise Failure
  | Some y ->
      c.before <- y :: c.before;
      match c.after with
        [] -> c.current <- None
      | x :: l -> c.current <- Some x; c.after <- l

let is_last_line c = c.current = None

let insert c x =
  begin match c.current with
    Some y -> c.before <- y :: c.before
  | None -> ()
  end;
  c.current <- Some x

let insert_last c x =
  match c.current with
    Some _ -> c.after <- c.after @ [x]
  | None -> c.current <- Some x

let peek c =
  match c.current with
    Some y -> y
  | None -> raise Failure

let peek_last c =
  let rec peek_rec =
    function
      [] -> raise Failure
    | [x] -> x
    | _ :: l -> peek_rec l
  in
  match c.before, c.current, c.after with
    _, Some x, [] -> x
  | x :: _, None, [] -> x
  | _, _, l -> peek_rec l

let rec goto_first c = try while true do before c done with Failure -> ()

let rec goto_last c = try while true do after c done with Failure -> ()

let get_all c =
  let end_list =
    match c.current with
      Some y -> y :: c.after
    | None -> c.after
  in
  List.rev_append c.before end_list
