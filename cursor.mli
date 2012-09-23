(***********************************************************************)
(*                                                                     *)
(*                               Ledit                                 *)
(*                                                                     *)
(*                Daniel de Rauglaudre, INRIA Rocquencourt             *)
(*                                                                     *)
(*  Copyright 2001-2008 Institut National de Recherche en Informatique *)
(*  et Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

type 'a t

exception Failure

val create : unit -> 'a t
val before : 'a t -> unit
val after : 'a t -> unit
val insert : 'a t -> 'a -> unit
val insert_last : 'a t -> 'a -> unit
val peek : 'a t -> 'a
val peek_last : 'a t -> 'a
val goto_first : 'a t -> unit
val goto_last : 'a t -> unit
val get_all : 'a t -> 'a list
val is_last_line : 'a t -> bool
