(***********************************************************************)
(*                                                                     *)
(*                               Ledit                                 *)
(*                                                                     *)
(*                Daniel de Rauglaudre, INRIA Rocquencourt             *)
(*                                                                     *)
(*  Copyright 1997-2008 Institut National de Recherche en Informatique *)
(*  et Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

val input_char : in_channel -> string

val set_prompt : string -> unit
val get_prompt : unit -> string
val open_histfile : bool -> string -> unit
val close_histfile : unit -> unit
val set_max_len : int -> unit
val set_son_pid : int -> unit

val unset_meta_as_escape : unit -> unit
val set_utf8 : unit -> unit
val set_ascii : unit -> unit

val trace_sequences : bool ref
