[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Angstrom
open LAst

(* ======= Utils ======= *)
val list1_exn : 'a list -> 'a list1

val unit : unit t
val ws : unit t
val ws1 : unit t
val ident : string -> ident t

val parens : 'a t -> 'a t
val spaced : 'a t -> 'a t
val opt : 'a t -> 'a option t

(* ======= Identifiers ======= *)
val pconstruct_id : ident t
val pvalue_id : ident t
val pty_var_id : ident t
val pty_con_id : ident t

val pinfix_id : ?starts:string -> unit -> ident t
val pprefix_id : ident t

(* ===== Constants, value bindings ===== *)
val pconst : constant t
val plet : expression t -> pattern t -> (rec_flag * value_binding list1) t

(* ======= Operators ======= *)
type ('op, 'oprnd) op_kind =
  | Prefix of {apply: 'op -> 'oprnd -> 'oprnd}
  | Infix of {assoc: [`Left | `Right]; apply: 'op -> 'oprnd -> 'oprnd -> 'oprnd}

type 'oprnd op_parse =
  | Op :
      { pop: 'op t  (** Operator symbol parser *)
      ; kind: ('op, 'oprnd) op_kind  (** Kind of an operator *) }
      -> 'oprnd op_parse

(**
  Order in a list sets operators' priority.
  First operator in a table has the highest priority
*)
type 'oprnd op_parse_table = 'oprnd op_parse list

val poperators : table:'oprnd op_parse_table -> poprnd:'oprnd t -> 'oprnd t
