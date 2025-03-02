[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open LTypes

val pp : Ty.t -> PPrint.document
val pp_var : Var.t -> PPrint.document
