[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LAst
open LTypes

open ICommon

module As = IGCommon.As
module Bounds = IGCommon.Bounds

type output =
  {assumptions: As.t; bounds: Bounds.t; constraints: ConSet.t; ty: Ty.t option}

let gen (item : StrItem.t) : (output, IError.t) Result.t =
  let open Result in
  let constraints, res = IGCommon.IGMonad.run (IGStr.gen item) in
  res >>| fun (assumptions, bounds, ty) -> {assumptions; bounds; constraints; ty}
