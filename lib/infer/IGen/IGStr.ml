[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LMisc
open LAst
open LTypes

open IGCommon
open IGMonad

let gen : StrItem.t -> (As.t * Bounds.t * Ty.t option) t = function
  | Eval expr ->
      let* asm, ty = IGExpr.gen expr in
      return (asm, Bounds.empty, Some ty)
  | Let (Nonrec, bindings) ->
      let* asm, bounds =
        fold ~dir:`Left (List1.to_list bindings) ~init:(As.empty, Bounds.empty)
          ~f:(fun (as_acc, bounds_acc) {pat; expr} ->
            let* as_pat, bounds_pat, ty_pat = IGPat.gen pat in
            let* as_expr, ty_expr = IGExpr.gen expr in

            let* () = cs [ty_pat == ty_expr] in
            let* bounds = Bounds.merge bounds_acc bounds_pat in
            return (as_acc ++ as_pat ++ as_expr, bounds) )
      in
      return (asm, bounds, None)
  | Let (Rec, _) ->
      fail (NotImplemented "recursive value bindings")
  | Type _ ->
      fail (NotImplemented "type definitions")
