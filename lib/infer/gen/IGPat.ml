[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LMisc
open LAst
open LTypes

open ICommon
open IGCommon
open IGMonad

module Bounds = struct
  type t = (Id.t, Var.t, Id.comparator_witness) Map.t

  let empty = Map.empty (module Id)
  let single x = Map.singleton (module Id) x

  exception Rebound of Id.t
  let merge m1 m2 =
    try
      return
      @@ Map.merge_skewed m1 m2 ~combine:(fun ~key:id _ _ ->
             raise (Rebound id) )
    with Rebound id -> fail (PatVarBoundSeveralTimes id)
end

let rec gen : Pat.t -> (As.t * Bounds.t * Ty.t) IGMonad.t = function
  | Var id ->
      let* var = fresh in
      return (As.empty, Bounds.single id var, Ty.Var var)
  | Any ->
      let* var = fresh in
      return (As.empty, Bounds.empty, Ty.Var var)
  | Const const ->
      return (As.empty, Bounds.empty, typeof_const const)
  | Tuple pats ->
      let* asm, bounds, tys = gen_many (List2.to_list pats) in
      return (asm, bounds, Ty.Tuple (List2.of_list_exn tys))
  | Constraint (pat, ty) ->
      let* asm, bounds, ty_pat = gen pat in
      let* () = cs [ty_pat == ty] in
      return (asm, bounds, ty_pat)
  | Construct (id, arg) ->
      let* var = fresh in
      let as_con = As.single id (VarSet.single var) in
      let ty_con = Ty.Var var in

      let* ty_res = fresh >>| fun var -> Ty.Var var in
      let* as_arg, bounds_arg =
        match arg with
        | None ->
            let* () = cs [ty_con == ty_res] in
            return (As.empty, Bounds.empty)
        | Some arg ->
            let* as_arg, bounds_arg, ty_arg = gen arg in
            let* () = cs [ty_con == Arr (ty_arg, ty_res)] in
            return (as_arg, bounds_arg)
      in

      return (as_con ++ as_arg, bounds_arg, ty_res)
  | Or _ ->
      assert false

and gen_many : Pat.t list -> (As.t * Bounds.t * Ty.t list) IGMonad.t =
  fold_right ~init:(As.empty, Bounds.empty, [])
    ~f:(fun pat (as_acc, bounds_acc, tys_acc) ->
      let* as_pat, bounds_pat, ty_pat = gen pat in
      let* bounds = Bounds.merge bounds_acc bounds_pat in
      return (as_acc ++ as_pat, bounds, ty_pat :: tys_acc) )
