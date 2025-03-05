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

let rec gen : Expr.t -> (As.t * Ty.t) IGMonad.t = function
  | Id id ->
      let* var = fresh in
      return (As.single id (VarSet.single var), Ty.Var var)
  | Const const ->
      return (As.empty, typeof_const const)
  | Apply (efun, earg) ->
      let* as_fun, ty_fun = gen efun in
      let* as_arg, ty_arg = gen earg in
      let* ty_res = fresh >>| fun var -> Ty.Var var in

      let* () = cs [ty_fun == Arr (ty_arg, ty_res)] in
      return (as_fun ++ as_arg, ty_res)
  | Fun (args, expr) ->
      let* as_args, bounds, ty_args = IGPat.gen_many (List1.to_list args) in

      let* as_expr, ty_expr =
        extend_vars (Set.of_list (module Var) (Map.data bounds)) (gen expr)
      in

      (* Create constraints for the type of every pattern
         and its occurrences in expression *)
      let* () =
        Map.fold bounds ~init:(return ()) ~f:(fun ~key:id ~data:var_pat acc ->
            let* () = acc in
            Map.find as_expr id
            |> Option.value ~default:VarSet.empty
            |> Set.fold ~init:(return ()) ~f:(fun acc var_expr ->
                   let* () = acc in
                   cs [Var var_pat == Var var_expr] ) )
      in

      let ty_res =
        List.fold_right ty_args ~init:ty_expr ~f:(fun ty_arg acc ->
            Ty.Arr (ty_arg, acc) )
      in

      return (as_args ++ (as_expr -- Map.keys bounds), ty_res)
  | _ ->
      assert false
