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
      let* as_args, bounds, ty_args =
        IGPat.gen_many ~dir:`Left (List1.to_list args)
      in

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
        List.fold ty_args ~init:ty_expr ~f:(fun acc ty_arg ->
            Ty.Arr (ty_arg, acc) )
      in

      return (as_args ++ (as_expr -- Map.keys bounds), ty_res)
  | Tuple exprs ->
      let* asm, tys = gen_many ~dir:`Right (List2.to_list exprs) in
      return (asm, Ty.Tuple (List2.of_list_exn tys))
  | If (econd, ethen, eelse) ->
      let* as_cond, ty_cond = gen econd in
      let* as_then, ty_then = gen ethen in
      let* as_else, ty_else =
        match eelse with None -> return (As.empty, Ty.unit) | Some e -> gen e
      in

      let* () = cs [ty_cond == Ty.bool; ty_then == ty_else] in
      return (as_cond ++ as_then ++ as_else, ty_then)
  | Seq exprs ->
      let* asm, tys = gen_many ~dir:`Left (List2.to_list exprs) in
      return (asm, List.hd_exn tys)
  | Construct (id, arg) ->
      let* var = fresh in
      let as_con = As.single id (VarSet.single var) in
      let ty_con = Ty.Var var in

      let* ty_res = fresh >>| fun var -> Ty.Var var in
      let* as_arg =
        match arg with
        | None ->
            let* () = cs [ty_con == ty_res] in
            return As.empty
        | Some arg ->
            let* as_arg, ty_arg = gen arg in
            let* () = cs [ty_con == Arr (ty_arg, ty_res)] in
            return as_arg
      in

      return (as_con ++ as_arg, ty_res)
  | _ ->
      assert false

and gen_many :
    dir:[`Left | `Right] -> Expr.t list -> (As.t * Ty.t list) IGMonad.t =
 fun ~dir l ->
  fold l ~dir ~init:(As.empty, []) ~f:(fun (as_acc, tys_acc) expr ->
      let* as_expr, ty_expr = gen expr in
      return (as_acc ++ as_expr, ty_expr :: tys_acc) )
