[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LMisc
open LAst
open LTypes

open ICommon

module IError = IError

module Env = struct
  type t = (Id.t, Ty.t, Id.comparator_witness) Map.t

  let empty : t = Map.empty (module Id)
  let single x y : t = Map.singleton (module Id) x y
end

type output = {ty: Ty.t option; env: Env.t; bounds: Id.t list}

let infer (env : Env.t) (item : StrItem.t) : (output, IError.t) Result.t =
  let open Result in
  let ( let* ) = ( >>= ) in

  (* generate type constraints *)
  let* {assumptions= asm; bounds; constraints= cs; ty} = IGen.gen item in

  (* everything left in assumptions are unknown identifiers.
     try to find them in environment and assign respective type
     by adding ExplInst constraints *)
  let* cs =
    Map.fold asm ~init:(return cs) ~f:(fun ~key:id ~data:vars acc ->
        let* acc : ConSet.t = acc in

        let* sc =
          Map.find env id
          |> Option.value_map ~default:(fail (IError.UnboundVariable id))
               ~f:(fun ty -> return (generalize VarSet.empty ty) )
        in
        Set.fold vars ~init:acc ~f:(fun acc var ->
            Set.add acc (ExplInst (Var var, sc)) )
        |> return )
  in

  (* solve type constraints *)
  let* sub = ISolve.solve cs in

  (* apply substitution & add new bounds to type environment *)
  let env =
    Map.fold bounds ~init:env ~f:(fun ~key ~data:var ->
        Map.set ~key ~data:(ISolve.Sub.apply sub (Var var)) )
  in
  let ty = Option.map ty ~f:(ISolve.Sub.apply sub) in

  return {ty; env; bounds= Map.keys bounds}
