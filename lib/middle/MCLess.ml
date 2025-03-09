[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LMisc
open LAst

open MCommon

type cexpr =
  | Id of Id.t
  | Const of Const.t
  | Apply of cexpr * cexpr
  | If of cexpr * cexpr * cexpr
  | Seq of cexpr List2.t
  | Unit

type func = Fun of Id.t List1.t * cexpr
type def = DefFunc of Id.t * func

type t = def list * cexpr

let rec to_expr : cexpr -> Expr.t = function
  | Id id ->
      Id id
  | Const const ->
      Const const
  | Apply (c1, c2) ->
      Apply (to_expr c1, to_expr c2)
  | If (ccond, cthen, celse) ->
      If (to_expr ccond, to_expr cthen, Some (to_expr celse))
  | Seq cexprs ->
      Seq (List2.map cexprs ~f:to_expr)
  | Unit ->
      Expr.unit

let to_stritem : def -> StrItem.t =
 fun (DefFunc (id, Fun (args, cexpr))) ->
  let efunc : Expr.t =
    Fun (List1.map args ~f:(fun id -> Pat.Var id), to_expr cexpr)
  in
  Let (Nonrec, List1.of_list_exn [Expr.{pat= Pat.Var id; expr= efunc}])

let to_structure ((defs, cexpr) : t) : structure =
  List.fold_right defs
    ~init:[Eval (to_expr cexpr)]
    ~f:(fun def acc -> to_stritem def :: acc)

let from_simpl (globals : IdSet.t) (sim : MSimpl.t) : t =
  let cnt = ref (-1) in
  let defs : def list ref = ref [] in

  let def (f : func) : Id.t =
    let id : Id.t =
      cnt := !cnt + 1 ;
      I ("f" ^ Int.to_string !cnt)
    in
    defs := DefFunc (id, f) :: !defs ;
    id
  in

  let rec f : MSimpl.t -> cexpr = function
    | Fun (args, sim) ->
        let args = List1.to_list args in
        let bound = Set.union globals (IdSet.of_list args) in
        let free = Set.diff (MSimpl.free sim) bound |> Set.to_list in

        let id_func =
          def (Fun (List.concat [free; args] |> List1.of_list_exn, f sim))
        in
        List.fold free ~init:(Id id_func) ~f:(fun acc id -> Apply (acc, Id id))
    | Fix _ ->
        assert false
    | Id id ->
        Id id
    | Apply (sim1, sim2) ->
        Apply (f sim1, f sim2)
    | If (scond, sthen, selse) ->
        If (f scond, f sthen, f selse)
    | Seq sims ->
        Seq (List2.map sims ~f)
    | Const const ->
        Const const
    | Unit ->
        Unit
  in

  let cexpr = f sim in
  (List.rev !defs, cexpr)
