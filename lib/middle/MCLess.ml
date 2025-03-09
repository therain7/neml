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
type def = DefFunc of Expr.rec_flag * Id.t * func

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
 fun (DefFunc (_, id, Fun (args, cexpr))) ->
  let efunc : Expr.t =
    Fun (List1.map args ~f:(fun id -> Pat.Var id), to_expr cexpr)
  in
  Let (Nonrec, List1.of_list_exn [Expr.{pat= Pat.Var id; expr= efunc}])

let to_structure ((defs, cexpr) : t) : structure =
  List.fold_right defs
    ~init:[Eval (to_expr cexpr)]
    ~f:(fun def acc -> to_stritem def :: acc)

let subst ~(from : Id.t) ~(to_ : Id.t) : cexpr -> cexpr =
  let rec f = function
    | Id id when Id.equal id from ->
        Id to_
    | Apply (cexp1, cexp2) ->
        Apply (f cexp1, f cexp2)
    | Seq cexps ->
        Seq (List2.map cexps ~f)
    | If (ccond, cthen, celse) ->
        If (f ccond, f cthen, f celse)
    | (Id _ | Const _ | Unit) as cexpr ->
        cexpr
  in
  f

(** Performs closure conversion and lambda lifting *)
let from_simpl (globals : IdSet.t) (sim : MSimpl.t) : t =
  let cnt = ref (-1) in
  let defs : def list ref = ref [] in

  let define ~(recf : MSimpl.rec_flag) ~(args : Id.t List1.t) ~cexpr : Id.t =
    let fresh : Id.t =
      cnt := !cnt + 1 ;
      I ("f" ^ Int.to_string !cnt)
    in

    let (recf : Expr.rec_flag), func =
      match recf with
      | Nonrec ->
          (Nonrec, Fun (args, cexpr))
      | Rec id_rec ->
          (Rec, Fun (args, subst ~from:id_rec ~to_:fresh cexpr))
    in

    defs := DefFunc (recf, fresh, func) :: !defs ;
    fresh
  in

  let rec f : MSimpl.t -> cexpr = function
    | Fun (recf, args, sim) ->
        let args = List1.to_list args in
        let bound =
          (match recf with Nonrec -> args | Rec id_rec -> id_rec :: args)
          |> IdSet.of_list |> Set.union globals
        in
        let free = Set.diff (MSimpl.free sim) bound |> Set.to_list in

        let id_func =
          define ~recf
            ~args:(List.concat [free; args] |> List1.of_list_exn)
            ~cexpr:(f sim)
        in
        List.fold free ~init:(Id id_func) ~f:(fun acc id -> Apply (acc, Id id))
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
