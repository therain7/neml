[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LMisc
open LAst

open MCommon

type rec_flag = Nonrec | Rec of Id.t [@@deriving show {with_path= false}]

type t =
  | Id of Id.t
  | Const of Const.t
  | Fun of rec_flag * Id.t List1.t * t
  | Apply of t * t
  | If of t * t * t
  | Seq of t List2.t
  | Unit
[@@deriving show {with_path= false}]

let rec to_expr : t -> Expr.t = function
  | Id id ->
      Id id
  | Const const ->
      Const const
  | Fun (Nonrec, args, sim) ->
      Fun (List1.map args ~f:(fun id -> Pat.Var id), to_expr sim)
  | Fun (Rec id, args, sim) ->
      let pats = List1.map (List1.cons id args) ~f:(fun id -> Pat.Var id) in
      Apply (Id id, Fun (pats, to_expr sim))
  | Apply (sim1, sim2) ->
      Apply (to_expr sim1, to_expr sim2)
  | If (scond, sthen, selse) ->
      If (to_expr scond, to_expr sthen, Some (to_expr selse))
  | Seq sims ->
      Seq (List2.map sims ~f:to_expr)
  | Unit ->
      Construct (I "()", None)

type err = NotImplemented of string [@@deriving show {with_path= false}]

let from_expr : Expr.t -> (t, err) Result.t =
  let open Result in
  let ( let* ) = ( >>= ) in

  let unpack : Pat.t -> (Id.t, err) Result.t = function
    | Var id ->
        return id
    | _ ->
        fail (NotImplemented "patterns")
  in

  let rec f = function
    | Expr.Id id ->
        return (Id id)
    | Const const ->
        return (Const const)
    | Fun (pats, expr) ->
        let* args =
          List.fold_right (List1.to_list pats) ~init:(return [])
            ~f:(fun pat acc ->
              let* acc = acc in
              let* arg = unpack pat in
              return (arg :: acc) )
        in

        let* sim = f expr in
        return (Fun (Nonrec, List1.of_list_exn args, sim))
    | Apply (expr1, exp2) ->
        let* sim1 = f expr1 in
        let* sim2 = f exp2 in
        return (Apply (sim1, sim2))
    | Let (Nonrec, bindings, expr) ->
        let* sim = f expr in
        List.fold_result (List1.to_list bindings) ~init:sim
          ~f:(fun acc {pat; expr= rhs} ->
            let* id = unpack pat in
            let* rhs = f rhs in
            return (Apply (Fun (Nonrec, List1.of_list_exn [id], acc), rhs)) )
    | Seq exprs ->
        let* sims =
          List.fold_right (List2.to_list exprs) ~init:(return [])
            ~f:(fun expr acc ->
              let* acc = acc in
              let* expr = f expr in
              return (expr :: acc) )
        in

        return (Seq (List2.of_list_exn sims))
    | If (econd, ethen, eelse) ->
        let* scond = f econd in
        let* sthen = f ethen in
        let* selse = Option.value_map eelse ~default:(return Unit) ~f in
        return (If (scond, sthen, selse))
    | Constraint (expr, _) ->
        f expr
    | Construct (I "()", _) ->
        return Unit
    | Let (Rec, _, _) ->
        fail (NotImplemented "recursive bindings")
    | Tuple _ ->
        fail (NotImplemented "tuples")
    | Function _ | Match _ ->
        fail (NotImplemented "pattern matching")
    | Construct _ ->
        fail (NotImplemented "constructors")
  in
  f

let from_structure (str : structure) : (t, err) Result.t =
  List.fold_right str ~init:Expr.unit
    ~f:(fun (item : StrItem.t) (acc : Expr.t) : Expr.t ->
      match item with
      | Type _ ->
          acc
      | Let (recf, bindings) ->
          Let (recf, bindings, acc)
      | Eval expr ->
          Seq (List2.of_list_exn [expr; acc]) )
  |> from_expr

let rec free : t -> IdSet.t = function
  | Id id ->
      IdSet.single id
  | Fun (recf, args, sim) ->
      let args = List1.to_list args in
      let bound =
        (match recf with Nonrec -> args | Rec id -> id :: args)
        |> IdSet.of_list
      in
      Set.diff (free sim) bound
  | Apply (sim1, sim2) ->
      Set.union (free sim1) (free sim2)
  | Seq sims ->
      List.fold (List2.to_list sims) ~init:IdSet.empty ~f:(fun acc sim ->
          Set.union acc (free sim) )
  | If (scond, sthen, selse) ->
      Set.union_list (module Id) [free scond; free sthen; free selse]
  | Const _ | Unit ->
      IdSet.empty
