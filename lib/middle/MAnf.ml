[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LMisc
open LAst

open MCommon

(** ANF IR *)

type imm = Id of Id.t | Const of Const.t | Unit

type cmplx =
  | Imm of imm
  | Apply of imm * imm
  | If of imm * imm * imm
  | Seq of imm List2.t

type anf = Let of Id.t * cmplx * anf | Cmplx of cmplx

type def = anf FuncDef.t
type t = def list * anf

let imm_to_expr : imm -> Expr.t = function
  | Id id ->
      Id id
  | Const const ->
      Const const
  | Unit ->
      Expr.unit

let cmplx_to_expr : cmplx -> Expr.t = function
  | Imm imm ->
      imm_to_expr imm
  | Apply (imm1, imm2) ->
      Apply (imm_to_expr imm1, imm_to_expr imm2)
  | If (icond, ithen, ielse) ->
      If (imm_to_expr icond, imm_to_expr ithen, Some (imm_to_expr ielse))
  | Seq imms ->
      Seq (List2.map imms ~f:imm_to_expr)

let rec to_expr : anf -> Expr.t = function
  | Let (id, cmplx, anf) ->
      Let
        ( Nonrec
        , [Expr.{pat= Var id; expr= cmplx_to_expr cmplx}] |> List1.of_list_exn
        , to_expr anf )
  | Cmplx cmplx ->
      cmplx_to_expr cmplx

let to_structure ((defs, cl) : t) : structure =
  List.fold_right defs
    ~init:[Eval (to_expr cl)]
    ~f:(fun def acc -> FuncDef.to_stritem to_expr def :: acc)

let from_cl (cl : MCLess.cl) : anf =
  let cnt = ref (-1) in
  let fresh () =
    cnt := !cnt + 1 ;
    Id.I ("__v" ^ Int.to_string !cnt)
  in

  let ( let* ) = ( @@ ) in
  let rec f (cl : MCLess.cl) (k : imm -> anf) =
    match cl with
    | Id id ->
        k (Id id)
    | Const const ->
        k (Const const)
    | Unit ->
        k Unit
    | Apply (c1, c2) ->
        let* imm1 = f c1 in
        let* imm2 = f c2 in
        let id = fresh () in
        Let (id, Apply (imm1, imm2), k (Id id))
    | If (_, _, _) | Seq _ ->
        assert false
  in
  f cl (fun imm -> Cmplx (Imm imm))

let from_cless ((defs, cls) : MCLess.t) : t =
  let defs =
    List.fold_right defs ~init:[] ~f:(fun (Func def) acc : def list ->
        Func {def with body= from_cl def.body} :: acc )
  in
  (defs, from_cl cls)
