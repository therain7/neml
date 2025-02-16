[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open LAst
open LMisc

let empty_global_scope =
  let init = Base.Set.empty (module String) in
  let init = Set.add init "( * )" in
  let init = Set.add init "( / )" in
  let init = Set.add init "( + )" in
  let init = Set.add init "( - )" in
  let init = Set.add init "( = )" in
  let init = Set.add init "( == )" in
  let init = Set.add init "( <> )" in
  let init = Set.add init "( != )" in
  let init = Set.add init "( < )" in
  let init = Set.add init "( <= )" in
  let init = Set.add init "( > )" in
  let init = Set.add init "( >= )" in
  let init = Set.add init "( && )" in
  let init = Set.add init "( || )" in
  let init = Set.add init "print_int" in
  let init = Set.add init "print_string" in
  let init = Set.add init "( ~+ )" in
  let init = Set.add init "( ~- )" in
  init

let empty_set = Base.Set.empty (module Base.String)

let empty_map = Map.empty (module String)

let rec get_pattern_arg = function
  | Pat.Any | Pat.Const _ ->
      empty_set
  | Pat.Var (I id) ->
      Set.singleton (module String) id
  | Pat.Tuple patterns ->
      List.fold (List2.to_list patterns) ~init:empty_set ~f:(fun acc pat ->
          Set.union acc (get_pattern_arg pat) )
  | Pat.Or (l, r) ->
      Set.union (get_pattern_arg l) (get_pattern_arg r)
  | Pat.Construct _ ->
      empty_set
  | Pat.Constraint (pat, _) ->
      get_pattern_arg pat

let get_patterns_arg_list patterns =
  List.fold patterns ~init:empty_set ~f:(fun acc pat ->
      Set.union acc (get_pattern_arg pat) )

let find_args =
  let rec find_args_in_binding acc (binding : Expr.value_binding) =
    match binding with
    | {pat; expr} ->
        let pat_args = get_pattern_arg pat in
        Set.diff (helper acc expr) pat_args
  and helper acc = function
    | Expr.Id (I id) ->
        Set.add acc id
    | Expr.Const _ ->
        acc
    | Expr.Let (_, bindings, body) ->
        let bindings_list = List1.to_list bindings in
        let acc = List.fold bindings_list ~init:acc ~f:find_args_in_binding in
        helper acc body
    | Expr.Fun (patterns, body) ->
        let pat_args = get_patterns_arg_list (List1.to_list patterns) in
        Set.diff (helper acc body) pat_args
    | Expr.Function cases ->
        List.fold (List1.to_list cases) ~init:acc ~f:(fun acc case ->
            Set.diff (helper acc case.right) (get_pattern_arg case.left) )
    | Expr.Apply (l, r) ->
        helper (helper acc l) r
    | Expr.Match (match_expr, cases) ->
        let acc = helper acc match_expr in
        List.fold (List1.to_list cases) ~init:acc ~f:(fun acc case ->
            Set.diff (helper acc case.right) (get_pattern_arg case.left) )
    | Expr.Tuple exprs ->
        List.fold (List2.to_list exprs) ~init:acc ~f:helper
    | Expr.Construct (I id, expr) -> (
        let acc = Set.add acc id in
        match expr with Some expr -> helper acc expr | None -> acc )
    | Expr.If (condition, if_expr, else_expr) -> (
        let acc = helper acc condition in
        let acc = helper acc if_expr in
        match else_expr with Some expr -> helper acc expr | None -> acc )
    | Expr.Seq exprs ->
        List.fold (List2.to_list exprs) ~init:acc ~f:helper
    | Expr.Constraint (expr, _) ->
        helper acc expr
  in
  helper empty_set

let get_pattern_list_from_bindings (bindings : Expr.value_binding List1.t) =
  List.fold (List1.to_list bindings) ~init:[] ~f:(fun acc binding ->
      let pat = binding.pat in
      pat :: acc )
