[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

(* open! Base *)
open LAst
open LMisc

module String_set = struct
  include Set.Make (String)

  let pp ppf set =
    let open Format in
    fprintf ppf "{set| " ;
    iter (fprintf ppf "%s, ") set ;
    fprintf ppf "|set}"
end

module SS = String_set

let log fmt = Format.fprintf Format.std_formatter fmt

let standart_globals = SS.of_list ["+"; "="; "<"; "*"; "-"; "/"]

let without set1 set2 = SS.diff set1 set2

let get_vars_from_pattern, get_vars_from_patterns =
  let rec helper acc = function
    | Pat.Any | Pat.Const _ ->
        acc
    | Pat.Var (I id) ->
        SS.add id acc
    | Pat.Tuple patterns ->
        List.fold_left helper acc (List2.to_list patterns)
    | Pat.Or (l, r) ->
        SS.union (helper acc l) (helper acc r)
    | Pat.Construct (I id, Some pattern) ->
        let acc = SS.add id acc in
        helper acc pattern
    | Pat.Construct (I id, None) ->
        SS.add id acc
    | Pat.Constraint (pattern, _) ->
        helper acc pattern
  in
  (helper SS.empty, List.fold_left helper SS.empty)

let get_patterns_from_bindings (bindings : Expr.value_binding List.t) =
  List.fold_left
    (fun acc (binding : Expr.value_binding) -> binding.pat :: acc)
    [] bindings

let get_free_vars_from_expr =
  let rec get_free_bindings_vars (bindings : Expr.value_binding List.t) =
    let bindings_free_vars =
      List.fold_left
        (fun acc (binding : Expr.value_binding) ->
          without (helper acc binding.expr) (get_vars_from_pattern binding.pat)
          )
        SS.empty bindings
    in
    bindings_free_vars
  and get_free_cases_vars (cases : Expr.case List.t) =
    let cases_free_vars =
      List.fold_left
        (fun acc (case : Expr.case) ->
          without (helper acc case.right) (get_vars_from_pattern case.left) )
        SS.empty cases
    in
    cases_free_vars
  and helper acc = function
    | Expr.Id (I id) ->
        SS.add id acc
    | Expr.Const _ ->
        acc
    | Expr.Let (Rec, bindings, body) ->
        let bound_vars =
          get_vars_from_patterns
            (get_patterns_from_bindings (List1.to_list bindings))
        in
        let bindings_free_vars =
          (* case Rec and NonRec are the same *)
          without (get_free_bindings_vars (List1.to_list bindings)) bound_vars
        in
        let body_free_vars =
          without (helper bindings_free_vars body) bound_vars
        in
        body_free_vars
    | Expr.Let (Nonrec, bindings, body) ->
        let bound_vars =
          get_vars_from_patterns
            (get_patterns_from_bindings (List1.to_list bindings))
        in
        let bindings_free_vars =
          get_free_bindings_vars (List1.to_list bindings)
        in
        let body_free_vars =
          without (helper bindings_free_vars body) bound_vars
        in
        body_free_vars
    | Expr.Fun (patterns, body) ->
        let bound_vars = get_vars_from_patterns (List1.to_list patterns) in
        let body_free_vars = helper acc body in
        without body_free_vars bound_vars
    | Expr.Function cases ->
        get_free_cases_vars (List1.to_list cases)
    | Expr.Apply (l, r) ->
        helper (helper acc l) r
    | Expr.Match (match_expr, cases) ->
        let free_match_expr_vars = helper acc match_expr in
        let free_cases_vars = get_free_cases_vars (List1.to_list cases) in
        SS.union free_match_expr_vars free_cases_vars
    | Expr.Tuple exprs ->
        List.fold_left helper acc (List2.to_list exprs)
    | Expr.Construct (I id, expr) -> (
        let bound_vars = SS.singleton id in
        match expr with
        | Some expr ->
            without (helper acc expr) bound_vars
        | None ->
            acc )
    | Expr.If (condition, if_expr, else_expr) -> (
        let acc = helper acc condition in
        let acc = helper acc if_expr in
        match else_expr with Some expr -> helper acc expr | None -> acc )
    | Expr.Seq exprs ->
        List.fold_left helper acc (List2.to_list exprs)
    | Expr.Constraint (expr, _) ->
        helper acc expr
  in
  helper SS.empty

let free_vars_str = function
  | StrItem.Let (Rec, bindings) ->
      get_free_vars_from_expr (Expr.Let (Rec, bindings, Expr.Const (Int 0)))
  | StrItem.Let (Nonrec, bindings) ->
      get_free_vars_from_expr (Expr.Let (Nonrec, bindings, Expr.Const (Int 0)))
  | StrItem.Eval expr ->
      get_free_vars_from_expr expr
  | _ ->
      SS.empty



let run_free_vars program =
  let rec helper acc = function
    | [] ->
        SS.empty
    | hd :: tl ->
        SS.union (free_vars_str hd) (helper acc tl)
  in
  helper SS.empty program

(* let run_free_test s =
  match LParse.parse s with
  | None ->
      print_endline "syntax error"
  | Some str -> (
    match List.hd str with
    | StrItem.Eval expr ->
        let pat, expr = group_funs expr in
        PPrint.ToChannel.pretty 1. 40 stdout
          (LPrint.pp_expr (Expr.Fun (List1.of_list_exn pat, expr)))
    | _ ->
        print_endline "not a function" ) *)
(* 
let%expect_test _ =
  run_free_test {| fun y -> fun z -> fun x -> z + x + y|} ;
  [%expect {| fun y z x -> (+) ((+) z x) y |}] *)



let run_pp_test s =
  match LParse.parse s with
  | None ->
      print_endline "syntax error"
  | Some str ->
      pp_structure Format.std_formatter str

let run_get_pattern pattern =
  let vars = get_vars_from_pattern pattern in
  SS.pp Format.std_formatter vars

let%expect_test _ =
  let lst2 = List2.of_list_exn @@ [Pat.Var (I "a"); Pat.Var (I "b")] in
  run_get_pattern (Pat.Tuple lst2) ;
  [%expect {| {set| a, b, |set} |}]

let%expect_test _ =
  run_pp_test {|fun y -> fun z -> fun x -> z + x + y|} ;
  [%expect
    {|
    [(Eval
        (Fun (((Var (I "y")), []),
           (Fun (((Var (I "z")), []),
              (Fun (((Var (I "x")), []),
                 (Apply (
                    (Apply ((Id (I "+")),
                       (Apply ((Apply ((Id (I "+")), (Id (I "z")))), (Id (I "x"))
                          ))
                       )),
                    (Id (I "y"))))
                 ))
              ))
           )))
      ]
    |}]

(* let find_args =
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
         pat :: acc ) *)
