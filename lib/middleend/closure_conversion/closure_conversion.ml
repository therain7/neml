[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open Stdio
open LMisc
open LAst
open Common

let rec cc_expr global_env env_map = function
  | Expr.Id (I id) as orig -> (
    match Map.find env_map id with
    | Some vals_to_apply ->
        List.fold vals_to_apply ~init:orig ~f:(fun orig val_to_apply ->
            Expr.Apply (orig, Expr.Id (I val_to_apply)) )
    | None ->
        orig )
  | Expr.Const _ as orig ->
      orig
  | Expr.Let (Rec, bindings, body) as orig ->
      orig
  | Expr.Let (Nonrec, bindings, body) ->
      let bindings_list = List1.to_list bindings in
      let first_binding = List.hd_exn bindings_list in
      let vars_to_add = Set.elements (find_args first_binding.expr) in
      let new_value_binding =
        match vars_to_add with
        | [] ->
            Expr.
              { pat= first_binding.pat
              ; expr= cc_expr global_env env_map first_binding.expr }
        | _ -> (
            let pattern_to_add =
              List.fold vars_to_add ~init:[] ~f:(fun acc var ->
                  Pat.Var (I var) :: acc )
            in
            let pattern_to_add = List.rev pattern_to_add in
            match first_binding.expr with
            | Expr.Fun (patterns, inner_body) ->
                let all_patterns =
                  List.append pattern_to_add (List1.to_list patterns)
                in
                Expr.
                  { pat= first_binding.pat
                  ; expr=
                      Expr.Fun
                        ( List1.of_list_exn all_patterns
                        , cc_expr global_env env_map inner_body ) }
            | _ ->
                let new_expr = cc_expr global_env env_map first_binding.expr in
                Expr.{pat= first_binding.pat; expr= new_expr} )
      in
      let pattern_args = Set.elements @@ get_pattern_arg first_binding.pat in
      let new_env_map =
        List.fold pattern_args ~init:env_map ~f:(fun acc new_var ->
            Map.add_exn acc ~key:new_var ~data:vars_to_add )
      in
      let new_global_env = List.fold pattern_args ~init:global_env ~f:Set.add in
      let new_body = cc_expr new_global_env new_env_map body in
      Expr.Let (Nonrec, List1.of_list_exn [new_value_binding], new_body)
  | Expr.Fun (args, body) as orig -> (
      let pattern_list = List1.to_list args in
      let patterns_args = get_patterns_arg_list pattern_list in
      let free_vars = Set.diff (find_args body) patterns_args in
      let free_vars = Set.diff free_vars global_env in
      let closed_args =
        Set.fold free_vars ~init:pattern_list ~f:(fun acc id ->
            Pat.Var (I id) :: acc )
      in
      let body = cc_expr global_env env_map body in
      match closed_args with
      | [] ->
          orig
      | hd :: tl ->
          Set.fold_right free_vars
            ~init:(Expr.Fun (List1.of_list_exn (hd :: tl), body))
            ~f:(fun var acc -> Expr.Apply (acc, Expr.Id (I var))) )
  | Expr.Function cases ->
      let new_cases =
        List.fold (List1.to_list cases) ~init:[] ~f:(fun acc case ->
            let pattern_args = Set.elements @@ get_pattern_arg case.left in
            let new_env_map =
              List.fold pattern_args ~init:env_map ~f:(fun acc new_var ->
                  Map.remove acc new_var )
            in
            let new_case_expr = cc_expr global_env new_env_map case.right in
            Expr.{left= case.left; right= new_case_expr} :: acc )
      in
      Expr.Function (List1.of_list_exn @@ List.rev new_cases)
  | Expr.Apply (func, arg) ->
      let func = cc_expr global_env env_map func in
      let arg = cc_expr global_env env_map arg in
      Expr.Apply (func, arg)
  | Expr.Match (expr, cases) ->
      let new_expr = cc_expr global_env env_map expr in
      let new_cases =
        List.fold (List1.to_list cases) ~init:[] ~f:(fun acc case ->
            let pattern_args = Set.elements @@ get_pattern_arg case.left in
            let new_env_map =
              List.fold pattern_args ~init:env_map ~f:(fun acc new_var ->
                  Map.remove acc new_var )
            in
            let new_case_expr = cc_expr global_env new_env_map case.right in
            Expr.{left= case.left; right= new_case_expr} :: acc )
      in
      Expr.Match (new_expr, List1.of_list_exn @@ List.rev new_cases)
  | Expr.If (condition, if_expr, else_expr) ->
      let new_condition = cc_expr global_env env_map condition in
      let new_if_expr = cc_expr global_env env_map if_expr in
      let new_else_expr =
        Option.map ~f:(cc_expr global_env env_map) else_expr
      in
      Expr.If (new_condition, new_if_expr, new_else_expr)
  | Expr.Construct _ ->
      Expr.Id (I "Don't have ideas")
  | Expr.Tuple expr_list2 ->
      let new_expr_list =
        List.map (List2.to_list expr_list2) ~f:(cc_expr global_env env_map)
      in
      Expr.Tuple (List2.of_list_exn new_expr_list)
  | Expr.Seq expr_list2 ->
      let new_expr_list =
        List.map (List2.to_list expr_list2) ~f:(cc_expr global_env env_map)
      in
      Expr.Seq (List2.of_list_exn new_expr_list)
  | Expr.Constraint (expr, t) ->
      let new_expr = cc_expr global_env env_map expr in
      Expr.Constraint (new_expr, t)

let cc_structure_item global_env = function
  | StrItem.Type _ as orig ->
      (global_env, orig)
  | StrItem.Eval expr ->
      (global_env, StrItem.Eval (cc_expr global_env empty_map expr))
  | StrItem.Let (Rec, bindings) ->
      let pattern_list = get_pattern_list_from_bindings bindings in
      let pattern_args = Set.elements @@ get_patterns_arg_list pattern_list in
      let global_env = List.fold pattern_args ~init:global_env ~f:Set.add in
      let bindings =
        List.fold (List1.to_list bindings) ~init:[] ~f:(fun acc binding ->
            match binding.expr with
            | Expr.Fun (patterns, body) ->
                let expr = cc_expr global_env empty_map body in
                Expr.{pat= binding.pat; expr= Expr.Fun (patterns, expr)} :: acc
            | _ ->
                let expr = cc_expr global_env empty_map binding.expr in
                Expr.{pat= binding.pat; expr} :: acc )
      in
      (global_env, StrItem.Let (Rec, List1.of_list_exn bindings))
  | StrItem.Let (Nonrec, bindings) ->
      let bindings =
        List.fold (List1.to_list bindings) ~init:[] ~f:(fun acc binding ->
            let pat = binding.pat in
            let expr = cc_expr global_env empty_map binding.expr in
            Expr.{pat; expr} :: acc )
      in
      (global_env, StrItem.Let (Nonrec, List1.of_list_exn bindings))

let run_closure_conversion program =
  let rec helper global_env = function
    | [] ->
        []
    | hd :: tl ->
        let new_global_env, new_str = cc_structure_item global_env hd in
        new_str :: helper new_global_env tl
  in
  helper empty_global_scope program

let run_cc s =
  match LParse.parse s with
  | None ->
      print_endline "syntax error"
  | Some str ->
      let str = run_closure_conversion str in
      PPrint.ToChannel.pretty 1. 40 stdout (LPrint.pp_structure str)

let run_cc_pp s =
  match LParse.parse s with
  | None ->
      print_endline "syntax error"
  | Some str ->
      let str = run_closure_conversion str in
      pp_structure Format.std_formatter str

let%expect_test _ =
  run_cc_pp {|let (c, d) = (1, 2) in fun a -> a + c + d|} ;
  [%expect
    {|
       let c, d = 1, 2 in
       (fun (+) a -> (+) ((+) a c) d) (+)
       |}]

let%expect_test _ =
  run_cc {|let (c, d) = (1, 2) in fun a -> a + c + d|} ;
  [%expect
    {|
       let c, d = 1, 2 in
       (fun (+) a -> (+) ((+) a c) d) (+)
       |}]

let%expect_test _ =
  run_cc {| let a = fun x -> x + 2|} ;
  [%expect {|
       let a = (fun (+) x -> (+) x 2) (+)
       |}]

let%expect_test _ =
  run_cc {| let a x = x + 2 |} ;
  [%expect {|
       let a = (fun (+) x -> (+) x 2) (+)
       |}]
