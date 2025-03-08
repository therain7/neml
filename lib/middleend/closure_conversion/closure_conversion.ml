[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open Stdio
open LMisc
open LAst
open Common
open PPrint

(*
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

let run_pp_test s =
  match LParse.parse s with
  | None ->
      print_endline "syntax error"
  | Some str ->
      pp_structure Format.std_formatter str


let%expect_test _ =
  run_cc_pp {|let (c, d) = (1, 2) in fun a -> a + c + d|} ;
  [%expect
    {|
    [(Eval
        (Let (Nonrec,
           ({ pat = (Tuple ((Var (I "c")), (Var (I "d")), []));
              expr = (Tuple ((Const (Int 1)), (Const (Int 2)), [])) },
            []),
           (Apply (
              (Fun (((Var (I "+")), [(Var (I "a"))]),
                 (Apply (
                    (Apply ((Id (I "+")),
                       (Apply ((Apply ((Id (I "+")), (Id (I "a")))), (Id (I "c"))
                          ))
                       )),
                    (Id (I "d"))))
                 )),
              (Id (I "+"))))
           )))
      ]
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
  run_pp_test {| let a x y = x + y |} ;
  [%expect {|
    [(Let (Nonrec,
        ({ pat = (Var (I "a"));
           expr =
           (Fun (((Var (I "x")), [(Var (I "y"))]),
              (Apply ((Apply ((Id (I "+")), (Id (I "x")))), (Id (I "y"))))))
           },
         [])
        ))
      ]
    |}]

(* let a x = ((fun x y -> (y  +  x)) x);; *)

let%expect_test _ =
  run_cc_pp {| let a x = x + 2 |} ;
  [%expect {|
    [(Let (Nonrec,
        ({ pat = (Var (I "a"));
           expr =
           (Apply (
              (Fun (((Var (I "+")), [(Var (I "x"))]),
                 (Apply ((Apply ((Id (I "+")), (Id (I "x")))), (Const (Int 2))))
                 )),
              (Id (I "+"))))
           },
         [])
        ))
      ]
    |}] *)

let gen_fresh_name =
  let last = ref 0 in
  fun () ->
    incr last ;
    Format.sprintf "fresh_%d" !last

let group_funs =
  let rec helper acc = function
    | Expr.Fun (patterns, body) ->
        let pat_list = List1.to_list patterns in
        helper (acc @ pat_list) body
    | not_a_fun ->
        (acc, not_a_fun)
  in
  helper []

let convert_structure global_env =
  let open Monands.Store in
  let save : StrItem.t -> (StrItem.t list, unit) t =
   fun x ->
    let* old = get in
    put (x :: old)
  in
  let is_inner_fun = function Expr.Fun _ -> true | _ -> false in
  let rec helper global_env expr : (StrItem.t list, Expr.t) t =
    match expr with
    | (Expr.Const _ | Expr.Id _) as orig ->
        return orig
    | Expr.Fun (_, _) as orig -> (
      match group_funs orig with
      | pat_list, body ->
          let free_vars =
            SS.diff
              (get_free_vars_from_expr
                 (Expr.Fun (List1.of_list_exn pat_list, body)) )
              global_env
          in
          let new_f_name = gen_fresh_name () in
          if SS.is_empty free_vars then
            let* new_body =
              helper
                (SS.union (get_vars_from_patterns pat_list) global_env)
                body
            in
            let* () =
              save
                (StrItem.Let
                   ( Nonrec
                   , List1.of_list_exn
                       [ Expr.
                           { pat= Pat.Var (I new_f_name)
                           ; expr=
                               Expr.Fun (List1.of_list_exn pat_list, new_body)
                           } ] ) )
            in

            return (Expr.Id (I new_f_name))
          else
            let free_vars_list = SS.elements free_vars in
            let* new_body =
              helper
                (SS.union (get_vars_from_patterns pat_list) global_env)
                body
            in
            let new_pattern_list =
              List.fold_left ~init:pat_list
                ~f:(fun acc var -> Pat.Var (I var) :: acc)
                free_vars_list
            in
            let new_fun_without_free_args =
              Expr.Fun (List1.of_list_exn new_pattern_list, new_body)
            in
            let* () =
              save
                (StrItem.Let
                   ( Nonrec
                   , List1.of_list_exn
                       [ Expr.
                           { pat= Pat.Var (I new_f_name)
                           ; expr= new_fun_without_free_args } ] ) )
            in
            let new_body_without_closure =
              List.fold_left ~init:(Expr.Id (I new_f_name))
                ~f:(fun acc var -> Expr.Apply (acc, Expr.Id (I var)))
                (List.rev free_vars_list)
            in
            return new_body_without_closure )
    | Expr.Let (Nonrec, bindings, inner_body) ->
        failwith "Not implemented"
    | Expr.Apply (func, arg) ->
        let* new_func = helper global_env func in
        let* new_arg = helper global_env arg in
        return (Expr.Apply (new_func, new_arg))
    | _ ->
        failwith "Not implemented here"
  in

  function
  | StrItem.Let (is_rec, (bindings : Expr.value_binding List1.t)) -> (
    match List1.to_list bindings with
    | {pat; expr} :: tl -> (
      match pat with
      | Pat.Var (I id) ->
          let pat_list, body = group_funs expr in
          let new_global_env = SS.add id global_env in
          let saved_str_items, last_expr =
            Monands.Store.run (helper new_global_env body) []
          in
          (* List.iter pat_list ~f:(fun var -> PPrint.ToChannel.pretty 1. 40 stdout (LPrint.pp_pat var);); *)
          (* PPrint.ToChannel.pretty 1. 40 stdout (LPrint.pp_expr body); *)
          let new_bindings_list =
            List1.of_list_exn
              [ Expr.
                  {pat; expr= Expr.Fun (List1.of_list_exn pat_list, last_expr)}
              ]
          in

          let new_str_item = StrItem.Let (is_rec, new_bindings_list) in
          (* PPrint.ToChannel.pretty 1. 40 stdout (LPrint.pp_stritem new_str_item); *)
          saved_str_items @ [new_str_item]
      | _ ->
          failwith "not implemented 1" )
    | [] ->
        failwith "HI" )
  | StrItem.Eval expr ->
      let pat_list, body = group_funs expr in
      let saved_str_items, last_expr =
        Monands.Store.run (helper global_env body) []
      in
      let new_str_item =
        StrItem.Eval (Expr.Fun (List1.of_list_exn pat_list, last_expr))
      in
      saved_str_items @ [new_str_item]
  | _ ->
      failwith "LALA"

(* let convert_structure global_env = function
   | StrItem.Let (Rec, bindings) as str_item ->
       str_item
   | StrItem.Let (Nonrec, bindings) as str_item ->
       let new_bindings = conve
   | StrItem.Eval _ ->
       failwith "not implemented"
   | StrItem.Type _ ->
       failwith "not implemented" *)

let convert structure =
  let init = (standart_globals, []) in
  List.fold_left ~init
    ~f:(fun (glob, ans) str_item ->
      let new_str_item = convert_structure glob str_item in
      let bound_vars_from_str_item =
        let helper acc = function
          | StrItem.Let (_, bindings) ->
              SS.elements
                (get_vars_from_patterns
                   (get_patterns_from_bindings (List1.to_list bindings)) )
              @ acc
          | StrItem.Eval _ ->
              acc
          | StrItem.Type _ ->
              acc
        in
        List.fold_left ~f:helper ~init:[] new_str_item
      in
      let new_global_env =
        List.fold_left ~init:glob
          ~f:(fun glob var -> SS.add var glob)
          bound_vars_from_str_item
      in
      (new_global_env, new_str_item @ ans) )
    structure
  |> snd

let run_pp_test s =
  match LParse.parse s with
  | None ->
      print_endline "syntax error"
  | Some str ->
      PPrint.ToChannel.pretty 1. 40 stdout (LPrint.pp_structure str) ;

      PPrint.ToChannel.pretty 1. 40 stdout (PPrint.break 1) ;
      let str = convert str in
      PPrint.ToChannel.pretty 1. 40 stdout (LPrint.pp_structure str)
(* pp_structure Format.std_formatter str *)

let%expect_test _ =
  run_pp_test {|let add x z = fun y -> x + y + z|} ;
  [%expect
    {|
    let add =
      fun x z -> fun y -> (+) ((+) x y) z
    let add = fun x z y -> (+) ((+) x y) z
    |}]
