[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open Stdio
open LMisc
open LAst
open Common

let emptyEnv = Map.empty (module String)

let empty = Set.empty (module String)

let rec get_identifiers = function
  | Pat.Any | Pat.Const _ ->
      empty
  | Pat.Var (I id) ->
      Set.singleton (module String) id
  | Pat.Tuple patterns ->
      List.fold (List2.to_list patterns) ~init:empty ~f:(fun acc pat ->
          Set.union acc (get_identifiers pat) )
  | Pat.Or (l, r) ->
      Set.union (get_identifiers l) (get_identifiers r)
  | Pat.Construct _ ->
      empty
  | Pat.Constraint (pat, _) ->
      get_identifiers pat

let emptyEnv = Map.empty (module String)

let empty = Set.empty (module String)

let cc_structure_item env global_scope = function
  | StrItem.Eval expr ->
      (env, StrItem.Eval expr)
  | StrItem.Let (Rec, bindings) ->
      (env, StrItem.Let (Rec, bindings))
  | StrItem.Let (Nonrec, bindings) ->
      (* let idents = get_identifiers bindings in

         (env, )
         List.map (List1.to_list bindings)
           ~f:(fun {pat; expr} ->
             group @@ nest 2 @@ ppat pat ^^ group (string " =" ^/^ pexpr expr) ) *)
      (env, StrItem.Let (Nonrec, bindings))
  | StrItem.Type {id; params; variants} ->
      (env, StrItem.Type {id; params; variants})

let run_closure_conversion program =
  let rec helper env = function
    | [] ->
        []
    | hd :: tl ->
        let cur_env, cur_ast = cc_structure_item env empty hd in
        cur_ast :: helper cur_env tl
  in
  helper emptyEnv program

let run_cc s =
  match LParse.parse s with
  | None ->
      print_endline "syntax error"
  | Some str ->
      let str = run_closure_conversion str in
      PPrint.ToChannel.pretty 1. 40 stdout (LPrint.pp_structure str)

let%expect_test _ =
  run_cc {|let (c, d) = (1, 2) in fun a -> a + c + d|} ;
  [%expect {|
    let c, d = 1, 2 in
    fun a -> (+) ((+) a c) d
    |}]

(* должно быть  (((fun c d a -> ((+ ((+ a) c)) d)) c) d) *)
