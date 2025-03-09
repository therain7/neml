[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Stdio

open LMiddle

module Format = Stdlib.Format

type err = ParseError | SimplError of MSimpl.err

let run' (ir : [`Simpl | `SimplOpt | `CLess]) s =
  let globals = IdSet.of_list [I "+"; I "-"] in

  let open Result in
  let ( let* ) = ( >>= ) in

  let* structure =
    LParse.parse s |> Option.value_map ~default:(fail ParseError) ~f:return
  in

  let* sim =
    MSimpl.from_structure structure |> map_error ~f:(fun err -> SimplError err)
  in
  let opt = MOpt.opt sim in

  let print = PPrint.ToChannel.pretty 1. 50 stdout in
  ( match ir with
  | `Simpl ->
      LPrint.pp_expr (MSimpl.to_expr sim) |> print
  | `SimplOpt ->
      LPrint.pp_expr (MSimpl.to_expr opt) |> print
  | `CLess ->
      let cls = MCLess.from_simpl globals opt in
      LPrint.pp_structure (MCLess.to_structure cls) |> print ) ;

  return ()

let run ir s =
  match run' ir s with
  | Error ParseError ->
      print_endline "syntax error"
  | Error (SimplError err) ->
      MSimpl.pp_err Format.std_formatter err
  | Ok _ ->
      ()

let%expect_test _ = run `Simpl {| let a = 1 |} ; [%expect {| (fun a -> ()) 1 |}]

let%expect_test _ =
  run `Simpl {| let 1 = 2 |} ; [%expect {| (NotImplemented "patterns") |}]

let%expect_test _ =
  run `Simpl {| let a = 1 in let b = 2 in b |} ;
  [%expect {| (fun a -> (fun b -> b) 2) 1; () |}]

let%expect_test _ =
  run `Simpl
    {| let a = 1 and b = 2 in
       let f x y = x + y in
       f 5;;

       type foo = Foo of string;;
       a + b |} ;
  [%expect
    {|
    (
     fun b ->
       (fun a -> (fun f -> f 5) (fun x y -> (+) x y))
         1
    ) 2; ((+) a b; ())
    |}]

let%expect_test _ =
  run `Simpl {| let x = 1;; let y = 2;; x + y|} ;
  [%expect {| (fun x -> (fun y -> (+) x y; ()) 2) 1 |}]

let%expect_test _ =
  run `SimplOpt {| let x = 1;; let y = 2;; x + y|} ;
  [%expect {| (fun x y -> (+) x y; ()) 1 2 |}]

let%expect_test _ =
  run `SimplOpt {| let x = 1 + 2 and y = 3 and z = 4 in x - z + y |} ;
  [%expect {| (fun z y x -> (+) ((-) x z) y) 4 ((+) 1 2) 3; () |}]

let%expect_test _ =
  run `Simpl
    {| let f x y = x + y;;
       f 1 1;;
       let f x y = x - y;;
       f 2 2 |} ;
  [%expect
    {|
    (
     fun f ->
       f 1 1;
       (fun f -> f 2 2; ()) (fun x y -> (-) x y)
    ) (fun x y -> (+) x y)
    |}]

let%expect_test _ =
  run `CLess {|(fun x -> fun y -> fun z -> x - 1 + y + z) 1 2 3 |} ;
  [%expect
    {|
    let f0 = fun x y z -> (+) ((+) ((-) x 1) y) z;;
    f0 1 2 3; ()
    |}]

let%expect_test _ =
  run `CLess
    {| let f x y =
       let x z = y + z in
       let y z = x 1 + z in
        x 1 + y 2;; f 5 10 |} ;
  [%expect
    {|
    let f0 = fun y z -> (+) y z;;
    let f1 = fun x z -> (+) (x 1) z;;
    let f2 = fun x y -> (+) (x 1) (y 2);;
    let f3 = fun x -> f2 x (f1 x);;
    let f4 = fun x y -> f3 (f0 y);;
    let f5 = fun f -> f 5 10; ();;
    f5 f4
    |}]

let%expect_test _ =
  run `CLess {| let x = 1;; let y = 2;; x + y|} ;
  [%expect {|
    let f0 = fun x y -> (+) x y; ();;
    f0 1 2
    |}]
