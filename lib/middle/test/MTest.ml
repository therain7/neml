[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Stdio

open LMiddle

module Format = Stdlib.Format

let run s =
  match LParse.parse s with
  | None ->
      print_endline "syntax error"
  | Some str -> (
    match MSimpl.from_structure str with
    | Error err ->
        MSimpl.pp_err Format.std_formatter err
    | Ok sim ->
        PPrint.ToChannel.pretty 1. 40 stdout
          (LPrint.pp_expr (MSimpl.to_expr sim)) )

let%expect_test _ = run {| let a = 1 |} ; [%expect {| (fun a -> ()) 1 |}]

let%expect_test _ =
  run {| let 1 = 2 |} ; [%expect {| (NotImplemented "patterns") |}]

let%expect_test _ =
  run {| let a = 1 in let b = 2 in b |} ;
  [%expect {| (fun a -> (fun b -> b) 2) 1; () |}]

let%expect_test _ =
  run
    {| let a = 1 and b = 2 in
       let f x y = x + y in
       f 5;;

       type foo = Foo of string;;
       a + b |} ;
  [%expect
    {|
    (
     fun b ->
       (
        fun a ->
          (fun f -> f 5)
            (fun x y -> (+) x y)
       ) 1
    ) 2; ((+) a b; ())
    |}]

let%expect_test _ =
  run
    {| let f x y = x + y;;
       f 1 1;;
       let f x y = x - y;;
       f 2 2 |} ;
  [%expect
    {|
    (
     fun f ->
       f 1 1;
       (fun f -> f 2 2; ())
         (fun x y -> (-) x y)
    ) (fun x y -> (+) x y)
    |}]
