[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Stdio

open LMiddle

module Format = Stdlib.Format

type err = ParseError | SimplError of MSimpl.err

let run' (ir : [`Simpl | `SimplOpt | `CLess | `Anf]) s =
  let globals = IdSet.of_list [I "+"; I "-"; I "*"; I "<"] in

  let open Result in
  let ( let* ) = ( >>= ) in

  let* structure =
    LParse.parse s |> Option.value_map ~default:(fail ParseError) ~f:return
  in

  let* sim =
    MSimpl.from_structure structure |> map_error ~f:(fun err -> SimplError err)
  in
  let opt = MOpt.opt sim in
  let cls = MCLess.from_simpl globals opt in

  let print = PPrint.ToChannel.pretty 1. 50 stdout in
  ( match ir with
  | `Simpl ->
      LPrint.pp_expr (MSimpl.to_expr sim) |> print
  | `SimplOpt ->
      LPrint.pp_expr (MSimpl.to_expr opt) |> print
  | `CLess ->
      LPrint.pp_structure (MCLess.to_structure cls) |> print
  | `Anf ->
      let anf = MAnf.from_cless cls in
      LPrint.pp_structure (MAnf.to_structure anf) |> print ) ;

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
  [%expect {| (fun a -> (fun b -> b) 2) 1 |}]

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
    ) 2; (+) a b
    |}]

let%expect_test _ =
  run `Simpl {| let x = 1;; let y = 2;; x + y|} ;
  [%expect {| (fun x -> (fun y -> (+) x y) 2) 1 |}]

let%expect_test _ =
  run `SimplOpt {| let x = 1;; let y = 2;; x + y|} ;
  [%expect {| (fun x y -> (+) x y) 1 2 |}]

let%expect_test _ =
  run `SimplOpt {| let x = 1 + 2 and y = 3 and z = 4 in x - z + y |} ;
  [%expect {| (fun z y x -> (+) ((-) x z) y) 4 ((+) 1 2) 3 |}]

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
       f 1 1; (fun f -> f 2 2) (fun x y -> (-) x y)
    ) (fun x y -> (+) x y)
    |}]

let%expect_test _ =
  run `CLess {|(fun x -> fun y -> fun z -> x - 1 + y + z) 1 2 3 |} ;
  [%expect
    {|
    let F0 = fun x y z -> (+) ((+) ((-) x 1) y) z;;
    F0 1 2 3
    |}]

let%expect_test _ =
  run `CLess
    {| let f x y =
       let x z = y + z in
       let y z = x 1 + z in
        x 1 + y 2;; f 5 10 |} ;
  [%expect
    {|
    let F0 = fun y z -> (+) y z;;
    let F1 = fun x z -> (+) (x 1) z;;
    let F2 = fun x y -> (+) (x 1) (y 2);;
    let F3 = fun x -> F2 x (F1 x);;
    let F4 = fun x y -> F3 (F0 y);;
    let F5 = fun f -> f 5 10;;
    F5 F4
    |}]

let%expect_test _ =
  run `CLess {| let x = 1;; let y = 2;; x + y|} ;
  [%expect {|
    let F0 = fun x y -> (+) x y;;
    F0 1 2
    |}]

let%expect_test _ =
  run `CLess
    {| let rec fact n = if n < 2 then 1 else n * fact (n-1) in fact 5 |} ;
  [%expect
    {|
    let rec F0 =
      fun n ->
        if (<) n 2 then 1 else (*) n (F0 ((-) n 1));;
    let F1 = fun fact -> fact 5;;
    F1 F0
    |}]

let%expect_test _ =
  run `Anf {|let a = 1|} ;
  [%expect {|
    let F0 = fun a -> ();;
    F0 1
    |}]

let%expect_test _ =
  run `Anf {|let f x =  x + x - 10 in f 5|} ;
  [%expect
    {|
    let F0 = fun x -> let V0 = (+) x x in (-) V0 10;;
    let F1 = fun f -> f 5;;
    F1 F0
    |}]

let%expect_test _ =
  run `Anf {| f x y z d (e + 2) |} ;
  [%expect {| let V0 = (+) e 2 in f x y z d V0 |}]

let%expect_test _ =
  run `Anf {| let rec fact n = if n < 2 then 1 else n * fact (n-1) in fact 5 |} ;
  [%expect
    {|
    let rec F0 =
      fun n ->
        let V0 = (<) n 2 in
        if V0
        then 1
        else
          let V1 = (-) n 1 in
          let V2 = F0 V1 in (*) n V2;;
    let F1 = fun fact -> fact 5;;
    F1 F0
    |}]
