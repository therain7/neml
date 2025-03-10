[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open MTestRun

let%expect_test _ =
  run `SimplOpt {| let 1 = 2 |} ;
  [%expect {| (NotImplemented "patterns") |}]

let code = {| let a = 1 |}

let%expect_test _ = run `SimplOpt code ; [%expect {| (fun a -> ()) 1 |}]

let%expect_test _ =
  run `CLess code ; [%expect {|
    let F0 = fun a -> ();;
    F0 1
    |}]

let code = {| let a = 1 in let b = 2 in b |}

let%expect_test _ = run `SimplOpt code ; [%expect {| (fun a b -> b) 1 2 |}]

let%expect_test _ =
  run `CLess code ; [%expect {|
    let F0 = fun a b -> b;;
    F0 1 2
    |}]

let code =
  {| let a = 1 and b = 2;;
     let f x y = x + y in
     f 5;;
     type foo = Foo of string;;
     a + b |}

let%expect_test _ =
  run `SimplOpt code ;
  [%expect
    {|
    (
     fun b a ->
       (fun f -> f 5) (fun x y -> (+) x y); (+) a b
    ) 2 1
    |}]

let%expect_test _ =
  run `CLess code ;
  [%expect
    {|
    let F0 = fun x y -> (+) x y;;
    let F1 = fun f -> f 5;;
    let F2 = fun b a -> F1 F0; (+) a b;;
    F2 2 1
    |}]

let%expect_test _ =
  run `Anf code ;
  [%expect
    {|
    let F0 = fun x y -> (+) x y;;
    let F1 = fun f -> f 5;;
    let F2 = fun b a -> F1 F0; (+) a b;;
    F2 2 1
    |}]

let code = {| let x = 1 + 2 and y = 3 and z = 4 in x - z + y |}

let%expect_test _ =
  run `SimplOpt code ;
  [%expect {| (fun z y x -> (+) ((-) x z) y) 4 ((+) 1 2) 3 |}]

let%expect_test _ =
  run `CLess code ;
  [%expect
    {|
    let F0 = fun z y x -> (+) ((-) x z) y;;
    F0 4 ((+) 1 2) 3
    |}]

let%expect_test _ =
  run `Anf code ;
  [%expect
    {|
    let F0 = fun z y x -> let V0 = (-) x z in (+) V0 y;;
    let V0 = (+) 1 2 in F0 4 V0 3
    |}]

let code =
  {| let f x y = x + y;;
     f 1 1;;
     let f x y = x - y;;
     f 2 2 |}

let%expect_test _ =
  run `SimplOpt code ;
  [%expect
    {|
    (
     fun f ->
       f 1 1; (fun f -> f 2 2) (fun x y -> (-) x y)
    ) (fun x y -> (+) x y)
    |}]

let%expect_test _ =
  run `CLess code ;
  [%expect
    {|
    let F0 = fun x y -> (+) x y;;
    let F1 = fun x y -> (-) x y;;
    let F2 = fun f -> f 2 2;;
    let F3 = fun f -> f 1 1; F2 F1;;
    F3 F0
    |}]

let code = {| (fun x -> fun y -> fun z -> x - 1 + y + z) 1 2 3 |}

let%expect_test _ =
  run `SimplOpt code ;
  [%expect {| (fun x y z -> (+) ((+) ((-) x 1) y) z) 1 2 3 |}]

let%expect_test _ =
  run `CLess code ;
  [%expect
    {|
    let F0 = fun x y z -> (+) ((+) ((-) x 1) y) z;;
    F0 1 2 3
    |}]

let%expect_test _ =
  run `Anf code ;
  [%expect
    {|
    let F0 =
      fun x y z ->
        let V0 = (-) x 1 in
        let V1 = (+) V0 y in (+) V1 z;;
    F0 1 2 3
    |}]

let code =
  {| let f x y =
     let x z = y + z in
     let y z = x 1 + z in
     x 1 + y 2;; f 5 10 |}

let%expect_test _ =
  run `SimplOpt code ;
  [%expect
    {|
    (fun f -> f 5 10)
      (
       fun x y ->
         (
          fun x ->
            (fun y -> (+) (x 1) (y 2))
              (fun z -> (+) (x 1) z)
         ) (fun z -> (+) y z)
      )
    |}]

let%expect_test _ =
  run `CLess code ;
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
  run `Anf code ;
  [%expect
    {|
    let F0 = fun y z -> (+) y z;;
    let F1 = fun x z -> let V0 = x 1 in (+) V0 z;;
    let F2 =
      fun x y ->
        let V0 = y 2 in let V1 = x 1 in (+) V1 V0;;
    let F3 = fun x -> let V0 = F1 x in F2 x V0;;
    let F4 = fun x y -> let V0 = F0 y in F3 V0;;
    let F5 = fun f -> f 5 10;;
    F5 F4
    |}]

let code = {| let rec fact n = if n < 2 then 1 else n * fact (n-1) in fact 5 |}

let%expect_test _ =
  run `SimplOpt code ;
  [%expect
    {|
    (fun fact -> fact 5)
      (
       fix
         (
          fun fact n ->
            if (<) n 2
            then 1 else (*) n (fact ((-) n 1))
         )
      )
    |}]

let%expect_test _ =
  run `CLess code ;
  [%expect
    {|
    let rec F0 =
      fun n ->
        if (<) n 2 then 1 else (*) n (F0 ((-) n 1));;
    let F1 = fun fact -> fact 5;;
    F1 F0
    |}]

let%expect_test _ =
  run `Anf code ;
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
