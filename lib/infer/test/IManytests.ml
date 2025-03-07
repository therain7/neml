[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Stdio

open ITestRun

let run path =
  try In_channel.read_all path |> run
  with Sys_error _ -> print_endline "failed to read"

let%expect_test _ =
  run "./manytests/do_not_type/001.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/do_not_type/002if.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/do_not_type/003occurs.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/do_not_type/004let_poly.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/do_not_type/015tuples.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/do_not_type/099.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/typed/001fac.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/typed/002fac.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/typed/003fib.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/typed/004manyargs.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/typed/005fix.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/typed/006partial.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/typed/006partial2.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/typed/006partial3.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/typed/007order.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/typed/008ascription.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/typed/009let_poly.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/typed/010sukharev.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/typed/011mapcps.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/typed/012fibcps.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/typed/013foldfoldr.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/typed/015tuples.ml" ;
  [%expect {| failed to read |}]

let%expect_test _ =
  run "./manytests/typed/016lists.ml" ;
  [%expect {| failed to read |}]
