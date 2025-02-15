[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Stdio

let%expect_test _ =
  printf "%d" (LAnf.Anf.mySum 1 3) ;
  [%expect {| 4 |}]
