[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Stdio

module Format = Stdlib.Format

type err = ParseError | InferError of LInfer.IError.t

let run' s =
  let print id ty =
    let open PPrint in
    let doc =
      group @@ LPrint.pp_id id ^^ string ": " ^^ LPrint.pp_ty ty ^^ hardline
    in
    ToChannel.pretty 1. 40 stdout doc
  in

  let open Result in
  let ( let* ) = ( >>= ) in

  let* structure =
    LParse.parse s |> Option.value_map ~default:(fail ParseError) ~f:return
  in

  List.fold_result structure ~init:LInfer.Env.empty ~f:(fun env item ->
      let* {ty; env; bounds} = LInfer.infer env item in

      (* print results *)
      Option.iter ty ~f:(print (I "_")) ;
      List.iter bounds ~f:(fun id -> Map.find_exn env id |> print id) ;

      return env )
  |> map_error ~f:(fun err -> InferError err)

let run s =
  match run' s with
  | Error ParseError ->
      print_endline "syntax error"
  | Error (InferError err) ->
      LInfer.IError.pp Format.std_formatter err
  | Ok _ ->
      ()

let%expect_test _ =
  run {|
    let id1 = fun x -> x in
    id1 42; id1 "hello"
  |} ;
  [%expect {| _: string |}]

let%expect_test _ =
  run {|
    let f id = id 42; id "hello" in
    f (fun x -> x)
  |} ;
  [%expect
    {| (UnificationFail ((Con ((I "int"), [])), (Con ((I "string"), [])))) |}]

let%expect_test _ =
  run {| fun x -> let y = x in y |} ;
  [%expect {| _: 'a -> 'a |}]

let%expect_test _ =
  run {|
    fun x ->
      let y = fun z -> x z in y |} ;
  [%expect {| _: ('a -> 'b) -> 'a -> 'b |}]

let%expect_test _ =
  run {| fun x f -> f x |} ; [%expect {| _: 'a -> ('a -> 'b) -> 'b |}]

let%expect_test _ =
  run {| fun f -> fun x -> f x |} ;
  [%expect {| _: ('a -> 'b) -> 'a -> 'b |}]

let%expect_test _ =
  run {| fun f -> fun x -> g x |} ;
  [%expect {| (UnboundVariable (I "g")) |}]
