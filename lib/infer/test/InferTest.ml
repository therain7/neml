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
      group @@ string "val " ^^ LPrint.pp_id id ^^ string ": "
      ^^ LPrint.pp_ty ty ^^ hardline
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
