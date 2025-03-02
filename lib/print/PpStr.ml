[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open PPrint
open Common

let pp_stritem =
  let open LAst.StrItem in
  function
  | Eval expr ->
      PpExpr.pp expr
  | Let (rec_flag, bindings) ->
      plet PpPat.pp PpExpr.pp rec_flag bindings None
  | Type {id; params; variants} ->
      let id = pp_id id in
      let params =
        let pparam id = string "'" ^^ pp_id id in

        match params with
        | [] ->
            empty
        | [id] ->
            pparam id ^^ space
        | _ ->
            let params = List.map params ~f:pparam in
            parens (flow (comma ^^ break 1) params) ^^ space
      in

      let variants =
        List.map variants ~f:(fun {id; arg} ->
            group @@ pp_id id
            ^^ optional
                 (fun ty -> string " of" ^^ nest 2 (break 1 ^^ PpTy.pp ty))
                 arg )
      in

      group @@ string "type"
      ^^ group (break 1)
      ^^ params ^^ id ^^ string " ="
      ^^ group
           ( break 1
           ^^ ifflat empty (string "| ")
           ^^ separate (break 1 ^^ string "| ") variants )

let pp_structure str =
  let open PPrint in
  let str = List.map str ~f:(fun item -> pp_stritem item) in
  flow (string ";;" ^^ twice hardline) str
