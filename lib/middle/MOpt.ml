[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LMisc

(** Groups chained functions *)
let group_funs =
  let rec f : MSimpl.t -> MSimpl.t = function
    | Fun (Nonrec, args0, Fun (Nonrec, args1, body)) ->
        (* fun ARGS0 -> fun ARGS1 -> BODY
           -> fun ARGS0 .. ARGS1 -> BODY *)
        let args0 = List1.to_list args0 in
        let args1 = List1.to_list args1 in
        Fun (Nonrec, List.concat [args0; args1] |> List1.of_list_exn, f body)
    | Apply (Fun (Nonrec, args0, Apply (Fun (Nonrec, args1, body), s1)), s2) ->
        (* (fun ARGS0 -> (fun ARGS1 -> BODY) S1) S2
           -> (fun ARGS0 .. ARGS1 BODY) S2 S1 *)
        let args0 = List1.to_list args0 in
        let args1 = List1.to_list args1 in
        let args = List.concat [args0; args1] |> List1.of_list_exn in
        Apply (Apply (Fun (Nonrec, args, f body), f s2), f s1)
    | Fun (recf, args, sim) ->
        Fun (recf, args, f sim)
    | Apply (sim1, sim2) ->
        Apply (f sim1, f sim2)
    | (Id _ | Const _ | Unit) as sim ->
        sim
    | If (scond, sthen, selse) ->
        If (f scond, f sthen, f selse)
    | Seq sims ->
        Seq (List2.map sims ~f)
  in
  f

let opt = group_funs
