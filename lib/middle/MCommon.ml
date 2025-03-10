[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LMisc
open LAst

module IdSet = struct
  type t = (Id.t, Id.comparator_witness) Set.t

  let empty : t = Set.empty (module Id)
  let single x : t = Set.singleton (module Id) x

  let of_list : Id.t list -> t = Set.of_list (module Id)
end

module FuncDef = struct
  type 'a t =
    | Func of {recf: Expr.rec_flag; id: Id.t; args: Id.t List1.t; body: 'a}

  let to_stritem (to_expr : 'a -> Expr.t) (Func {recf; id; args; body} : 'a t) :
      StrItem.t =
    let efunc : Expr.t =
      Fun (List1.map args ~f:(fun id -> Pat.Var id), to_expr body)
    in
    Let (recf, List1.of_list_exn [Expr.{pat= Pat.Var id; expr= efunc}])
end
