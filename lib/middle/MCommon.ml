[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LMisc

module IdSet = struct
  type t = (Id.t, Id.comparator_witness) Set.t

  let empty : t = Set.empty (module Id)
  let single x : t = Set.singleton (module Id) x

  let of_list : Id.t list -> t = Set.of_list (module Id)
end
