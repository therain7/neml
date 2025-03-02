[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open LMisc

module Ty = struct
  type t =
    | Var of Id.t  (** A type variable such as ['a] *)
    | Arr of t * t  (** [T1 -> T2] *)
    | Tuple of t List2.t  (** [T1 * ... * Tn] *)
    | Con of Id.t * t list
        (** [Con(tconstr, l)] represents:
          - [tconstr]               when [l=[]]
          - [T tconstr]             when [l=[T]]
          - [(T1, ..., Tn) tconstr] when [l=[T1, ..., Tn]]
        *)
  [@@deriving show {with_path= false}]
end
