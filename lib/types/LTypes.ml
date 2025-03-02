[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open LMisc

module Var = struct
  module T = struct
    type t = V of string [@@deriving show {with_path= false}, ord, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

module VarSet = struct
  type t = (Var.t, Var.comparator_witness) Set.t

  let pp ppf (set : t) =
    let pp_contents =
      Format.pp_print_list
        ~pp_sep:(fun out () -> Format.fprintf out ",@ ")
        Var.pp
    in
    Format.fprintf ppf "{%a}" pp_contents (Set.to_list set)
end

module Ty = struct
  type t =
    | Var of Var.t  (** A type variable such as ['a] *)
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
