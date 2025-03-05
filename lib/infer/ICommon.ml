[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open LMisc
open LTypes

module IError = struct
  type t = PatVarBoundSeveralTimes of Id.t
end

module As = struct
  (**
    Assumptions about identifiers.
    Maps identifiers to a set of type variables
    that represent identifier's supposed type.
  *)
  type t = (Id.t, VarSet.t, Id.comparator_witness) Map.t

  let empty = Map.empty (module Id)
  let single x = Map.singleton (module Id) x
  let merge = Map.merge_skewed ~combine:(fun ~key:_ v1 v2 -> Set.union v1 v2)
end

module Con = struct
  module T = struct
    (** Type constraints *)
    type t =
      | TyEq of Ty.t * Ty.t  (** Reflects that types should be unified *)
      | ImplInst of Ty.t * VarSet.t * Ty.t
          (** States that t1 should be an instance of the type scheme
            that is obtained by generalizing type t2 with respect
            to the set of monomorphic type variables M *)
    [@@deriving show {with_path= false}, ord, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

module ConSet = struct
  (** Set of type constraints *)
  type t = (Con.t, Con.comparator_witness) Set.t
end
