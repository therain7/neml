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
