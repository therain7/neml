[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open LMisc
open LTypes

module IError = struct
  type t =
    | UnboundVariable of Id.t
        (** Failed to find a variable in the environment *)
    | UnificationFail of Ty.t * Ty.t  (** Failed to unify two types *)
    | UnificationMismatch of Ty.t list * Ty.t list
        (** Lists of types to unify have different lengths *)
    | OccursIn of Var.t * Ty.t  (** Type variable occurs in a type *)
    | PatVarBoundSeveralTimes of Id.t
        (** Pattern(s) bound the same variable several times. E.g. `let x, x = ..` *)
    | NotImplemented of string  (** Too bad something's not done *)
  [@@deriving show {with_path= false}]
end

module Sc = struct
  (** Type with universally quantified type variables *)
  type t = Forall of VarSet.t * Ty.t
  [@@deriving show {with_path= false}, ord, sexp_of]

  let vars : t -> VarSet.t =
   fun (Forall (quantified, ty)) -> Set.diff (Ty.vars ty) quantified
end

let generalize (bound : VarSet.t) (ty : Ty.t) : Sc.t =
  Forall (Set.diff (Ty.vars ty) bound, ty)

module Con = struct
  module T = struct
    (** Type constraints *)
    type t =
      | TyEq of Ty.t * Ty.t  (** Reflects that types should be unified *)
      | ImplInst of Ty.t * VarSet.t * Ty.t
          (** States that t1 should be an instance of the type scheme
            that is obtained by generalizing type t2 with respect
            to the set of monomorphic type variables M *)
      | ExplInst of Ty.t * Sc.t
          (** States that ty has to be a generic instance of the type scheme *)
    [@@deriving show {with_path= false}, ord, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

module ConSet = struct
  (** Set of type constraints *)
  type t = (Con.t, Con.comparator_witness) Set.t

  let empty : t = Set.empty (module Con)
  let single x : t = Set.singleton (module Con) x
end
