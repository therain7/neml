[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LMisc
open LTypes

open ICommon

module Sub = struct
  type t = (Var.t, Ty.t, Var.comparator_witness) Map.t

  let empty : t = Map.empty (module Var)
  let single x y : t = Map.singleton (module Var) x y

  let apply (sub : t) : Ty.t -> Ty.t =
    let rec f = function
      | Ty.Var var as ty ->
          Map.find sub var |> Option.value ~default:ty
      | Arr (ty1, ty2) ->
          Arr (f ty1, f ty2)
      | Tuple tys ->
          Tuple (List.map (List2.to_list tys) ~f |> List2.of_list_exn)
      | Con (id, args) ->
          Con (id, List.map args ~f)
    in
    f

  let compose s1 s2 =
    Map.merge_skewed
      ~combine:(fun ~key:_ _ v2 -> v2)
      (Map.map s2 ~f:(apply s1))
      s1

  let apply_sc : t -> Sc.t -> Sc.t =
   fun sub (Forall (quantified, ty)) ->
    (* remove quantified vars from substitution *)
    let sub = Set.fold quantified ~init:sub ~f:Map.remove in
    Forall (quantified, apply sub ty)

  let apply_varset : t -> VarSet.t -> VarSet.t =
   fun sub ->
    (* construct new varset by adding all vars occuring in types
       on the right hand side of respective substitutions *)
    Set.fold ~init:VarSet.empty ~f:(fun acc var ->
        let vars =
          Map.find sub var
          |> Option.value_map ~default:(VarSet.single var) ~f:Ty.vars
        in
        Set.union acc vars )

  let apply_conset : t -> ConSet.t -> ConSet.t =
   fun sub ->
    let f : Con.t -> Con.t = function
      | TyEq (ty1, ty2) ->
          TyEq (apply sub ty1, apply sub ty2)
      | ImplInst (ty1, bound, ty2) ->
          ImplInst (apply sub ty1, apply_varset sub bound, apply sub ty2)
      | ExplInst (ty, sc) ->
          ExplInst (apply sub ty, apply_sc sub sc)
    in
    Set.map (module Con) ~f
end

module Monad : sig
  include MONAD

  val fail : IError.t -> 'a t

  val fresh : Var.t t
  (** Generate fresh type variable *)
end = struct
  type state = {counter: int}

  include
    MakeSEMonad
      (struct
        type t = state
      end)
      (IError)

  let fresh =
    let* {counter} = get in
    let* () = put {counter= counter + 1} in
    return (Var.V ("solve" ^ Int.to_string counter))
end

open Monad

let occurs_check var ty = Set.mem (Ty.vars ty) var
let rec unify (ty1 : Ty.t) (ty2 : Ty.t) : Sub.t t =
  match (ty1, ty2) with
  | _, _ when Ty.equal ty1 ty2 ->
      return Sub.empty
  | Var var, ty | ty, Var var ->
      if occurs_check var ty then fail (OccursIn (var, ty))
      else return (Sub.single var ty)
  | Arr (l1, r1), Arr (l2, r2) ->
      unify_many [l1; r1] [l2; r2]
  | Tuple tys1, Tuple tys2 ->
      unify_many (List2.to_list tys1) (List2.to_list tys2)
  | Con (id1, tys1), Con (id2, tys2) when Id.equal id1 id2 ->
      unify_many tys1 tys2
  | _ ->
      fail (UnificationFail (ty1, ty2))

and unify_many (tys1 : Ty.t list) (tys2 : Ty.t list) =
  List.fold2 tys1 tys2 ~init:(return Sub.empty) ~f:(fun acc ty1 ty2 ->
      let* acc = acc in
      let* sub = unify ty1 ty2 in
      return (Sub.compose acc sub) )
  |> function
  | Unequal_lengths ->
      fail (UnificationMismatch (tys1, tys2))
  | Ok x ->
      x

let generalize (bound : VarSet.t) (ty : Ty.t) : Sc.t =
  Forall (Set.diff (Ty.vars ty) bound, ty)

let instantiate : Sc.t -> Ty.t t =
 fun (Forall (quantified, ty)) ->
  let* sub =
    Set.fold quantified ~init:(return Sub.empty) ~f:(fun acc qvar ->
        let* acc = acc in
        let* var = fresh in
        return (Sub.compose acc (Sub.single qvar (Var var))) )
  in
  return (Sub.apply sub ty)

let activevars : ConSet.t -> VarSet.t =
  let f : Con.t -> VarSet.t = function
    | TyEq (ty1, ty2) ->
        Set.union (Ty.vars ty1) (Ty.vars ty2)
    | ImplInst (ty1, bound, ty2) ->
        Set.union (Ty.vars ty1) (Set.inter bound (Ty.vars ty2))
    | ExplInst (ty, sc) ->
        Set.union (Ty.vars ty) (Sc.vars sc)
  in
  Set.fold ~init:VarSet.empty ~f:(fun acc con -> Set.union acc (f con))
