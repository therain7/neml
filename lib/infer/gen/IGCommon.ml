[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

open LMisc
open LAst
open LTypes

open ICommon

module IGMonad : sig
  include MONAD

  val run : 'a t -> ConSet.t * ('a, IError.t) result
  val fail : IError.t -> 'a t

  val fresh : Var.t t
  (** Generate fresh type variable *)

  val cs : Con.t list -> unit t
  (** Save new type constraints *)

  val bound_vars : VarSet.t t
  (** Get current bound type variables *)

  val extend_vars : VarSet.t -> 'a t -> 'a t
  (** Run computation with extended bound vars *)

  val fold :
       'a list
    -> dir:[`Left | `Right]
    -> init:'acc
    -> f:('acc -> 'a -> 'acc t)
    -> 'acc t
end = struct
  type state = {counter: int; conset: ConSet.t; bound_vars: VarSet.t}

  include
    MakeSEMonad
      (struct
        type t = state
      end)
      (IError)

  let run m =
    let {conset; _}, x =
      run m
        {counter= 0; conset= Set.empty (module Con); bound_vars= VarSet.empty}
    in
    (conset, x)

  let fresh =
    let* st = get in
    let* () = put {st with counter= st.counter + 1} in
    return (Var.V ("gen" ^ Int.to_string st.counter))

  let cs new_cs =
    let* st = get in
    put {st with conset= Set.union st.conset (Set.of_list (module Con) new_cs)}

  let bound_vars =
    let* {bound_vars; _} = get in
    return bound_vars

  let extend_vars vars m =
    let* st = get in
    let* () = put {st with bound_vars= Set.union st.bound_vars vars} in

    let* x = m in
    let* new_st = get in
    let* () = put {new_st with bound_vars= st.bound_vars} in
    return x

  let fold l ~dir ~init ~f =
    let init = return init in
    let f acc x =
      let* acc = acc in
      f acc x
    in
    match dir with
    | `Left ->
        List.fold_left l ~init ~f
    | `Right ->
        List.fold_right l ~init ~f:(fun acc x -> f x acc)
end

let typeof_const : Const.t -> Ty.t = function
  | Int _ ->
      Ty.int
  | Char _ ->
      Ty.char
  | String _ ->
      Ty.string

let ( == ) t1 t2 = Con.TyEq (t1, t2)
let ( ++ ) = As.merge
let ( -- ) asm = List.fold ~init:asm ~f:Map.remove
