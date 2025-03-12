[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Llvm

open LMisc
open LTypes
open LBack

let llvm_binop (id : Id.t)
    (build : llvalue -> llvalue -> string -> llbuilder -> llvalue) :
    BCodegen.builtin =
  let (LId id as lid) = LLId.from_tagged (id, User) in
  fun lmod ->
    let lctx = module_context lmod in
    let lbld = builder lctx in
    let i64 = i64_type lctx in

    let typ = function_type i64 [|i64; i64|] in
    let func = define_function id typ lmod in
    add_function_attr func (create_enum_attr lctx "alwaysinline" 0L) Function ;
    position_at_end (entry_block func) lbld ;

    let ret = build (param func 0) (param func 1) id lbld in
    let _ = build_ret ret lbld in
    (lid, func, typ)

let ty s = LParse.parse_ty s |> Option.value_exn
let ty_binop = ty "int -> int -> int"

let builtins : (Id.t * Ty.t * BCodegen.builtin) list =
  [ (let id = Id.I "+" in
     (id, ty_binop, llvm_binop id build_add) )
  ; (let id = Id.I "-" in
     (id, ty_binop, llvm_binop id build_sub) )
  ; (let id = Id.I "*" in
     (id, ty_binop, llvm_binop id build_mul) ) ]
