[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Llvm

module LLRuntime (LLModule : sig
  val lmod : llmodule
end) : sig
  val create_closure : func:llvalue -> argc:int -> llbuilder -> llvalue
  val apply_closure :
    closure:llvalue -> args:llvalue list -> llbuilder -> llvalue
end = struct
  let lmod = LLModule.lmod
  let i64 = i64_type (module_context lmod)

  module Func = struct
    module T = struct
      type t = CreateClosure | ApplyClosure [@@deriving ord, sexp_of]
    end
    include T
    include Comparator.Make (T)
  end

  let funcs =
    Map.of_alist_exn
      (module Func)
      [ (CreateClosure, ("neml_create_closure", function_type i64 [|i64; i64|]))
      ; ( ApplyClosure
        , ("neml_apply_closure", var_arg_function_type i64 [|i64; i64|]) ) ]

  let () =
    Map.iter funcs ~f:(fun (id, typ) -> declare_function id typ lmod |> ignore)

  let create_closure ~func ~argc bld =
    let fcreate_id, fcreate_type = Map.find_exn funcs CreateClosure in
    let fcreate = lookup_function fcreate_id lmod |> Option.value_exn in
    build_call fcreate_type fcreate [|func; const_int i64 argc|] "r" bld

  let apply_closure ~closure ~args bld =
    let fapply_id, fapply_type = Map.find_exn funcs ApplyClosure in
    let fapply = lookup_function fapply_id lmod |> Option.value_exn in
    let argc = List.length args in
    build_call fapply_type fapply
      (Array.of_list (closure :: const_int i64 argc :: args))
      "r" bld
end
