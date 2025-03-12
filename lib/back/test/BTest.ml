[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Stdio

open LMiddle
open LBack

module Format = Stdlib.Format

type err = ParseErr | MiddleErr of MSimpl.err | CodegenErr of BCodegen.err

let run' s =
  let open Result in
  let ( let* ) = ( >>= ) in

  let* structure =
    LParse.parse s |> Option.value_map ~default:(fail ParseErr) ~f:return
  in

  let* sim =
    MSimpl.from_structure structure |> map_error ~f:(fun err -> MiddleErr err)
  in
  let opt = MOpt.opt sim in

  let module LLModule = struct
    open Llvm
    let lctx = create_context ()
    let lmod = create_module lctx "neml"
  end in
  let module LLRuntime = LLRuntime.Make (LLModule) in
  let module Builtin = LBuiltin.Make (LLModule) (LLRuntime) in
  let globals, builtins =
    List.fold_right Builtin.builtins ~init:(IdSet.empty, [])
      ~f:(fun (id, _, bltn) (globals, builtins) ->
        (Set.add globals id, bltn :: builtins) )
  in

  let cls = MCLess.from_simpl ~globals opt in
  let anf = MAnf.from_cless cls in

  PPrint.ToChannel.pretty 1. 80 stdout
    (LPrint.pp_structure (MAnf.to_structure anf)) ;
  let module CodeGen = BCodegen.LLCodeGen (LLModule) (LLRuntime) in
  let* () =
    CodeGen.gen ~builtins anf |> map_error ~f:(fun err -> CodegenErr err)
  in

  return @@ Format.printf "\n\n%s" (Llvm.string_of_llmodule LLModule.lmod)

let run s =
  match run' s with
  | Error ParseErr ->
      print_endline "syntax error"
  | Error (MiddleErr err) ->
      MSimpl.pp_err Format.std_formatter err
  | Error (CodegenErr err) ->
      BCodegen.pp_err Format.std_formatter err
  | Ok () ->
      ()

let%expect_test _ =
  run {| let f x y = x - y + 42 in print_int (10 - 5) 1 |} ;
  [%expect
    {|
    let f0 = fun x y -> let v0 = ( - ) x y in ( + ) v0 42;;
    let f1 = fun f -> let v0 = ( - ) 10 5 in print_int v0 1;;
    f1 f0

    ; ModuleID = 'neml'
    source_filename = "neml"

    declare i64 @neml_print_int(i64)

    declare i64 @neml_apply_closure(i64, i64, ...)

    declare i64 @neml_create_closure(i64, i64)

    ; Function Attrs: alwaysinline
    define i64 @u43(i64 %0, i64 %1) #0 {
    entry:
      %r = add i64 %0, %1
      ret i64 %r
    }

    ; Function Attrs: alwaysinline
    define i64 @u45(i64 %0, i64 %1) #0 {
    entry:
      %r = sub i64 %0, %1
      ret i64 %r
    }

    ; Function Attrs: alwaysinline
    define i64 @u42(i64 %0, i64 %1) #0 {
    entry:
      %r = mul i64 %0, %1
      ret i64 %r
    }

    ; Function Attrs: alwaysinline
    define i64 @u61(i64 %0, i64 %1) #0 {
    entry:
      %r = icmp eq i64 %0, %1
      ret i1 %r
    }

    ; Function Attrs: alwaysinline
    define i64 @uprint95int(i64 %0) #0 {
    entry:
      %r = call i64 @neml_print_int(i64 %0)
      ret i64 %r
    }

    define i64 @gf0(i64 %ux, i64 %uy) {
    entry:
      %gv0 = call i64 @u45(i64 %ux, i64 %uy)
      %r = call i64 @u43(i64 %gv0, i64 42)
      ret i64 %r
    }

    define i64 @gf1(i64 %uf) {
    entry:
      %gv0 = call i64 @u45(i64 10, i64 5)
      %r = call i64 @neml_create_closure(ptr @uprint95int, i64 1)
      %r1 = call i64 (i64, i64, ...) @neml_apply_closure(i64 %r, i64 2, i64 %gv0, i64 1)
      ret i64 %r1
    }

    define i64 @main() {
    entry:
      %r = call i64 @neml_create_closure(ptr @gf0, i64 2)
      %r1 = call i64 @gf1(i64 %r)
      ret i64 0
    }

    attributes #0 = { alwaysinline }
    |}]
