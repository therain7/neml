open Base
module Format = Stdlib.Format
open Format
open LCC.Common

let () =
  let s = Stdio.In_channel.(input_all stdin) |> String.rstrip in

  match LParse.parse s with
  | None ->
      print_endline "syntax error"
  | Some str ->
      let acc = LCC.Common.run_free_vars str in
      SS.pp Format.std_formatter acc
