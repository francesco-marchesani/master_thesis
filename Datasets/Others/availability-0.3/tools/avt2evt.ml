(* Convert a trace in ".avt" format to ".evt" format.
 *)
open Printf
open Util
open Trace

let main =
	let nargs = Array.length Sys.argv in
	if nargs != 3 then (
		fprintf stderr "Usage: %s <input.avt> <output.evt>\n"
			Sys.argv.(0);
		exit 1);
	let input_path = Sys.argv.(1) in
	let output_path = Sys.argv.(2) in
	write_evt_trace (load_trace input_path) output_path
