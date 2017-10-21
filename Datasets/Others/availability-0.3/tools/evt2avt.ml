(* Convert a trace in ".evt" format to ".avt" format.
 *)
open Printf
open Util
open Trace

let main =
	let nargs = Array.length Sys.argv in
	if nargs != 3 then (
		fprintf stderr "Usage: %s <input.evt> <output.avt>\n"
			Sys.argv.(0);
		exit 1);
	let input_path = Sys.argv.(1) in
	let output_path = Sys.argv.(2) in
	write_trace (evt2avt (load_evt_ignoresuffix input_path)) output_path
