(* Splits a trace file into a few equal parts (across time),
 * or extracts a specified interval of time from the trace.
 *)

open Printf
open Util

let main =
	let nargs = Array.length Sys.argv in
	if nargs != 3 && nargs != 4 then (
		fprintf stderr "Usage: %s <trace> <# of parts>
    or %s <trace> <start time> <end time>\n" Sys.argv.(0) Sys.argv.(0);
		exit 1);
	let trace_name = Sys.argv.(1) in
	let trace = Trace.load_trace trace_name in
	let s = Trace.start_time trace in
	let t = Trace.end_time trace in
	let len = Trace.length trace in
	if nargs == 4 then (
		let s_new = float_of_string Sys.argv.(2) in
		let t_new = float_of_string Sys.argv.(3) in
		let subt = Trace.subtrace trace s_new t_new in
		Trace.write_trace subt (trace_name ^ (sprintf "-%f-%f.avt" s_new t_new))
		)
	else (
		let num_parts = int_of_string Sys.argv.(2) in
		assert(num_parts > 1);
		let part_len = len /. (i2f num_parts) in
		for i = 0 to num_parts-1 do
			let subt = Trace.subtrace trace (part_len *. i2f i) (part_len *. i2f (i+1)) in
			Trace.write_trace subt (trace_name ^ (sprintf "-%i.avt" i))
			done
		)
