(* Produce a new trace which contains a random subset of the nodes in
 * a given trace.
 *)
open Printf
open Util

let main =
	let nargs = Array.length Sys.argv in
	if nargs != 3 && nargs != 4 then (
		fprintf stderr "Usage: %s <trace> <# of nodes to sample> <outfile>\n"
			Sys.argv.(0);
		exit 1);
	let trace_name = Sys.argv.(1) in
	let n = int_of_string Sys.argv.(2) in
	let trace = Trace.load_trace trace_name in
	let sampled_trace =
		if n > trace.Trace.n then (
			fprintf stderr "Error: %d nodes desired, but only %d in trace\n"
				n trace.Trace.n;
			exit 1
			)
		else if n = trace.Trace.n then
			trace
		else
			Trace.random_subset trace n
		in
	Trace.write_trace sampled_trace Sys.argv.(3)
