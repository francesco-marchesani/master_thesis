(* "Cleans" a trace file, producing another trace.
 *
 * Specifically, for each period of downtime, we remove that period (i.e.
 * consider the node up during that interval) when the average number of
 * nodes up during that time is less than half the average number of nodes
 * up over all time.
 *
 * This is just an heuristic.  Use with care.
 *
 * One reason to use this is when measurements were taken by pinging
 * from a single site.  This will hopefully filter out failed pings due to
 * a local failure rather than a true failure of the remote node.  For
 * the PlanetLab All Pairs Ping data, I used it to weed out the cases when
 * essentially all of PlanetLab was down due to upgrades or whatever.
 *
 * Incidentally, this also removes nodes which are never up (which also
 * happens whenever we use Trace.load_trace).
 *
 * The program is way slower than it needs to be (see FIXME below).
 *)

open Printf
open Util

let avg_num_up num_up_log =
	assert(List.length num_up_log >= 2);
	let t0, n0 = List.hd num_up_log in
	let last_time = ref t0 in
	let last_num = ref n0 in
	let total_num = ref 0.0 in
	foreach (List.tl num_up_log) (fun (t, n) ->
		total_num := !total_num +. (t -. !last_time) *. (i2f !last_num);
		last_time := t;
		last_num := n);
	let length = !last_time -. t0 in
	!total_num /. length

(* Find the portion of a number-of-nodes-up-log which falls within a given
 * time range (inclusive).  FIXME: really we want to do this with an array
 * and use binary search, but hopefully this will be fast enough. *)
let restrict num_up_log t0 t1 =
	List.filter (fun (t,n) -> t0 <= t && t <= t1) num_up_log

(* num_up_log represents number of nodes up over time throughout entire trace.
 * t is number-of-nodes-up-threshold below which a particular node's downtime
 * is considered spurious.
 *)
let rec clean_one_node (num_up_log:(float*int) list) t events =
	match events with
		uptime1::downtime::uptime2::events ->
			(
			let restricted_log = restrict num_up_log downtime uptime2 in
			let a = avg_num_up restricted_log in
			if a <= t /. 2.0 then (
				printf "Downtime @ %g, length %f: avg %f up, %f necessary\n"
					downtime (uptime2 -. downtime) a (t /. 2.0);
				(* Assume node was actually up during this time *)
				clean_one_node num_up_log t (uptime1::events)
				)
			else
				(* Most other nodes were up; this node was probably really down *)
				uptime1::downtime::(clean_one_node num_up_log t (uptime2::events))
			)
		| _ ->
			events

let main =
	if Array.length Sys.argv != 3 then (
		fprintf stderr "Usage: %s <trace> <output_trace>\n" Sys.argv.(0);
		exit 1);
	let infilename = Sys.argv.(1) in
	let outfilename = Sys.argv.(2) in
	let trace = Trace.load_trace infilename in
	let num_up_log = Trace.num_nodes_up_log trace in
	let avg_up = avg_num_up num_up_log in
	for i = 0 to trace.Trace.n - 1 do
		printf "%s...\n" trace.Trace.nodenames.(i); flush stdout;
		trace.Trace.events.(i) <- clean_one_node num_up_log avg_up
			trace.Trace.events.(i)
		done;
	Trace.write_trace trace outfilename
