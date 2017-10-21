open Printf
open Util
open Trace

(* EDITED VERSION WITH POSTSCRIPT, JUST SWAP TO PDF IN CASE OF NEED *)
let makeplot basename contents =
	let gplfile = open_out (basename ^ ".gpl") in
	 (*Old postscript version:*)
	fprintf gplfile "set output \"%s.ps\"\nset terminal postscript color\n"
		basename; 
	(*fprintf gplfile "set output \"%s.pdf\"\nset terminal pdf\n"
		basename; *)
	output_string gplfile contents;
	close_out gplfile;
	ignore (Sys.command (sprintf "gnuplot %s.gpl" basename));
	(* Old postscript version: *)
		ignore (Sys.command (sprintf "gv --spartan --watch %s.ps" basename)) 
	(*ignore (Sys.command (sprintf "open %s.pdf" basename))*)

let scale_down trace =
	if trace.n > 5000 then (
		printf "**** Large number of nodes in trace (%d); showing 5000 random nodes.\n"
			trace.n;
		flush stdout;
		random_subset trace 5000
		)
	else
		trace

(* Relabels the names in a trace (returning a new trace, not modifying old one)
 * so that nodes with temporally correlated events have similar names.
 * Useful for visualization.
 *
 * Heuristic strategy: Assume the "similarity" of two nodes is the inverse of the
 * distance in time of their closest events.
 *)
let cluster trace =
	let similarity i j =
		let sim_aux i j =
			let total_similarity = ref 0.0 in
			let evts_i = Array.of_list trace.events.(i) in
			let evts_j = Array.of_list trace.events.(j) in
			let n_ev_i = Array.length evts_i in
			let n_ev_j = Array.length evts_j in
			let curr_j = ref 0 in
			for curr_i = 0 to n_ev_i - 1 do
				while !curr_j < n_ev_j && evts_j.(!curr_j) < evts_i.(curr_i) do
					incr curr_j
					done;
				let min_dist = ref max_float in
				let consider idx = 
					if 0 <= idx && idx < n_ev_j then
						min_dist := min !min_dist (abs_float (evts_i.(curr_i) -. evts_j.(idx)))
					in
				consider !curr_j;
				consider (!curr_j - 1);
				if n_ev_j > 0 then (
					assert(!min_dist < max_float);
					total_similarity := !total_similarity +. 1. /. (1. +. !min_dist *. !min_dist)
					)
				done;
			!total_similarity
			in
		min (sim_aux i j) (sim_aux j i)
		in
	let mapping = Array.init trace.n (fun i -> 0) in
	let unprocessed = ref (make_list (fun i -> i + 1) (trace.n - 1)) in
	let last = ref 0 in
	for j = 1 to trace.n - 1 do
		let closest = argmax !unprocessed (similarity !last) in
		unprocessed := List.filter (fun x -> x != closest) !unprocessed;
		mapping.(j) <- closest;
		last := closest
		done;
	{
		name = trace.name ^ " (clustered)";
		n = trace.n;
		nodenames = Array.init trace.n (fun i -> trace.nodenames.(mapping.(i)));
		events    = Array.init trace.n (fun i -> trace.events.(mapping.(i)))
	}


let timeline please_cluster trace =
	let trace =
		if please_cluster then
			cluster (scale_down trace)
		else
			scale_down trace
		in
	let key = Hashtbl.hash trace.name in
	let basename = sprintf "/tmp/timeline-%d" key in
	let datfile = open_out (basename ^ ".dat") in
	for i = 0 to trace.n - 1 do
		let rec process events =
			match events with
				t1::t2::events -> (
					fprintf datfile "%g %d\n%g %d\n\n" t1 i t2 i;
					process events
					)
				| [] -> ()
				| _ -> raise (Failure "Illegal trace file!\n")
			in
		process trace.events.(i)
		done;
	close_out datfile;
	makeplot basename (sprintf "
		set xlabel \"Time\"
		set ylabel \"Nodes\"
		set title \"Periods of uptime in trace %s\"
		set nokey
		plot \"%s.dat\" w l" trace.name basename)

let binned_sessiontimes sessiontimes =
	let rounded_sessiontimes = List.map (fun x -> f2i (log_2 x)) sessiontimes in
	let max_sessiontime = List.fold_left max 0 rounded_sessiontimes in
	let min_sessiontime = List.fold_left min max_int rounded_sessiontimes in
	let bins = Array.create (abs (max_sessiontime - min_sessiontime) + 1) 0 in
	foreach rounded_sessiontimes (fun t ->
		let i = abs (t - min_sessiontime) in
		bins.(i) <- 1 + bins.(i));
	bins, min_sessiontime

let sessiontime_distribution_aux title sessiontimes =
	let key = Hashtbl.hash title in
	let basename = sprintf "/tmp/sessiontime_pdf-%d" key in
	let outfile = open_out (basename ^ ".dat") in
	let bins, min_index = binned_sessiontimes sessiontimes in
	for i = 0 to Array.length bins - 1 do
		fprintf outfile "%d %d\n" (i + min_index) bins.(i)
		done;
	close_out outfile;
	makeplot basename (sprintf "
		set xlabel \"floor(log_2(session length))\"
		set ylabel \"Frequency\"
		set title \"%s\"
		set nokey
		plot \"%s.dat\" w lp" title basename)

let sessiontime_distribution trace =
	let trace = scale_down trace in
	let sessiontimes = Array.map extract_sessiontimes trace.events in
	let avg_sessiontimes = Array.map (fun lts -> fst (mean_stddev lts)) trace.events in
	let nodes = Array.init trace.n (fun i -> i) in
	Array.sort (fun i j -> compare avg_sessiontimes.(i) avg_sessiontimes.(j)) nodes;
	(* let sorted_sessiontimes = Array.map (fun i -> sessiontimes.(i)) nodes in *)

	sessiontime_distribution_aux ("PDF of sessiontimes from "^trace.name)
		(List.flatten (Array.to_list sessiontimes))
	(*;
	for i = 0 to 9 do
		let slice_of_sessiontimes = Array.sub sorted_sessiontimes (i*trace.n/10) (trace.n/10) in
		sessiontime_distribution_aux (sprintf
			"PDF of sessiontimes of %dth 10%% least long-lived nodes from %s"
			(i+1) trace.name) (List.flatten (Array.to_list slice_of_sessiontimes))
		done
	*)

let sessiontime_cdf trace =
	(*let trace = scale_down trace in *)
	let sessiontimes = List.flatten (Array.to_list (Array.map extract_sessiontimes trace.events)) in
	let sessiontimes = List.filter (fun x -> x > 0.0) sessiontimes in
	let key = Hashtbl.hash trace.name in
	let basename = sprintf "/tmp/sessiontime_cdf-%d" key in
	output_cdf_floats sessiontimes (basename ^ ".dat");
	makeplot basename (sprintf "
		set log x
		set xlabel \"x\"
		set ylabel \"Fraction of sessions of length <= x\"
		set title \"CDF of session lengths\"
		set nokey
		set yrange [0:1]
		plot \"%s.dat\" w lp" basename)
	
(* Session time distribution, but each session is normalized by the mean session time of its node.
 *)
let normalized_sessiontime_cdf trace =
	(* get array of, for each node, list of sessions *)
	let sessions = Array.to_list (Array.map extract_sessiontimes trace.events) in
	let sessions = List.filter (fun s -> List.length s >= 10) sessions in
	let sessions = Array.of_list sessions in
	for i = 0 to Array.length sessions - 1 do
		sessions.(i) <- List.filter (fun x -> x > 0.0) sessions.(i);
		let mean, _ = mean_stddev sessions.(i) in
		sessions.(i) <- List.map (fun x -> x /. mean) sessions.(i)
		done;

	let key = Hashtbl.hash trace.name in
	let basename = sprintf "/tmp/normalized_sessiontime_cdf-%d" key in
	
	let sessiontimes = List.flatten (Array.to_list sessions) in
	output_cdf_floats sessiontimes (basename ^ ".dat");
	let gpltext = ref (sprintf "
		set log x
		set xlabel \"x\"
		set ylabel \"Fraction of sessions of relative length <= x\"
		set title \"CDF of session lengths\"
		set nokey
		set yrange [0:1]
		plot \"%s.dat\" w lp lt -1 lw 4" basename) in

	(* Output some random session time distributions of individual nodes too *)
	for i = 1 to 10 do
		let j = Random.int (Array.length sessions) in
		output_cdf_floats ~square:false sessions.(j) (sprintf "%s-%d.dat" basename i);
		gpltext := !gpltext ^ (sprintf ",\\\n\"%s-%d.dat\" w lp" basename i)
		done;

	makeplot basename !gpltext
	
let downtime_cdf trace =
	(* let trace = scale_down trace in *)
	let downtimes = List.flatten (Array.to_list (Array.map extract_downtimes trace.events)) in
	let downtimes = List.sort compare downtimes in
	let num_downtimes = List.length downtimes in

	let key = Hashtbl.hash trace.name in
	let basename = sprintf "/tmp/downtime_cdf-%d" key in
	let outfile = open_out (basename ^ ".dat") in
	let i = ref 0 in
	foreach downtimes (fun lt ->
		incr i;
		fprintf outfile "%g %g\n" lt ((i2f !i) /. (i2f num_downtimes));
		);
	close_out outfile;
	makeplot basename (sprintf "
		set log x
		set xlabel \"x\"
		set ylabel \"Fraction of downtimes of length <= x\"
		set title \"CDF of downtime lengths\"
		set nokey
		set yrange [0:1]
		plot \"%s.dat\" w lp" basename)
	
let per_node_sessiontimes trace =
	let key = Hashtbl.hash trace.name in
	let basename = sprintf "/tmp/per_node_sessiontimes-%d" key in
	let outfile = open_out (basename ^ ".dat") in
	let avg_sessiontimes = Array.map (fun evts ->
		fst (mean_stddev (extract_sessiontimes evts))) trace.events in
	let nodes = Array.init trace.n (fun i -> i) in
	let bins_and_min_indeces = Array.map (fun events -> binned_sessiontimes
		(extract_sessiontimes events)) trace.events in
	Array.sort (fun i j -> compare avg_sessiontimes.(i) avg_sessiontimes.(j)) nodes;
	for i = 0 to trace.n-1 do
		(*let bins = binned_sessiontimes (extract_sessiontimes trace.events.(i)) in *)
		let bins, min_index = bins_and_min_indeces.(nodes.(i)) in
		for j = 0 to Array.length bins - 1 do
			fprintf outfile "%d %d %d\n" i (j + min_index) bins.(j)
			done;
		fprintf outfile "\n";
		done;
	close_out outfile;
	makeplot basename (sprintf "
		set xlabel \"Node\"
		set ylabel \"sessiontime\"
		set zlabel \"Frequency\"
		set title \"%s\"
		set nokey
		splot \"%s.dat\" w lines" trace.name basename)


let total_uptime_cdf trace =
	let sessiontimes_arr = Array.map extract_sessiontimes trace.events in
	let uptimes_arr = Array.map sum_floats sessiontimes_arr in
	Array.sort compare uptimes_arr;
	let key = Hashtbl.hash trace.name in
	let basename = sprintf "/tmp/total_uptime_cdf-%d" key in
	let outfile = open_out (basename ^ ".dat") in
	for i = 1 to trace.n do
		fprintf outfile "%g %g\n" uptimes_arr.(i-1) ((i2f i) /. (i2f trace.n))
		done;
	close_out outfile;
	makeplot basename (sprintf "
		set xlabel \"Total time during trace spent up\"
		set ylabel \"CDF\"
		set title \"CDF of nodes' total uptime\"
		set nokey
		plot \"%s.dat\" w lp" basename)

let avg_sessiontime_cdf trace =
	let sessiontimes_arr = Array.map extract_sessiontimes trace.events in
	let avg_lt_arr = Array.map (fun lts -> fst (mean_stddev lts)) sessiontimes_arr in
	Array.sort compare avg_lt_arr;
	let key = Hashtbl.hash trace.name in
	let basename = sprintf "/tmp/avg_sessiontime_cdf-%d" key in
	let outfile = open_out (basename ^ ".dat") in
	for i = 1 to trace.n do
		fprintf outfile "%g %g\n" avg_lt_arr.(i-1) ((i2f i) /. (i2f trace.n))
		done;
	close_out outfile;
	makeplot basename (sprintf "
		set log x
		set xlabel \"Average sessiontime\"
		set ylabel \"CDF\"
		set title \"CDF of nodes' average sessiontime\"
		set nokey
		plot \"%s.dat\" w lp" basename)

let num_nodes_up_vs_time trace =
	let trace = scale_down trace in
	let num_up_log = num_nodes_up_log trace in
	let key = Hashtbl.hash trace.name in
	let basename = sprintf "/tmp/num_nodes_up_vs_time-%d" key in
	let outfile = open_out (basename ^ ".dat") in
	(*printf "Got %d events, writing out to %s.dat\n" (List.length sorted_evts)
		basename; flush stdout;*)
	foreach num_up_log (fun (time, num) ->
		fprintf outfile "%g %d\n" time num);
	close_out outfile;
	makeplot basename (sprintf "
		set xlabel \"Time\"
		set ylabel \"Number of nodes up\"
		set title \"Number of nodes up in %s\"
		set nokey
		plot \"%s.dat\" w lines" trace.name basename)

let event_rate_vs_time trace =
	let n_bins_int = 1000 in
	let n_bins = i2f n_bins_int in
	let n_evts = Array.create n_bins_int 0 in
	let start = start_time trace in
	let length = length trace in
	foreach_arr trace.events (fun node_evts ->
		foreach node_evts (fun t ->
			let bin = f2i (n_bins *. (t -. start) /. length) in
			if bin < n_bins_int then (* cut off events at very end of trace *)
				n_evts.(bin) <- 1 + n_evts.(bin)
			)
		);
	let key = Hashtbl.hash trace.name in
	let basename = sprintf "/tmp/event_rate_vs_time-%d" key in
	let outfile = open_out (basename ^ ".dat") in
	let bin_length = length /. n_bins in
	for i = 0 to Array.length n_evts - 1 do
		let time = (i2f i /. n_bins) *. length in
		fprintf outfile "%g %g\n" time (i2f n_evts.(i) /. bin_length)
		done;
	close_out outfile;
	makeplot basename (sprintf "
		set xlabel \"Time\"
		set ylabel \"Events per time unit\"
		set title \"Event rate vs. time in %s\"
		set nokey
		plot \"%s.dat\" w lines lw 2" trace.name basename)
	

let avg_sessiontime_vs_avail trace =
	let key = Hashtbl.hash trace.name in
	let basename = sprintf "/tmp/avg_sessiontime_vs_avail-%d" key in
	let avg_sessiontimes = Array.map (fun evts ->
		fst (mean_stddev (extract_sessiontimes evts))) trace.events in
	let availabilities = Array.map (fun evts ->
		let len = (last_event evts) -. (first_event evts) in
		if len > 0.0 then
			(sum_floats (extract_sessiontimes evts)) /. len
		else
			0.0)
		trace.events in
	let outfile = open_out (basename ^ ".dat") in
	for i = 0 to trace.n - 1 do
		fprintf outfile "%g %g\n" avg_sessiontimes.(i) (1.0 -. availabilities.(i))
		done;
	close_out outfile;
	makeplot basename (sprintf "
		set xlabel \"Average sessiontime\"
		set ylabel \"Fraction of time down\"
		set title \"Scatterplot of node sessiontime vs. availability %s\"
		set nokey
		set log x
		set log y
		plot \"%s.dat\" w points" trace.name basename)	

(* Filters pairs of entries (x.(i), y.(i)) for which one entry is not finite or nan. *)
let filter x y =
	let n = Array.length x in
	assert(n = Array.length y);
	let is_finite_number x = x != neg_infinity && x != infinity && x < infinity in
	let index_is_good i = is_finite_number x.(i) && is_finite_number y.(i) in
	let x_filtered = Array.make n 0.0 in
	let y_filtered = Array.make n 0.0 in
	let j = ref 0 in
	for i = 0 to n-1 do
		if index_is_good i then (
			x_filtered.(!j) <- x.(i);
			y_filtered.(!j) <- y.(i);
			incr j
			)
		done;
	(Array.sub x_filtered 0 !j), (Array.sub y_filtered 0 !j)

let first_half_vs_second_half trace =
	let start = Trace.start_time trace in
	let end_time : float = Trace.end_time trace in
	let mid = (start +. end_time) /. 2. in
	let half1 = Trace.subtrace trace start mid in
	let half2 = Trace.subtrace trace mid end_time in

	let key = Hashtbl.hash trace.name in
	let basename = sprintf "/tmp/1st_vs_2nd-%d" key in
	let avg_sessiontimes1 = Array.map (fun evts ->
		fst (mean_stddev (extract_sessiontimes evts))) half1.events in
	let avg_sessiontimes2 = Array.map (fun evts ->
		fst (mean_stddev (extract_sessiontimes evts))) half2.events in
	let al1_filtered, al2_filtered = filter avg_sessiontimes1 avg_sessiontimes2 in

    let num_only_in_1st = ref 0 in
	let num_only_in_2nd = ref 0 in
	for i = 0 to trace.n - 1 do
		if avg_sessiontimes1.(i) > 0.0 && (not (avg_sessiontimes2.(i) > 0.0)) then
			incr num_only_in_1st
		else if avg_sessiontimes2.(i) > 0.0 && (not (avg_sessiontimes1.(i) > 0.0)) then
			incr num_only_in_2nd
		done;
	printf "Not shown: %d nodes that were up only in the 1st half; %d only in 2nd\n"
		!num_only_in_1st !num_only_in_2nd;
	printf "Correlation coefficient between avg sessiontimes in 1st and 2nd half: %f\n"
		(correlation_coefficient al1_filtered al2_filtered); flush stdout;
	let outfile = open_out (basename ^ ".dat") in
	(*for i = 0 to trace.n - 1 do
		fprintf outfile "%g %g\n" avg_sessiontimes1.(i) avg_sessiontimes2.(i)
		done;*)
	for i = 0 to Array.length al1_filtered - 1 do
		fprintf outfile "%g %g\n" al1_filtered.(i) al2_filtered.(i)
		done;
	close_out outfile;
	makeplot basename (sprintf "
		set xlabel \"Average sessiontime in first half\"
		set ylabel \"Average sessiontime in second half\"
		set title \"Scatterplot of avg node sessiontime in 1st vs. 2nd half of %s\"
		set nokey
		set log x
		set log y
		plot \"%s.dat\" w points" trace.name basename)	

let num_sessions_cdf trace =
	let num_lt_arr = Array.map (fun l -> (List.length l) / 2) trace.events in
	Array.sort compare num_lt_arr;
	let key = Hashtbl.hash trace.name in
	let basename = sprintf "/tmp/num_sessions_cdf-%d" key in
	let outfile = open_out (basename ^ ".dat") in
	for i = 1 to trace.n do
		fprintf outfile "%d %g\n" num_lt_arr.(i-1) ((i2f i) /. (i2f trace.n))
		done;
	close_out outfile;
	makeplot basename (sprintf "
		set log x
		set xlabel \"Number of sessions in trace\"
		set ylabel \"Fraction of nodes\"
		set title \"CDF of nodes' number of sessions\"
		set nokey
		plot \"%s.dat\" w lp" basename)

let session_time_vs_num_sessions trace =
	let key = Hashtbl.hash trace.name in
	let basename = sprintf "/tmp/avg_sessiontime_vs_avail-%d" key in
	let avg_sessiontimes = Array.map (fun evts ->
		fst (mean_stddev (extract_sessiontimes evts))) trace.events in
	let num_sessions = Array.map (fun evts -> List.length evts / 2) trace.events in
	let outfile = open_out (basename ^ ".dat") in
	for i = 0 to trace.n - 1 do
		fprintf outfile "%g %d\n" avg_sessiontimes.(i) num_sessions.(i)
		done;
	close_out outfile;
	let l = Trace.length trace in
	let x_max = Array.fold_left max 0.0 avg_sessiontimes in
	let x_min = Array.fold_left min max_float avg_sessiontimes in
	let y_max = Array.fold_left max 0 num_sessions in
	let y_min = Array.fold_left min max_int num_sessions in
	assert(x_min <= x_max);
	assert(y_min <= y_max);
	makeplot basename (sprintf "
		set xlabel \"Average session time\"
		set ylabel \"Number of sessions\"
		set title \"Scatterplot of mean sessiontime vs. number of sessions %s\"
		set log x
		set log y
		plot [%g:%g] [%d:%d] \"%s.dat\" notitle w points, \\
		     %g / x w lines title \"100%% availability\", \\
		     0.1 * %g / x w lines title \"10%% availability\", \\
		     0.01 * %g / x w lines title \"1%% availability\"
		"
		trace.name x_min x_max y_min y_max basename l l l)

let statistics trace =
	let avg_sessiontimes = Array.map (fun evts ->
		fst (mean_stddev (extract_sessiontimes evts))) trace.events in
	let mean, stddev = mean_stddev (Array.to_list avg_sessiontimes) in
	let normalized_stddev = stddev /. mean in
	let num_sec = length trace in
	let num_days = num_sec /. (24. *. 60. *. 60.) in

	printf "Name: %s\n" trace.name;
	printf "Number of nodes: %d\n" trace.n;
	printf "Length: %g (%f days if time is in seconds)\n" num_sec num_days;
	printf "Time range: %g to %g\n" (start_time trace) (end_time trace);
	printf "Average number of nodes up: %f\n" (avg_num_nodes_up trace);
	printf "Normalized sample std deviation in nodes' mean session times: %f\n" normalized_stddev;
	flush stdout

let options = [|
	"Basic statistics", statistics;
	"Timeline", timeline false;
	"Timeline, clustered (warning: slow for large traces)", timeline true;
	"Number of nodes up vs. time", num_nodes_up_vs_time;
	"Event rate vs. time", event_rate_vs_time;
	"Session time distribution", sessiontime_distribution;
	"Session time CDF", sessiontime_cdf;
	"Session time CDF w/sessions normalized by node's mean", normalized_sessiontime_cdf;
	"Per-node session time distribution", per_node_sessiontimes;
	"CDF of total node uptime", total_uptime_cdf;
	"CDF of average node session time", avg_sessiontime_cdf;
	"CDF of number of sessions", num_sessions_cdf;
	"Scatterplot of mean session time vs. number of sessions", session_time_vs_num_sessions;
	"Scatterplot of mean session time vs. availability", avg_sessiontime_vs_avail;
	"Scatterplot of mean session time in 1st vs. 2nd half of trace", first_half_vs_second_half;
	"Downtime CDF (excluding downtimes touching beginning or end of trace)", downtime_cdf;
	|]

let main =

	Random.self_init ();

	if Array.length Sys.argv != 2 then (
		fprintf stderr "Usage: %s <trace_filename>\n" Sys.argv.(0);
		exit 1);
	let filename = Sys.argv.(1) in
	let trace = load_trace filename in
	printf "Loaded %d nodes from trace.\n" trace.n;
	while true do
		for i = 0 to Array.length options - 1 do
			printf "%2d   %s\n" i (fst options.(i))
			done;
		printf "Your selection: ";
		flush stdout;
		(snd options.(read_int ())) trace
		done
