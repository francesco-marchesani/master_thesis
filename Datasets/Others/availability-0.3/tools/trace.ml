open Util
open Printf

type trace_t =
	{
	name: string;
	n: int; (* number of nodes *)
	nodenames: string array;
	events: float list array (* Sorted list of up/down events for each node *)
	}

type evt_trace_t =
	{
	evts: (float * string * bool * string) list; (* time, nodename, is_up, extra info *)
	}
	
let first_event event_list =
	List.hd event_list

let last_event event_list =
	List.nth event_list (List.length event_list - 1)

let start_time trace =
	Array.fold_left (fun earliest event_list ->
		if list_empty event_list then
			earliest
		else
			min earliest (first_event event_list))
		max_float trace.events

let end_time trace =
	Array.fold_left (fun latest event_list ->
		if list_empty event_list then
			latest
		else
			max latest (last_event event_list))
		0.0 trace.events


let length trace =
	(end_time trace) -. (start_time trace)

let evt2avt evt =
	(* This hashtable maps nodename to list of events for that node, in reverse
	 * chronological order. *)
	let events = Hashtbl.create 1 in
(*
	printf "Event list:\n";
	foreach evt.evts (fun (t,node,is_up,extra) -> printf "    (%g, %s, %b, %s)\n"
		t node is_up extra);
	printf "\n";
*)
	foreach evt.evts (fun (time, nodename, is_up, extra_info) ->
		let evts = if Hashtbl.mem events nodename
		    then Hashtbl.find events nodename
			else []
			in
		Hashtbl.replace events nodename ((time, is_up)::evts)
		);
	let nodenames = Array.of_list (hashtbl_keys events) in
	let n = Array.length nodenames in
	(* Find the last event time in the trace.  We only need to look at the first event
	 * in each node's list, since the lists are in reverse chronological order. *)
	let last_evt_time = Hashtbl.fold (fun _ evts l -> max (fst (List.hd evts)) l)
		events 0.0 in
	{
	name = "Trace converted by evt2avt";
	n = n;
	nodenames = nodenames;
	events = Array.init n (fun i ->
		let evts = Hashtbl.find events nodenames.(i) in
(*		printf "Event list:";
		foreach evts (fun (t,is_up) -> printf " (%g, %b)" t is_up);
		printf "\n";
*)
		let evts =
			(* If there were an odd number of events then we add one at the end
			 * representing the node going down at the end of the trace.  The "end"
			 * is taken to be the last event for any node. *)
			if (List.length evts) mod 2 = 0 then
				evts
			else
				(last_evt_time, false) :: evts
			in
		let evts = List.rev evts in
(*		printf "Event list:";
		foreach evts (fun (t,is_up) -> printf " (%g, %b)" t is_up);
		printf "\n";
*)
		(* Check to make sure the format is right:  The events must consist of pairs
		 * of (up, down) times, all increasing and nonnegative. *)
		(* FIXME: it would be nice to give a more meaningful error here, rather than
		 * just asserting... *)
		let assert_time_order t1 t2 =
			if t1 > t2 then
				raise (Failure (sprintf "Time %g is out of order: appears after time %g"
					t2 t1))
			in
		let rec check evts last_time =
			match evts with
				(t1, is_up1)::(t2, is_up2)::rest -> (
					assert_time_order last_time t1;
					assert_time_order t1 t2;
					assert(is_up1 && (not is_up2));
					check rest t2
					)
				| [] -> ()
				| _ -> assert(false)
			in
		check evts 0.0;
		(* Finally, just strip off the state (is_up) since this is now implicit *)
		List.map fst evts
		)
	}

(* Given an event list, e.g. trace.events.(6), return a list of
 * the session times (lengths of intervals between pairs of events) *)
let extract_sessiontimes event_list =
	let rec extract_sessiontimes_aux sessiontimes event_list =
		match event_list with
			x::y::event_list ->
				extract_sessiontimes_aux ((y-.x)::sessiontimes) event_list
			| [] ->
				sessiontimes
			| _ -> raise (Failure "Malformed event list in extract_sessiontimes")
		in
	extract_sessiontimes_aux [] event_list

let extract_downtimes event_list =
	let n = List.length event_list in
	if n = 0 then
		[]
	else (
		(* Chop off the first and last events.  Result is periods of downtime
		 * between uptimes. *)
		let pseudoevents = List.rev (List.tl (List.rev (List.tl event_list))) in (* wheee!!! *)
		assert(List.length pseudoevents = n - 2);
		extract_sessiontimes pseudoevents
		)

let comment_regexp = Str.regexp "[ \t]*\\#.*"

(* Load a trace in ".evt" format, i.e., each line is
 *
 *     <time> <node_name> <state> <extra_info>
 *
 * where <time> is a real value, <node_name> is arbitrary non-whitespace,
 * <state> is either "up" or "down", and <extra_info> is arbitrary and may include
 * whitespace.
 *)
let load_evt_ignoresuffix filename =
	let infile = open_in filename in
	let events = ref [] in
	let line_regexp = Str.regexp "[ \t]*\\([^ \t]+\\)[ \t]*\\([^ \t]+\\)[ \t]*\\([^ \t]+\\)\\([ \t]+\\(.+\\)\\)?" in
	let line_num = ref 0 in
	(try while true do
		incr line_num;
		let line = input_line infile in
		if Str.string_match comment_regexp line 0 then
			() (* Ignore comment line *)
		else (
			if not (Str.string_match line_regexp line 0) then
				raise (Failure (sprintf "Line %d: malformed" !line_num));
			let t = float_of_string (Str.matched_group 1 line) in
			let node_name = Str.matched_group 2 line in
			let state = match Str.matched_group 3 line with
				| "up" -> true
				| "down" -> false
				| _ -> raise (Failure (sprintf "Line %d: expecting \"up\" or \"down\", got %s\n"
					!line_num (Str.matched_group 3 line)))
				in
			let extra_info = try (Str.matched_group 5 line) with Not_found -> "" in
			events := (t, node_name, state, extra_info) :: !events
			)
		done
	with End_of_file -> ());
	close_in infile;
	{evts = (List.rev !events)}

(* Load a trace in ".avt" format, as described in the README. *)
let load_avt_ignoresuffix filename =
	let infile = open_in filename in
	let names = ref [] in
	let events = ref [] in
	let line_num = ref 0 in
	(try while true do
		incr line_num;
		let line = input_line infile in
		let tokens = Str.split (Str.regexp "[ \t]+") line in
		if Str.string_match comment_regexp line 0 then
			() (* Ignore comment line *)
		else (
			match tokens with
				| id::length::rest -> (
					let length = int_of_string length in
					let node_i_events = List.map float_of_string rest in
					let m = List.length node_i_events in
					if m != 2*length then (
						let msg = sprintf "Line %d: %d entries, not %d as stated"
							!line_num m (2*length) in
						raise (Failure msg));
					(* Skip entries for nodes that are never up *)
					let total_lifetime = sum_floats (extract_sessiontimes node_i_events) in
					if total_lifetime > 0.0 then (
						names := id :: !names;
						events := node_i_events :: !events
						)
					)
				| _ ->
					raise (Failure ("Malformed line: " ^ line))
			)
		done
	with End_of_file -> ());
	close_in infile;
	names := List.rev !names;
	events := List.rev !events;
	{name=filename; n=List.length !names; nodenames=Array.of_list !names;
	 events=Array.of_list !events}

let load_avt filename =
	if Filename.check_suffix filename ".evt" then
		evt2avt (load_evt_ignoresuffix filename)
	else if Filename.check_suffix filename ".avt" then
		load_avt_ignoresuffix filename
	else
		raise (Failure "Expected filename to end in .avt or .evt")

(* Deprecated *)
let load_trace = load_avt_ignoresuffix

(* FIXME: should implement this -- just need to write `avt2evt', should be similar to
 * the function below that writes an avt in evt format. *)
(*
let load_evt filename =
	if Filename.check_suffix filename ".avt" then
		avt2evt (load_avt_ignoresuffix filename)
	else if Filename.check_suffix filename ".evt" then
		load_evt_ignoresuffix filename
	else
		raise (Failure "Expected filename to end in .avt or .evt")
*)

let write_trace trace filename =
	let prettyprint f =
		if floor f = f then
			sprintf "%Ld" (Int64.of_float f)
		else
			sprintf "%f" f
		in 
	let outfile = open_out filename in
	for i = 0 to trace.n - 1 do
		assert(List.length trace.events.(i) mod 2 = 0);
		let num_sessions = List.length trace.events.(i) / 2 in
		fprintf outfile "%s %d " trace.nodenames.(i) num_sessions;
		foreach trace.events.(i) (fun t -> fprintf outfile " %s " (prettyprint t));
		fprintf outfile "\n"
		done;
	close_out outfile


(* Write avt trace in .evt format.  Probably this should be deprecated in favor of
 * converting to avt format and then using write_trace_evt *)
let write_evt_trace trace filename =
	let all_evts = ref [] in
	let nodes_processed_so_far = Hashtbl.create 1 in
	for i = 0 to trace.n - 1 do
		if Hashtbl.mem nodes_processed_so_far trace.nodenames.(i) then (
			printf "Warning: node name \"%s\" is repeated -- skipping subsequent instances\n"
				trace.nodenames.(i);
			flush stdout;
			)
		else (
			Hashtbl.add nodes_processed_so_far trace.nodenames.(i) ();
			(* Note we have to work from the end because we're pushing items
			 * onto `all_evts' and we want events at the same time to remain
			 * in the same order. So we start with the node being up, and 
			 * reverse the event list. *)
			let up = ref true in
			foreach (List.rev trace.events.(i)) (fun t ->
				up := not !up;
				all_evts := (t, i, !up) :: !all_evts
				)
			)
		done;
	(match List.hd !all_evts with
		(_,_,true) -> ()
		| _ -> assert(false)); (* First event should be a node coming up *)
	let all_evts = List.stable_sort (fun (t1,_,_) (t2,_,_) -> compare t1 t2) !all_evts in
	let outfile = open_out filename in
	foreach all_evts (fun (t,i,up) ->
		fprintf outfile "%f %s %s\n" t trace.nodenames.(i) (if up then "up" else "down")
		);
	close_out outfile

(* Write evt trace in evt format *)
let write_trace_evt trace filename =
	let outfile = open_out filename in
	foreach trace.evts (fun (t,name,is_up,extra_info) ->
		let formatted_time = string_of_float t in
		let formatted_state = if is_up then "up" else "down" in
		fprintf outfile "%s %s %s %s\n" formatted_time name formatted_state extra_info
		);
	close_out outfile

(* Creates a trace representing the events in `trace' between times s and t.
 * Could be sped up by about 2x prolly... *)
let subtrace trace s t =
	{
	name = sprintf "%s from %g to %g" trace.name s t;
	n = trace.n;
	nodenames = Array.copy trace.nodenames;
	events = Array.map (fun event_list ->
		let new_e = ref (List.filter (fun u -> s <= u) event_list) in
		if List.length !new_e mod 2 = 1 then
			(* Record that the node was up at beginning of trace *)
			new_e := s :: !new_e;
		new_e := List.filter (fun u -> u <= t) !new_e;
		if List.length !new_e mod 2 = 1 then
			(* Record that the node was up at end of trace *)
			new_e := !new_e @ [t];
		!new_e
		) trace.events
	}	

(* Creates a trace using a random m of the n nodes in the trace *)
let random_subset trace m =
	assert(m <= trace.n);
	let p = permutation trace.n in
	{
		name = trace.name ^ "(random subset)";
		n = m;
		nodenames = Array.init m (fun i -> trace.nodenames.(p.(i)));
		events = Array.init m (fun i -> trace.events.(p.(i)))
	}

(* List with a pair (time, number of nodes up) for each change in the
 * number of nodes up in the trace. *)
let num_nodes_up_log trace =
	let events = Array.mapi (fun i times ->
		let rec make_events times =
			match times with
				t1::t2::times -> (
					if t1 > t2 then
						raise (Failure (sprintf "Malformed trace: node %d has event at time %f before %f\n"
							i t1 t2));
					(*
					else if t1 = t2 then (
						printf "WARNING: node %d (name %s) has two events at same time (%g)\n"
							i trace.nodenames.(i) t1;
						flush stdout;
						exit(1);
						);
					*)
					(t1, true) :: (t2, false) :: (make_events times)
					)
				| [] -> []
				| _ -> raise (Failure "Malformed trace structure")
			in
		make_events times) trace.events
	in
	let all_evts = List.flatten (Array.to_list events) in
	let sorted_evts = List.stable_sort (fun e1 e2 -> compare (fst e1) (fst e2)) all_evts in
	let num_up = ref 0 in
	(* Note, rev_map is tail-recursive. *)
	List.rev (List.rev_map (fun (time, is_recovery) ->
		if is_recovery then
			incr num_up
		else
			decr num_up;
		(*assert (!num_up >= 0);*)
		(time, !num_up)
		) sorted_evts)

let avg_num_nodes_up trace =
	let total_uptime = ref 0.0 in
	for i = 0 to trace.n - 1 do
		total_uptime := !total_uptime +. sum_floats (extract_sessiontimes trace.events.(i))
		done;
	!total_uptime /. length trace

(* The minimum number of nodes up at any time in the trace, excluding the
 * first and last 0.1% of the trace where data is likely to be inaccurate
 * as measurements were probably started and stopped on various nodes
 * at slightly different times. *)
let min_nodes_up trace =
	let s = start_time trace in
	let t = end_time trace in
	let len = t -. s in
	let s2 = s +. 0.001 *. len in
	let t2 = t -. 0.001 *. len in
	(*printf "s=%g, s2=%g, t2=%g, t=%g\n" s s2 t2 t;*)
	let nnul = List.filter (fun (time, num_up) ->
		s2 <= time && time <= t2) (num_nodes_up_log trace) in
	List.fold_left (fun min_n (t,m) -> (
		(*if m < min_n then
			printf "Time %g: found new min of %d\n" t m;*)
		min min_n m)) trace.n nnul
