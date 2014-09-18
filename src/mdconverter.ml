open Batteries;;
open Printf;;
open Arg;;
(* CONSTANTS ---------------------------------------------------------------- *)
let prog_title = "MdConverter";;

(* ARGS --------------------------------------------------------------------- *)
let opt_file_name = ref "file.txt";;
let opt_file_name_output = ref "out";;
let opt_html    = ref false;;
let opt_latex   = ref false;;
let opt_verbose = ref false;;
let opt_std     = ref false;;

let speclist = 
	[("-f", Arg.Set_string opt_file_name_output, "Name of the output file");
	("-h", Arg.Set opt_html, "Convert in HTML");
	("-l", Arg.Set opt_latex, "Convert in LaTeX");
	("-tex", Arg.Set opt_latex, "Convert in LaTeX");
	("-s", Arg.Set opt_std, "Print in the standard output");
	("-v", Arg.Set opt_verbose, "Verbose mode");
	("--html", Arg.Set opt_html, "Convert in HTML");
	("--latex", Arg.Set opt_latex, "Convert in LaTeX"); ];;
	
let usage_msg = prog_title^" is a markdown converter, it can produce"
				^ " .tex or .html files.\n"
				^ "Usage: "^"\tMdConverter.exe [OPTION] [INPUT_FILE]";;

let parse_args = (* anonymous arguments = file to convert*)
	Arg.parse speclist 
		(fun anon ->
			opt_file_name := anon) usage_msg;;
	
(* MAIN --------------------------------------------------------------------- *)
let output_choice file_name format_name format_extension opt_std_out =
	if not opt_std_out then (
		if !opt_verbose then
			ignore (print_endline ("> convert to the " ^ format_name 
				^ " file \""^file_name ^ "." ^ format_extension ^ "\"") );
		open_out (file_name ^ "." ^ format_extension);
	) else 
		stdout;
	;;
	
let () = 
	parse_args;
	if not (!opt_html || !opt_latex) then (
		Arg.usage speclist usage_msg;
		flush_all ();
	);
	if not !opt_std && !opt_verbose then (
		print_endline (" --- "^prog_title^" --- ");
		print_endline "Input file : ";
	);
	if !opt_html then (
		let channel = 
			output_choice !opt_file_name_output "html" "htm" !opt_std in
		let module Html_r = Rules.Apply_rule(Rules.Rule_HTML) in
		ignore (Html_r.convert_file !opt_file_name channel);
		if not !opt_std then close_out channel;
	);
	if !opt_latex then (
		let channel =
			output_choice !opt_file_name_output "LaTeX" "tex" !opt_std in
		let module Latex_r = Rules.Apply_rule(Rules.Rule_Latex) in
		ignore (Latex_r.convert_file !opt_file_name channel);
		if not !opt_std then close_out channel;
	);
	if not !opt_std && !opt_verbose then (
		print_endline " ------------------- ";
	);
	
(* --- *)
