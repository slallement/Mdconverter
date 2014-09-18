open Batteries;;
open Printf;;

(* Constant and types *)
type block_state = 
	| Not_matched
	| Begin_match
	| Matched
	| End_match

let blank = "[ \r\t]*";; (* cst useful *)
let regex_title = Str.regexp ("^" ^ blank ^ "\\(#+\\)" ^ blank ^ "\\(.*\\)");;
let regex_bold = Str.regexp " \\*\\(.*\\)\\*";;
let regex_italic = Str.regexp " _\\(.*\\)_";;
let regex_list_item = Str.regexp ("^" ^ blank ^ "[*-]" ^ blank ^ "\\(.*\\)");;
let regex_image = Str.regexp "!\\[\\(.*\\)\\](\\(.*\\))";;
let regex_table = Str.regexp ("" ^ blank ^ "|\\(.*\\)|" ^ blank ^ "");;

let ending_string = "EOF\n";

(* Module signature to match to make a format for converting the markdown 
format to your own format (with simples rules) *)
module type RULE_TYPE = 
sig
	val make_title : int -> string -> string
	(*val format_text : Str.regexp -> Str.regexp -> string -> string*)
	val format_list : block_state -> string -> string
	val format_table : block_state -> string -> string
	val begin_par : string
	val end_par : string
	val replace_italic : string
	val replace_bold : string
	val replace_image : string
end;;

(* Module to make the conversion of a string channel *)
module Apply_rule = 
functor (Rule: RULE_TYPE) ->
struct
	
	let format_text text =
		let text = Str.global_replace regex_bold Rule.replace_bold text in
		let text = Str.global_replace regex_italic Rule.replace_italic text in
		let text = Str.global_replace regex_image Rule.replace_image text in
		text;;
		
	let process_text_formatting text =
		format_text text;;
		
	(* title *)
	let get_title_content text =
		let r = regex_title in
		let s = Str.string_match r text 0 in
		if s then 
			let match1 = (Str.matched_group 1 text) in
			let match2 = (Str.matched_group 2 text) in
			
			let title_level = String.length match1 in
			let result = Rule.make_title title_level match2 in 
			(true, result)
		else
			(false, text);;
		
	(* list processing *)
	let process_text =
		let in_list = ref false in
		let in_table = ref false in
		let in_paragraph = ref false in
		
		(* list *)
		let get_list_content text =
			let r = regex_list_item in
			let is_list = Str.string_match r text 0 in
			match (is_list,!in_list) with
				| (false, false) -> (Not_matched, text)
				| (false, true)  -> ignore (in_list := false); (End_match, text)
				| (true,  true)  -> (Matched, Str.matched_group 1 text)
				| (true,  false) -> ignore (in_list := true);
								(Begin_match, Str.matched_group 1 text) in
								
		(* table *)
		let get_table_content text =
			let r = regex_table in
			let is_matched = Str.string_match r text 0 in
			match (is_matched,!in_table) with
				| (false, false) -> (Not_matched, text)
				| (false, true)  -> ignore (in_table := false); (End_match, text)
				| (true,  true)  -> (Matched, Str.matched_group 1 text)
				| (true,  false) -> ignore (in_table := true);
								(Begin_match, Str.matched_group 1 text) in
		
		let rec process_text_fun text =
			
			(* title *)
			let (in_title, text) = get_title_content text in
			(* list *)
			let (current_block_state, text) = (get_list_content text) in
			(* table *)
			let (current_table_state, text) = (get_table_content text) in
			
			let test_no_paragraph = !in_list || in_title || !in_table in
			
			(* paragraph *)
			let text = 
				(* some correction to add here *)
				if !in_paragraph = false && test_no_paragraph = false then (
						Rule.begin_par^text
				) else
					if text = ending_string && !in_paragraph = true then (
						ignore (in_paragraph := false);
						Rule.end_par
					) else
						text^"" (* space here normally ? (!) *)
				in

			let text = Rule.format_list current_block_state text in
			let text = Rule.format_table current_table_state text in
			
			(* paragraph end *)
			match (!in_paragraph, test_no_paragraph) with
				| (false, false) -> ignore (in_paragraph := true); text
				| (false, true) | (true, false)	-> text
				| (true, true) -> ignore (in_paragraph := false);
										Rule.end_par^"\n"^text
			
		in
		process_text_fun;;

	(* apply the rules for converting the string text *)
	let convert_markdown text =
		process_text text 
		|> process_text_formatting;;
		
	let process_line output line  = 
		let text = (convert_markdown line) in
		fprintf output "%s\n" text;;

	(* Function to use to convert a markdown channel input to an output *)
	let convert_file input_file_name output_channel =
		try
			let filelines = File.lines_of input_file_name in
				Enum.iter (process_line output_channel) filelines;
				process_line output_channel ending_string; (* to end the last paragraph *)
			true;
		with e -> begin
			print_endline ("\tERROR: " ^ (Printexc.to_string e));
			print_endline  "\t -> No file was written";
			(*ignore (raise e);*)
			false;
		end;;
end;;
(* ------------------------------------------------------------------------- *)

(* For html conversion *)
module Rule_HTML : RULE_TYPE =
struct
	let tag_surround tag text =
		"<"^tag^">"^text^"</"^tag^">"
		
	let begin_par = "<p>"
	let end_par = "</p>"
	
	let replace_bold =
		let tag = "strong" in
		" "^(tag_surround tag "\\1");;
		
	let replace_italic = 
		let tag = "em" in
		" "^(tag_surround tag "\\1") ;;
	
	let replace_image = "<img alt=\"\\1\" src=\"\\2\" />"

	let make_title title_level content =
		let s = string_of_int(title_level) in 
		("<h"^s^">"^content^"</h"^s^">")
		
	let format_list block_state text = match block_state with
		| Not_matched -> text
		| Begin_match  -> "<ul>\n\t<li>" ^ text ^ "</li>"
		| Matched     -> "\t<li>" ^ text ^ "</li>"
		| End_match    -> "</ul>\n" ^ text
		
	let format_table state text = 
		let splitting text =
			String.concat "</td><td>" (Str.split (Str.regexp "|") text)
		in
		match state with
		| Not_matched -> text
		| Begin_match  -> "<table>\n\t<tr><td>" ^ (splitting text) ^ "</td></tr>"
		| Matched     -> "\t<tr><td>" ^ (splitting text) ^ "</td></tr>"
		| End_match    -> "</table>\n" ^ (splitting text)	
	
end

(* For LaTeX conversion*)
module Rule_Latex : RULE_TYPE =
struct

	let begin_par = ""
	let end_par = "\\\\"
	
	let make_title title_level content =
		let begin_title =
			match title_level with
				| 0 -> "section"
				| x -> ( (String.repeat "sub" x) ^ "section" ) in
		"\\"^begin_title ^ "{" ^ content ^ "}"
		
	let replace_bold = " \\textbf{\\1}"
	let replace_italic = " \\emph{\\1}"
	let replace_image = "\\begin{figure}[ht!]\n\\includegraphics[scale=1]{\\2}\n\\caption{\\1}\n\\end{figure}"
			
	let format_list block_state text = match block_state with
		| Not_matched -> text
		| Begin_match  -> "\\begin{itemize}\n\t\\item " ^ text ^ ""
		| Matched     -> "\t\\item " ^ text ^ ""
		| End_match    -> "\\end{itemize}\n" ^ text
	
	let format_table state text = text
	
	let format_table state text = 
		let count_substring str sub =
			let sub_len = String.length sub in
			let len_diff = (String.length str) - sub_len
			and reg = Str.regexp_string sub in
			let rec aux i n =
			if i > len_diff then n else
			  try
				let pos = Str.search_forward reg str i in
				aux (pos + sub_len) (succ n)
			  with Not_found -> n
			in
			aux 0 0
		in
		let splitting text =
			String.concat "</td><td>" (Str.split (Str.regexp "|") text)
		in
		let nbcol = count_substring text "|" in
		match state with
		| Not_matched -> text
		| Begin_match  -> "\begin{tabular}{" 
				^ (String.repeat " c" nbcol)
				^"}\n\t<tr><td>" ^ (splitting text) ^ "</td></tr>"
				(* todo *)
		| Matched     -> "\t<tr><td>" ^ (splitting text) ^ "</td></tr>"
		| End_match    -> "</table>\n" ^ (splitting text)	
		

end