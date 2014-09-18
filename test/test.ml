open OUnit2;;

(* Used to do unit testing *)

let test1_1 test_ctxt = 
	assert_equal 
		(Rules.Rule_Latex.make_title 1 "content")
		"\\subsection{content}"
	;;
	
let test1_2 test_ctxt =
	let module Latex_r = Rules.Apply_rule(Rules.Rule_Latex) in
	assert_equal 
		(Latex_r.format_text "abc *def* _ghi_")
		"abc \\textbf{def} \\emph{ghi}"
	;;
	
let test1_3 test_ctxt =
	let module Latex_r = Rules.Apply_rule(Rules.Rule_Latex) in
	assert_equal ~printer:(fun x -> x)
		(Latex_r.format_text "abc ![caption](image/path.jpg)")
		("abc " ^ "\\begin{figure}[ht!]\n\\includegraphics[scale=1]{image/path.jpg}\n\\caption{caption}\n\\end{figure}")
	;;
	
let test1_4 test_ctxt =
	let lines = [	"test";
					" - item1";
					" - item2";
					"testend"] in
	let module Latex_r = Rules.Apply_rule(Rules.Rule_Latex) in
	let res = List.map Latex_r.convert_markdown lines in
	let comp = 
		[
		"test\\\\\n";
		"\\begin{itemize}\n\t\\item item1";
		"\t\\item item2";
		"\\end{itemize}\ntestend"] in
	let res_line = String.concat "" res in
	let comp_line = String.concat "" comp in
	assert_equal ~printer:(fun x -> x)
		res_line
		comp_line
	;;
	
let test1_5 test_ctxt =
	let lines = [	"test";
					"|c1|c2|";
					"|c3|c4|";
					"testend"] in
	let comp = 
		[
		"<p>test";
		"</p>\n<table>\n\t<tr><td>c1</td><td>c2</td></tr>";
		"\t<tr><td>c3</td><td>c4</td></tr>";
		"</table>\n<p>testend"] in
	let module Html_r = Rules.Apply_rule(Rules.Rule_HTML) in
	let res = List.map Html_r.convert_markdown lines in
	let res_line = String.concat "\n" res in
	let comp_line = String.concat "\n" comp in
	assert_equal ~printer:(fun x -> x)
		comp_line
		res_line
	;;
	
let pack = 
	"test rules.ml" >:::
	[
	"test title latex" >:: test1_1;
	"test formatting text latex" >:: test1_2;
	"test formatting image latex" >:: test1_3;
	"test formatting list latex" >:: test1_4;
	"test formatting table html" >:: test1_5;
	]
	
let () =
	run_test_tt_main pack
;;