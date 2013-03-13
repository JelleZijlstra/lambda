(* Compiles a lambda expression into JavaScript *)
if Array.length Sys.argv <> 2 then
	(Printf.printf "Usage: %s file\n" Sys.argv.(0); exit 1);;

let com = Util.parse_file (Sys.argv.(1)) in
let result = Compile.compile com in
Printf.printf "%s\n" result
