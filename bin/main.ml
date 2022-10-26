open Hw06
open Util.Print

(* Add testing code here. To compile and run, execute:
   $ dune exec bin/main.exe
*)
 
  
let _ =
  (* `f x |> g`  is equivalent to `g (f x)`. 
     The operator |> is left-associative. 
  *)
  unzip [(1, "a"); (2, "b")] |>
  pair_to_string ilist_to_string slist_to_string |>
  print_endline
