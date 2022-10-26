(** Pretty printing utility functions *)

let format_list pr ppf xs =
  let rec format ppf = function
    | [] -> ()
    | [x] -> Format.fprintf ppf "%a" pr x
    | x :: xs ->
        Format.fprintf ppf "%a, %a" pr x format xs
  in
  Format.fprintf ppf "[%a]" format xs

let print_of_format pr x out_ch = Format.fprintf (Format.formatter_of_out_channel out_ch) "%a@?" pr x
       
let string_of_format pr t = pr Format.str_formatter t; Format.flush_str_formatter ()

let print_list out_ch pr xs = print_of_format (format_list pr) xs out_ch

let list_to_string pr xs = string_of_format (format_list pr) xs
    
let pair_to_string pts1 pts2 (x, y) =
  Printf.sprintf "(%s, %s)" (pts1 x) (pts2 y)
    
let plist_to_string =
  list_to_string (fun ppf (x, s) -> Format.fprintf ppf "(%d, \"%s\")" x s)

let slist_to_string =
  list_to_string (fun ppf s -> Format.fprintf ppf "\"%s\"" s)

let ilist_to_string =
  list_to_string (fun ppf -> Format.fprintf ppf "%d")
