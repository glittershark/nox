open! Core
include String

let quickcheck_generator = (String.gen' [@mode portable]) Char.gen_print

let to_string =
  let escape_string =
    String.Escaping.escape ~escapeworthy:[ '"'; '$'; '\\' ] ~escape_char:'\\'
    |> (Staged.unstage [@mode portable])
  in
  fun s ->
    if String.is_empty s
       || not
            (Char.is_alpha (String.nget s 0)
             && String.for_all s ~f:(fun char ->
               Char.is_alphanum char || List.mem ~equal:[%equal: char] [ '-'; '_' ] char)
            )
    then "\"" ^ escape_string s ^ "\""
    else s
;;
