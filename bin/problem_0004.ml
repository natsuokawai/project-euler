open Core

let is_palidromic_num n =
  let s = Int.to_string n in
  String.equal s (String.rev s)

let largest_palindrome_product () =
  let rec search_i i =
    if i = 0 then None
    else if is_palidromic_num i then
      let rec search_j j =
        if j < 100 then None
        else if i mod j = 0 && i / j >= 100 && i / j <= 999 then Some i
        else search_j (j - 1)
      in
      match search_j 999 with Some x -> Some x | None -> search_i (i - 1)
    else search_i (i - 1)
  in
  search_i (999 * 999)

let () =
  match largest_palindrome_product () with
  | Some x -> Printf.printf "%d\n" x
  | None -> Printf.printf "Not found."
