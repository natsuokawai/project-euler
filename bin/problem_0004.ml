open Core

let is_palidromic_num n =
  let s = Int.to_string n in
  String.equal s (String.rev s)

let largest_palindrome_product () =
  let max_palindrome = ref 0 in

  for a = 999 downto 100 do
    for b = a downto 100 do
      let prod = a * b in
      if prod > !max_palindrome && is_palidromic_num prod then
        max_palindrome := prod
      else ()
    done
  done;

  !max_palindrome

let () = Printf.printf "%d\n" (largest_palindrome_product ())
