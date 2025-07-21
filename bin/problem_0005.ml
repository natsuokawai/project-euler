let smallest_multiple () =
  let rec seek n =
    let rec divisible_to_max n x max =
      if n mod x <> 0 then false
      else if x = max then true
      else divisible_to_max n (x + 1) max
    in
    (* 2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 = 9699690 *)
    if divisible_to_max n 1 20 then n else seek (n + 9699690)
  in
  seek (2520 * 11 * 13 * 17 * 19)

let () = Printf.printf "%d\n" (smallest_multiple ())
