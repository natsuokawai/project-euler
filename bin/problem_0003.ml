open Core
open Int64

let isqrt n = Int64.to_float n |> Float.sqrt |> Float.to_int64

let is_prime n : bool =
  let rec is_dividable d =
    d <= isqrt n && (rem n 2L = 0L || rem n d = 0L || is_dividable (d + 1L))
  in

  if n <= 1L then false else if n = 2L then true else not (is_dividable 2L)

let rec largest_prime_factor n x =
  if is_prime x && rem n x = 0L then x else largest_prime_factor n (x - 1L)

let () =
  let target = 600851475143L in
  print_endline (Int64.to_string (largest_prime_factor target (target / 2L)))
