open Core
open Int64

let isqrt n = Int64.to_float n |> Float.sqrt |> Float.to_int64

let is_prime n : bool =
  let rec is_dividable d =
    d <= isqrt n && (rem n 2L = 0L || rem n d = 0L || is_dividable (d + 1L))
  in
  if n <= 1L then false else if n = 2L then true else not (is_dividable 2L)

let rec largest_prime_factor n d =
  if not (is_prime d) then largest_prime_factor n (d + 1L)
  else if n = d then n
  else if rem n d = 0L then largest_prime_factor (n / d) d
  else largest_prime_factor n (d + 1L)

let () = print_endline (Int64.to_string (largest_prime_factor 600851475143L 2L))
