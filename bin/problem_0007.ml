open Core
open Int64

let isqrt n = Int64.to_float n |> Float.sqrt |> Float.to_int64

let is_prime n : bool =
  let rec is_divisible d =
    d <= isqrt n && (rem n 2L = 0L || rem n d = 0L || is_divisible (d + 1L))
  in
  if n <= 1L then false else if n = 2L then true else not (is_divisible 2L)

let nth_prime n =
  let rec search num c =
    let num_is_prime = is_prime num in
    if num_is_prime && c + 1L = n then num
    else search (num + 1L) (if num_is_prime then c + 1L else c)
  in
  search 2L 0L

let () = print_endline (Int64.to_string (nth_prime 10001L))
