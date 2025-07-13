open Core

let isqrt n = Int64.to_float n |> Float.sqrt |> Float.to_int64

let is_prime (n : Int64.t) : bool =
  let open Int64 in
  let rec is_dividable d =
    d < isqrt n && (rem n 2L = 0L || rem n d = 0L || is_dividable (d + 1L))
  in

  if n <= 1L then false else if n = 2L then true else not (is_dividable 2L)
