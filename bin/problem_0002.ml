open Core

let even_fibonacci_numbers max_num =
  let fib_seq = Seq.unfold (fun (a, b) ->
    if a >= max_num then
      None
    else
      Some (a, (b, a + b))
  )   (1  ,  2)
  in
  
  fib_seq
  |> Seq.filter (fun n -> n mod 2 = 0)
  |> Seq.fold_left (+) 0

let () = printf "%d\n" (even_fibonacci_numbers 4_000_000)
