let rec sum_of_squares num =
  if num = 0 then 0 else (num * num) + sum_of_squares (num - 1)

let sum num = num * (num + 1) / 2

let square_of_sum num =
  let sum_result = sum num in
  sum_result * sum_result

let limit = 100
let diff = square_of_sum limit - sum_of_squares limit
let () = Printf.printf "%d\n" diff
