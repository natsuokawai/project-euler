open Core

let multiples_of_3_or_5 n =
    List.range 1 n
     |> List.filter ~f:(fun n -> n mod 3 = 0 || n mod 5 = 0)
     |> List.fold ~init:0 ~f:(+)

let () = printf "%d\n" (multiples_of_3_or_5 1000)
