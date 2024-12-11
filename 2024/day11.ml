(*https://github.com/LeedsJohn/OCaml-Advent-Of-Code/blob/main/lib/solutions/problem_2024_11.ml*)

let parse file =
  let inp = In_channel.with_open_text file In_channel.input_all in
  let ss = String.split_on_char '\n' inp |> List.hd in
  let l = String.split_on_char ' ' ss |> List.map Stdlib.int_of_string in
  l;;

let split_n n =
  let str = Stdlib.string_of_int n in
  let strlen = String.length str in
  let n1 = String.sub str 0 (strlen/2) in
  let n2 = String.sub str (strlen/2) (strlen/2) in
  [Stdlib.int_of_string n1; Stdlib.int_of_string n2];;

let get_next n =
  if n = 0 then [1]
  else if (Stdlib.string_of_int n |> String.length) mod 2 = 0 then split_n n
  else [n*2024];;

let doblinkyblinky stones num =
  let mem = Hashtbl.create 1024 in
  let rec inner stone num =
    if num = 0 then 1 else
      match Hashtbl.find_opt mem (stone,num) with
      | Some v -> v
      | None ->
        let next = get_next stone in
        let res = List.fold_left (fun acc x -> acc + (inner x (num-1))) 0 next in
        Hashtbl.add mem (stone,num) res;
        res
    in
  List.fold_left (fun acc stone -> acc + inner stone num) 0 stones;;

(* Part 1 *)

let part1 file =
  let inp = parse file in
  doblinkyblinky inp 25;;

(*print_int (part1 "inp.txt")*)

(* Part 2 *)

let part2 file =
  let inp = parse file in
  doblinkyblinky inp 75;;

print_int (part2 "inp.txt")
