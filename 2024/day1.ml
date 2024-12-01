(* First ocaml pirogram *)

(*let split str = Scanf.sscanf str "%d %d" (fun x y -> (x,y));;*)

let rec split i l = match (List.nth l i) with
  | "" -> []
  | str -> (Scanf.sscanf str "%d %d" (fun x y -> (x,y))) :: split (i+1) l;;

let parse_content content =
  let lines = String.split_on_char '\n' content in
  let pairs = split 0 lines in
  List.split pairs;;

(* Part1 *)

let rec diff_n_sum tup = match tup with
  | ([], []) -> 0
  | (x::l1, y::l2) -> (abs (x-y)) + diff_n_sum (l1,l2)
  | _ -> assert false;;

let part1 content =
  let (l1,l2) = parse_content content in
  let sorted1 = List.sort Stdlib.compare l1 in
  let sorted2 = List.sort Stdlib.compare l2 in
  let answer = diff_n_sum (sorted1, sorted2) in
  print_int answer;;

(* Part 2 *)

let rec count x l = match l with
  | [] -> 0
  | hd::v -> (count x v) + (if hd = x then 1 else 0)

let rec score (l1,l2) = match l1 with
  | [] -> 0
  | x::v -> (x * (count x l2)) + score (v, l2)


let part2 content =
  let answer = score (parse_content content) in
  print_int answer;;


let ic = In_channel.open_text "day1.txt";;
let content = In_channel.input_all ic;;
In_channel.close ic;;
(*part1 content*)
part2 content
