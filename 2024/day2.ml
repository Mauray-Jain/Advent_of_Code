(* Input and parse *)

let rm_tail l = List.rev (List.tl (List.rev l))

let parse_inp file =
  let content = In_channel.with_open_text file In_channel.input_all in
  let l = String.split_on_char '\n' content in
  let strs = List.map (String.split_on_char ' ') l in
  let filtered = List.map (List.filter (fun x -> not (String.equal x ""))) strs in
  let almost_there = List.map (List.map Stdlib.int_of_string) filtered in
  rm_tail almost_there;;

let rec diffs inp = match inp with
  | [] -> []
  | [_] -> []
  | x::v -> (x - (List.hd v)) :: (diffs v);;

(* Part 1 *)

let issafe l = (List.for_all (fun x -> ((x <= 3) && (x >= 1))) l) ||
  (List.for_all (fun x -> ((x <= -1) && (x >= -3))) l);;

let print_bool bb = match bb with
  | true -> print_string "true"
  | false -> print_string "false";;

let rec pp_list l = match l with
  | [] -> print_newline ()
  | x::v -> print_int x; print_string "; "; pp_list v;;

let issafe_complete l = issafe (diffs l);;

let part1 file =
  let l = parse_inp file in
  let diff = List.map diffs l in
  (*List.iter pp_list diff;*)
  let bools = List.map issafe diff in
  (*List.iter print_bool bools;;*)
  let ans = List.length (List.filter (fun x -> x) bools) in
  print_int ans;;
(*part1 "day2.txt"*)

(* Part 2 *)
let thresh_pos x = (x >= 1) && (x <= 3);;
let thresh_neg x = (x >= -3) && (x <= -1);;

let check_safe_on_level og idx =
  (let audited_l1 = List.filteri (fun i _ -> i <> idx) og in
    let audited_l2 = List.filteri (fun i _ -> i <> (idx+1)) og in
    (issafe_complete audited_l1) || (issafe_complete audited_l2));;

let rec inner thresh idx l og =
  match l with
  | [] -> true
  | x::v -> if thresh x then inner thresh (idx+1) v og
    else (check_safe_on_level og idx);;

let issafe2 og =
  let l = diffs og in
  let baksoditime = (List.length (List.filter (fun x -> x < 0) l)) in
  let secnd = (List.length (List.filter (fun x -> x > 0) l)) in
  let thresh = if secnd > baksoditime then thresh_pos else thresh_neg in
  inner thresh 0 l og;;

let rec brute idx ans l  =
  if idx >= (List.length l) then ans else
    let audited = (List.filteri (fun i _ -> idx <> i) l) in
    if issafe_complete audited then true else brute (idx+1) false l;;

let part2 file =
  let l = parse_inp file in
  (*pp_list (List.nth diff 2);*)
  (*let test = issafe2 0 (List.nth diff 2) in*)
  (*print_bool test;;*)
  let ans = List.map issafe2 l in
  (*let ans = List.map (brute 0 false) l in*)
  print_int (List.length (List.filter (fun x -> x) ans));;

part2 "day2.txt"
