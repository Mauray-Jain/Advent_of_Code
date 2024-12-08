let pop_hashtbl hashtbl line l =
  let rec inner hashtbl l i line =
    match l with
    | [] -> ()
    | a::b -> (if a <> '.' then Hashtbl.add hashtbl a (line, i)); inner hashtbl b (i+1) line
  in
  inner hashtbl l 0 line;;

let explode s =
  let rec inner l i =
    if i < 0 then l else inner (s.[i] :: l) (i-1)
  in
  inner [] (String.length s - 1);;

let rec pp_tup_list l = match l with
  | [] -> print_newline ()
  | (a,b)::v -> Printf.printf "(%d,%d); " a b; pp_tup_list v;;

let rec pp_tup_tup_list l = match l with
  | [] -> print_newline ()
  | ((a,b),(c,d))::v -> Printf.printf "((%d,%d), (%d,%d)); " a b c d; pp_tup_tup_list v;;

let pp_tup (a,b) = Printf.printf "(%d,%d); " a b

let parse file =
  let inp = In_channel.with_open_text file In_channel.input_all in
  let x = String.split_on_char '\n' inp in
  let l = List.rev (List.tl (List.rev x)) in
  let exploded_l = List.map explode l in
  let coords = Hashtbl.create 1024 in
  List.iteri (fun i l -> pop_hashtbl coords i l) exploded_l;
  ((List.length l, String.length (List.hd l)), coords);;

let gen_perm l =
  let rec gen_for_one elem l =
    List.map (fun x -> (elem, x)) l
  in
  let rec inner l =
    match l with
    | [] -> []
    | x::v -> List.append (gen_for_one x v) (inner v)
  in
  inner l;;

module S = Set.Make(struct
  type t = int * int
  let compare = compare
end);;


(*let (_, coords) = parse "inp.txt" in*)
(*let zer = Hashtbl.find_all coords '0' in*)
(*let perms = gen_perm zer in*)
(*pp_tup_list zer;*)
(*pp_tup_tup_list perms;*)

(* Part 1 *)

let find_antinodes1 ((x1,y1),(x2,y2)) (len_lines, len_chars) =
  let antix1 = 2 * x1 - x2 in
  let antix2 = 2 * x2 - x1 in
  let antiy1 = 2 * y1 - y2 in
  let antiy2 = 2 * y2 - y1 in
  let coord1 = ref (Some (antix1, antiy1)) in
  let coord2 = ref (Some (antix2, antiy2)) in
  if (antix1 < 0) || (antix1 >= len_lines) || (antiy1 < 0) || (antiy1 >= len_chars) then
    coord1 := None;
  if (antix2 < 0) || (antix2 >= len_lines) || (antiy2 < 0) || (antiy2 >= len_chars) then
    coord2 := None;
  (!coord1, !coord2);;

let for_a_freq size set l =
  let perms = gen_perm l in
  let rec inner l acc = (
    match l with
    | [] -> acc
    | x::v -> (
      let antis = find_antinodes1 x size in
      let new_acc = (
        match antis with
      | (None, None) -> acc
      | (Some a, None) -> S.add a acc
      | (None, Some b) -> S.add b acc
      | (Some a, Some b) -> S.add a acc |> S.add b
      ) in
      inner v new_acc
      )
    ) in
  inner perms set;;

let rec compute_ans keys table size set =
  match keys with
  | [] -> set
  | key::v -> (
    let vals = Hashtbl.find_all table key in
    let val_set = for_a_freq size set vals in
    compute_ans v table size val_set
  );;

(*This was completely unnecessary*)
(*let rec rm_antenna keys table set =*)
(*  match keys with*)
(*  | [] -> set*)
(*  | x::v -> ( *)
(*    let val_set = S.of_list (Hashtbl.find_all table x) in*)
(*    rm_antenna v table (S.diff set val_set)*)
(*  );;*)

let part1 file =
  let (size, coords) = parse file in
  let ans = S.of_list [] in
  let keys = Hashtbl.to_seq_keys coords |> List.of_seq in
  let finol = compute_ans keys coords size ans in
  S.to_list finol |> List.length;;

(*print_int (part1 "inp.txt")*)

(* Part 2 *)

let find_antinodes2 ((x1,y1),(x2,y2)) (len_lines, len_chars) =
  let rec inner_left (x1,y1) (x2,y2) (len_lines, len_chars) l =
    let antix = 2 * x1 - x2 in
    let antiy = 2 * y1 - y2 in
    if (antix < 0) || (antix >= len_lines) || (antiy < 0) || (antiy >= len_chars) then l
    else (inner_left (antix,antiy) (x1,y1) (len_lines, len_chars) ((antix,antiy)::l))
    in
  let rec inner_right (x1,y1) (x2,y2) (len_lines, len_chars) l =
    let antix = 2 * x2 - x1 in
    let antiy = 2 * y2 - y1 in
    if (antix < 0) || (antix >= len_lines) || (antiy < 0) || (antiy >= len_chars) then l
    else (inner_right (x2,y2) (antix,antiy) (len_lines, len_chars) ((antix,antiy)::l))
    in
  List.append (inner_left (x1,y1) (x2,y2) (len_lines, len_chars) [])
  (inner_right (x1,y1) (x2,y2) (len_lines, len_chars) []);;

let for_a_freq2 size set l =
  let perms = gen_perm l in
  let rec inner l acc = (
    match l with
    | [] -> acc
    | x::v -> (
      let antinodes = find_antinodes2 x size in
      let new_acc = S.union acc (S.of_list antinodes) in
      inner v new_acc
      )
    ) in
  inner perms set;;

let rec compute_ans2 keys table size set =
  match keys with
  | [] -> set
  | key::v -> (
    let vals = Hashtbl.find_all table key in
    let val_set = for_a_freq2 size set vals in
    compute_ans2 v table size (S.union val_set set)
  );;

let part2 file =
  let (size, coords) = parse file in
  let ans = S.of_list [] in
  let keys = Hashtbl.to_seq_keys coords |> List.of_seq in
  let almost = compute_ans2 keys coords size ans in
  let rec get_mult_antenna table keys =
    match keys with
    | [] -> []
    | x::v -> List.append (if List.length (Hashtbl.find_all table x) > 1 then Hashtbl.find_all table x else []) (get_mult_antenna table v)
  in
  let finol = S.union almost (S.of_list (get_mult_antenna coords keys)) in
  (*pp_tup_list (S.to_list finol);*)
  S.to_list finol |> List.length;;

print_int (part2 "inp.txt")
