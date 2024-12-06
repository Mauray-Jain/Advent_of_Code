let find_char c l =
  let rec inner c l i =
    match l with
    | [] -> []
    | a::b -> if a = c then i :: (inner c b (i+1)) else (inner c b (i+1))
  in
  inner c l 0;;

let find_guard l =
  let rec inner c i l =
    match l with
    | [] -> None
    | a::b -> if a = c then Some i else inner c (i+1) b
  in
  let rec last_inner c i l =
    match l with
    | [] -> assert false
    | x::v -> match x with
      | None -> last_inner c (i+1) v
      | Some a -> (i,a)
  in
  let optl = List.map (inner '^' 0) l in
  last_inner '^' 0 optl;;

let explode s =
  let rec inner l i =
    if i < 0 then l else inner (s.[i] :: l) (i-1)
  in
  inner [] (String.length s - 1);;

let rec pp_list l = match l with
  | [] -> print_newline ()
  | x::v -> print_int x; print_string "; "; pp_list v;;

let rec pp_tup_list l = match l with
  | [] -> print_newline ()
  | (a,b)::v -> Printf.printf "(%d,%d); " a b; pp_tup_list v;;

let parse file =
  let inp = In_channel.with_open_text file In_channel.input_all in
  let x = String.split_on_char '\n' inp in
  let l = List.rev (List.tl (List.rev x)) in
  let exploded_l = List.map explode l in
  let ind = List.map (find_char '#') exploded_l in
  let rec convert l i =
    match l with
    | [] -> []
    | a::b -> List.append (List.map (fun y -> (i,y)) a) (convert b (i+1))
  in
  let guard_pos = find_guard exploded_l in
  let obs = convert ind 0 in
  ((List.length l, List.length (List.nth exploded_l 0)), obs, guard_pos);;

let turn dirn =
  match dirn with
  | (-1,0) -> (0,1)
  | (0,1) -> (1,0)
  | (1,0) -> (0,-1)
  | (0,-1) -> (-1,0)
  | _ -> assert false;;

let move_n_add len_lines len_chars obs guard =
  let rec inner len_lines len_chars obs guard dirn l =
    let (x,y) = dirn in
    let (old_x, old_y) = guard in
    let (new_x, new_y) = (old_x + x, old_y + y) in
    if (new_x >= len_lines) || (new_x < 0) || (new_y >= len_chars) || (new_y < 0)
    then l else
      let new_pos = (new_x, new_y) in
      if List.mem new_pos obs then
        inner len_lines len_chars obs guard (turn dirn) l
      else
        inner len_lines len_chars obs new_pos dirn (new_pos :: l)
  in
  inner len_lines len_chars obs guard (-1,0) [guard];;

module S = Set.Make(struct
  type t = int * int
  let compare = compare
end);;

(* Part 1 *)

let part1 file =
  let ((len_lines, len_chars), obs, guard) = parse file in
  let l = move_n_add len_lines len_chars obs guard in
  let s = S.of_list l in
  (*pp_tup_list (S.to_list s);*)
  List.length (S.to_list s);;

(*print_int (part1 "inp.txt")*)

(* Part 2 *)

let loop_checker len_lines len_chars obs guard =
  let rec inner len_lines len_chars obs guard dirn l =
    let (x,y) = dirn in
    let (old_x, old_y) = guard in
    let (new_x, new_y) = (old_x + x, old_y + y) in
    let new_pos = (new_x, new_y) in
    if List.mem (new_pos, dirn) l then true
    else if (new_x >= len_lines) || (new_x < 0) || (new_y >= len_chars) || (new_y < 0)
    then false
    else if List.mem new_pos obs then
      inner len_lines len_chars obs guard (turn dirn) l
    else
      inner len_lines len_chars obs new_pos dirn ((new_pos, dirn) :: l)
  in
  inner len_lines len_chars obs guard (-1,0) [(guard, (-1,0))];;

let part2 file =
  let ((len_lines, len_chars), obs, guard) = parse file in
  let l = move_n_add len_lines len_chars obs guard in
  let s = S.of_list l in
  let path = S.remove guard s in
  let rec inner path =
    match path with
    | [] -> 0
    | (a,b)::v -> (if (loop_checker len_lines len_chars ((a,b)::obs) guard) then 1 else 0)
      + (inner v)
  in
  inner (S.to_list path);;

print_int (part2 "inp.txt")
