let parse file =
  let inp = In_channel.with_open_text file In_channel.input_all in
  let l = String.split_on_char '\n' inp in
  List.rev (List.tl (List.rev l));;

let find_chr c s =
  let rec inner c s idx =
    match String.index_opt s c with
    | None -> []
    | Some a -> (idx+a) :: (inner c (String.sub s (a+1) ((String.length s) - a - 1)) (idx+a+1))
  in
  inner c s 0;;

let rec pp_list l = match l with
  | [] -> print_newline ()
  | x::v -> print_int x; print_string "; "; pp_list v;;

let str_get_opt str n =
  try Some (String.get str n) with
    Invalid_argument _ -> None;;

(*List.iter pp_list (List.map (find_chr 'X') (parse "inp.txt"))*)

(* Part 1 *)

let part1 file =
  let inp = parse file in
  let str1 = "XMAS" in
  let str2 = "SAMX" in
  let dirn = [(0,1);(1,0);(1,1);(-1,1)] in
  let x_ind = List.map (find_chr 'X') inp in
  let s_ind = List.map (find_chr 'S') inp in

  let rec make_str inp line from len dirn =
    let (a,b) = dirn in
    match len with
    | 0 -> ""
    | _ ->  (
      if line < 0 then " " else
        match List.nth_opt inp line with
        | None -> " "
        | Some str -> (
          match str_get_opt str from with
          | None -> " "
          | Some c -> String.make 1 c
        ) ^ (make_str inp (line+a) (from+b) (len-1) dirn)
    )
  in
  
  let rec find_match str line ind dirn =
    match dirn with
    | [] -> 0
    | (a,b)::v -> (
      let string = make_str inp line ind 4 (a,b) in
      (*print_string "line: "; print_int line;*)
      (*print_string " ind: "; print_int ind;*)
      (*print_newline ();*)
      (*Printf.printf "dirn: (%d, %d) " a b;*)
      (*print_endline string;*)
      (if String.equal string str then 1 else 0) + (find_match str line ind v)
    )
  in

  let rec find_matches str line indices dirn =
    match indices with
    | [] -> 0
    | ind::rest -> (find_match str line ind dirn) + (find_matches str line rest dirn)
  in

  let rec inner str line ind =
    match ind with
    | [] -> 0
    | x::v -> (find_matches str line x dirn) + (inner str (line+1) v)
  in
  (inner str1 0 x_ind) + (inner str2 0 s_ind);;

(*print_int (part1 "inp.txt")*)


(* Part 2 *)

let part2 file =
  let inp = parse file in
  let a_ind = List.map (find_chr 'A') inp in

  let is_x ind line =
    if line = 0 then false else
      match (List.nth_opt inp (line-1), List.nth_opt inp (line+1)) with
      | (Some up, Some down) -> (
        match (str_get_opt up (ind-1),
          str_get_opt up (ind+1),
          str_get_opt down (ind-1),
          str_get_opt down (ind+1)) with
        | (Some a, Some b, Some c, Some d) -> (
          ((a = 'M' && d = 'S') || (a = 'S' && d = 'M')) &&
          ((b = 'M' && c = 'S') || (b = 'S' && c = 'M'))
        )
        | _ -> false
      )
      | _ -> false
  in

  let rec find_x indices line =
    match indices with
    | [] -> 0
    | x::v -> (if is_x x line then 1 else 0) + (find_x v line)
  in

  let rec find_xs ind line =
    match ind with
    | [] -> 0
    | x::v -> (find_x x line) + (find_xs v (line+1))
  in

  find_xs a_ind 0;;

print_int (part2 "inp.txt")
