let split_list l idx =
  let rec inner_less l idx i =
    match l with
    | [] -> []
    | x::v -> if i < idx then x :: (inner_less v idx (i+1)) else []
  in
  let rec rest l idx i =
    match l with
    | [] -> []
    | x::v -> if idx <= i then v else (rest v idx (i+1))
  in
  (inner_less l idx 0, rest l idx 0);;

let split_rule rule =
  let l = String.split_on_char '|' rule in
  (Stdlib.int_of_string (List.nth l 0), Stdlib.int_of_string (List.nth l 1));;

let split_page page =
  List.filter_map Stdlib.int_of_string_opt (String.split_on_char ',' page);;

let parse file =
  let content = In_channel.with_open_text file In_channel.input_all in
  let l = String.split_on_char '\n' content in
  let l2 = List.rev (List.tl (List.rev l)) in
  let idx_split = (
    match List.find_index (fun x -> String.equal x "") l2 with
    | None -> assert false
    | Some a -> a
  ) in
  let (im_rules,im_pages) = split_list l2 idx_split in
  let rules = List.map split_rule im_rules in
  let pages = List.map split_page im_pages in
  (rules, pages);;

let rec pp_list l = match l with
  | [] -> print_newline ()
  | x::v -> print_int x; print_string "; "; pp_list v;;

let rec pp_rules l = match l with
  | [] -> print_newline ()
  | (a,b)::v -> Printf.printf "(%d,%d); " a b; pp_rules v;;

let satisy_rules rules page =
  let rec inner rules page ans =
    match rules with
    | [] -> ans
    | (a,b)::v -> (
      let idx1 = List.find_index (fun x -> x = a) page in
      let idx2 = List.find_index (fun x -> x = b) page in
      match (idx1, idx2) with
      | (Some a, Some b) -> if idx2 < idx1 then false else inner v page true
      | _ -> inner v page ans
    )
  in
  inner rules page false;;

(* Part 1 *)

let part1 file =
  let (rules, pages) = parse file in
  let inner rules page =
    if satisy_rules rules page then (List.nth page ((List.length page) / 2)) else 0
  in
  List.fold_left (fun x page -> x + (inner rules page)) 0 pages;;

(*print_int (part1 "inp.txt")*)

(* Part 2 *)

let page_comp rules a b =
  let rec inner rules a b =
    match rules with
    | [] -> 0
    | (c,d)::v -> (
        if (a,b) = (c,d) then -1 else if (a,b) = (d,c) then 1 else inner v a b
    )
  in
  inner rules a b;;

let part2 file =
  let (rules, pages) = parse file in
  let sorted page = List.sort (page_comp rules) page in
  let get_mid page = List.nth page ((List.length page) / 2) in
  let inner rules page =
    if satisy_rules rules page then 0 else (get_mid (sorted page))
  in
  List.fold_left (fun x page -> x + (inner rules page)) 0 pages;;

print_int (part2 "inp.txt")
