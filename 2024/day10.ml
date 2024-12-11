let explode s =
  let rec inner l i =
    if i < 0 then l else inner (s.[i] :: l) (i-1)
  in
  inner [] (String.length s - 1);;

let find_char c l =
  let rec inner c l i =
    match l with
    | [] -> []
    | a::b -> if a = c then i :: (inner c b (i+1)) else (inner c b (i+1))
  in
  inner c l 0;;

let parse file =
  let inp = In_channel.with_open_text file In_channel.input_all in
  let split = String.split_on_char '\n' inp in
  let perfecto = List.rev split |> List.tl |> List.rev in
  let parsedinp = List.map explode perfecto in
  List.map (List.map (fun x -> (Char.code x - Char.code '0'))) parsedinp;;

let get inp (x,y) = List.nth (List.nth inp x) y;;

let pp_tup (a,b) = Printf.printf "(%d,%d); " a b;;

let rec pp_tup_list l = match l with
  | [] -> print_newline ()
  | (a,b)::v -> Printf.printf "(%d,%d); " a b; pp_tup_list v;;

let traverse (x,y) inp =
  let len_lines = List.length inp in
  let len_nums = List.hd inp |> List.length in
  let rec inner (x,y) inp l =
    (*pp_tup (x,y);*)
    let num = get inp (x,y) in
    if num = 9 then (x,y)::l else
      (
        let l_a = ref [] in
        let l_b = ref [] in
        let l_c = ref [] in
        let l_d = ref [] in
        if x > 0 && get inp (x-1,y) = (num+1) then
          l_a := inner (x-1,y) inp l;
        if y > 0 && get inp (x,y-1) = (num+1) then
          l_b := inner (x,y-1) inp l;
        if x < (len_lines - 1) && get inp (x+1,y) =
          (num+1) then l_c := inner (x+1,y) inp l;
        if y < (len_nums - 1) && get inp (x,y+1) =
          (num+1) then l_d := inner (x,y+1) inp l;
        List.append !l_a !l_b |> List.append !l_c |> List.append !l_d
      )
  in
  inner (x,y) inp [];;

let proper_parse ill =
  let rec inner acc line l =
    match l with
    | [] -> acc
    | x::v -> inner ((line,x)::acc) line v
  in
  let a = List.mapi (fun i v -> inner [] i v) ill in
  List.concat a;;

(* Part 1 *)

module S = Set.Make(struct
  type t = int * int
  let compare = compare
end);;

let part1 file =
  let inp = parse file in
  let zeros = List.map (find_char 0) inp |> proper_parse in
  (*pp_tup_list zeros;*)
  let l = List.map (fun x -> traverse x inp) zeros in
  List.fold_left (fun acc x ->
    let s = S.of_list x in
    acc + (List.length (S.to_list s))
  ) 0 l;;

(*print_int (part1 "inp.txt")*)

(* Part 2 *)

let part2 file =
  let inp = parse file in
  let zeros = List.map (find_char 0) inp |> proper_parse in
  (*pp_tup_list zeros;*)
  let l = List.map (fun x -> traverse x inp) zeros in
  List.fold_left (fun acc x ->
    acc + (List.length x)
  ) 0 l;;

print_int (part2 "inp.txt")
