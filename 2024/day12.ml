let explode s =
  let rec inner l i =
    if i < 0 then l else inner (s.[i] :: l) (i-1)
  in
  inner [] (String.length s - 1);;

let rec pp_tup_list l = match l with
  | [] -> print_newline ()
  | (a,b)::v -> Printf.printf "(%d,%d); " a b; pp_tup_list v;;

module S = Set.Make(
  struct
    type t = int * int
    let compare = compare
  end
);;

module SS = Set.Make(
  struct
    type t = char
    let compare = compare
  end
);;

let parse file =
  let inp = In_channel.with_open_text file In_channel.input_all in
  let split = String.split_on_char '\n' inp |> List.rev |> List.tl |> List.rev in
  let l = List.map explode split in
  let tbl1 = Hashtbl.create 1024 in
  let tbl2 = Hashtbl.create 1024 in
  List.iteri (fun i v ->
    List.iteri (fun j w -> Hashtbl.add tbl1 w (i,j)) v
  ) l;
  Hashtbl.to_seq_keys tbl1 |> SS.of_seq |> SS.to_list |>
  List.iter (fun x ->
    let vals = Hashtbl.find_all tbl1 x in
    Hashtbl.add tbl2 x (S.of_list vals)
  );
  tbl2;;

let make_region (x,y) l =
  (*Printf.printf "got (%d,%d)\n" x y;*)
  let intermediate = [(x,y)] in
  let rec inner queue processed acc =
    if List.length queue = 0 then acc else
      let (a,b) = List.hd queue in
      let rest = List.tl queue in
      if S.mem (a,b) processed then
        inner rest processed acc
      else if S.mem (a,b) l then
        inner (List.append [(a-1,b);(a+1,b);(a,b-1);(a,b+1)] rest) (S.add (a,b) processed) ((a,b)::acc)
      else
        inner rest (S.add (a,b) processed) acc
  in
  inner intermediate S.empty [] |> S.of_list;;

let getregions coords keys =
  let rec inner coords keys acc =
    match keys with
    | [] -> acc
    | x::_ -> (
      (*Printf.printf "processing %c\n" x;*)
      (*flush_all ();*)
      let vals = Hashtbl.find coords x in
      let region = make_region (S.choose vals) vals in
      let diff = S.diff vals region in
      if S.is_empty diff then Hashtbl.remove coords x else Hashtbl.replace coords x diff;
      inner coords (Hashtbl.to_seq_keys coords |> List.of_seq) (region::acc)
    )
  in
  inner coords keys [];;

(* Part 1 *)

let getperimeter l =
  let rec inner lst acc =
    match lst with
    | [] -> acc
    | (x,y)::v -> (
      let sides = ref 0 in
      if not (List.mem (x-1,y) l) then sides := !sides + 1;
      if not (List.mem (x+1,y) l) then sides := !sides + 1;
      if not (List.mem (x,y-1) l) then sides := !sides + 1;
      if not (List.mem (x,y+1) l) then sides := !sides + 1;
      inner v (acc + !sides)
    )
  in
  inner l 0;;

let part1 file =
  let tbl = parse file in
  (*Hashtbl.iter (fun k v -> print_char k; print_char ' '; pp_tup_list (S.to_list v)) tbl;*)
  let keys = Hashtbl.to_seq_keys tbl |> List.of_seq in
  (*List.iter print_char keys;*)
  (*print_endline "calling regions";*)
  let regions = getregions tbl keys in
  (*print_int (List.length regions);*)
  List.fold_left (fun acc v ->
    let l = S.to_list v in
    let area = List.length l in
    let perimeter = getperimeter l in
    acc + (area * perimeter)
  ) 0 regions;;

(*print_int (part1 "inp.txt")*)

(* Part 2 *)

let getperimeter2 l =
  let rec inner lst acc =
    match lst with
    | [] -> acc
    | (x,y)::v -> (
      let corners = ref 0 in
      (*Outer*)
      if not (List.mem (x-1,y) l) && not (List.mem (x,y-1) l) then corners := !corners + 1;
      if not (List.mem (x-1,y) l) && not (List.mem (x,y+1) l) then corners := !corners + 1;
      if not (List.mem (x+1,y) l) && not (List.mem (x,y-1) l) then corners := !corners + 1;
      if not (List.mem (x+1,y) l) && not (List.mem (x,y+1) l) then corners := !corners + 1;
      (*Inner*)
      if (List.mem (x-1,y) l) && (List.mem (x,y-1) l) && not (List.mem (x-1,y-1) l) then
        corners := !corners + 1;
      if (List.mem (x-1,y) l) && (List.mem (x,y+1) l) && not (List.mem (x-1,y+1) l) then
        corners := !corners + 1;
      if (List.mem (x+1,y) l) && (List.mem (x,y-1) l) && not (List.mem (x+1,y-1) l) then
        corners := !corners + 1;
      if (List.mem (x+1,y) l) && (List.mem (x,y+1) l) && not (List.mem (x+1,y+1) l) then
        corners := !corners + 1;
      inner v (acc + !corners)
    )
  in
  inner l 0;;

let part2 file =
  let tbl = parse file in
  (*Hashtbl.iter (fun k v -> print_char k; print_char ' '; pp_tup_list (S.to_list v)) tbl;*)
  let keys = Hashtbl.to_seq_keys tbl |> List.of_seq in
  (*List.iter print_char keys;*)
  (*print_endline "calling regions";*)
  let regions = getregions tbl keys in
  (*print_int (List.length regions);*)
  List.fold_left (fun acc v ->
    let l = S.to_list v in
    let area = List.length l in
    let perimeter = getperimeter2 l in
    (*Printf.printf "%d\n" perimeter;*)
    acc + (area * perimeter)
  ) 0 regions;;

print_int (part2 "inp.txt")
