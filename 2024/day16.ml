let explode s =
  let rec inner l i =
    if i < 0 then l else inner (s.[i] :: l) (i-1)
  in
  inner [] (String.length s - 1);;

let rec pp_queue l = match l with
  | [] -> print_newline ()
  | ((x,y),p,(dx,dy))::v -> Printf.printf "(%d,%d) %d (%d,%d); " x y p dx dy; pp_queue v;;

let pp_path p =
  let rec inner p =
    match p with
    | [] -> print_newline ()
    | x::v -> print_char x; inner v
  in
  List.iter inner p;;

let find_SE l =
  let rec inner c i l =
    match l with
    | [] -> None
    | x::v -> if x = c then Some i else inner c (i+1) v
  in
  let rec last_inner c i l =
    match l with
    | [] -> assert false
    | x::v -> match x with
      | None -> last_inner c (i+1) v
      | Some a -> (i,a)
  in
  let optl = List.map (inner 'S' 0) l in
  let start = last_inner 'S' 0 optl in
  let optl = List.map (inner 'E' 0) l in
  let ant = last_inner 'E' 0 optl in
  (start, ant);;

let parse file =
  let inp = In_channel.with_open_text file In_channel.input_all in
  let strl = String.split_on_char '\n' inp in
  let l = List.rev strl |> List.tl |> List.rev |> List.map explode in
  let (s,e) = find_SE l in
  (l, s, e);;

let get path (x,y) = List.nth (List.nth path x) y;;

(* Part 1 *)

module S = Set.Make(struct
  type t = (int * int) * int * (int * int) (*current priority dirn*)
  let compare (a1,p1,_) (a2,p2,_) =
    if p1 < p2 then -1 else if p2 > p1 then 1
    else if a1 = a2 then 0 else 1
end);;

module S2 = Set.Make(struct
  type t = (int*int) * int
  let compare (a1,p1) (a2,p2) =
    if p1 < p2 then -1 else if p2 > p1 then 1
    else if a1 = a2 then 0 else 1
end);;

let get_oppdir_in_90_deg (x,y) current =
  let dirns = [(0,1);(-1,0);(0,-1);(1,0)] in
  let idx = List.find_index ((=) current) dirns |> Option.get in
  let idx1 = (idx+1) mod 4 in
  let idx2 = (((idx-1) mod 4) + 4) mod 4 in
  let (dx1,dy1) = List.nth dirns idx1 in
  let pt1 = (x+dx1,y+dy1) in
  let (dx,dy) = List.nth dirns idx2 in
  let pt2 = (x+dx,y+dy) in
  (pt1,(dx1,dy1),pt2,(dx,dy));;

let dijkstra path s e =
  let to_add_visited pt newp visited =
    let set = S2.filter (fun (x,_) -> x = pt) visited in
    if set = S2.empty then true else
      let (_,p) = S2.min_elt set in
      p > newp
  in

  let (ex,ey) = e in
  let ends = [(ex+1,ey);(ex,ey-1)] in

  let rec inner queue visited =
    if S.empty = queue then (-1) else
      let (top,topp,(dx,dy)) = S.min_elt queue in
      if List.mem top ends then topp+1 else
        (
          let newq = ref (S.(diff queue (singleton (top,topp,(dx,dy))))) in
          let newv = ref visited in
          let x,y = top in
          let pt1 = (x+dx,y+dy) in
          let pt2,d2,pt3,d3 = get_oppdir_in_90_deg top (dx,dy) in
          let newp = topp + 1 in
          if get path pt1 = '.' && (to_add_visited pt1 newp visited) then
            (
              newq := S.add (pt1, newp, (dx,dy)) !newq;
              newv := S2.add (pt1, newp) !newv
            );
          let newp = topp + 1001 in
          if get path pt2 = '.' && (to_add_visited pt2 newp visited) then
            (
              newq := S.add (pt2, newp, d2) !newq;
              newv := S2.add (pt2, newp) !newv
            );
          if get path pt3 = '.' && (to_add_visited pt3 newp visited) then
            (
              newq := S.add (pt3, newp, d3) !newq;
              newv := S2.add (pt3, newp) !newv
            );
          inner !newq !newv
        )
  in
  inner (S.of_list [(s,0,(0,1))]) (S2.of_list [(s,0)]);;

let part1 file =
  let path,s,e = parse file in
  dijkstra path s e;;

part1 "inp.txt" |> print_int

(* Part 2 *)
