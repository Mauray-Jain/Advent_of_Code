let parse file =
  let inp = In_channel.with_open_text file In_channel.input_all in
  let strl = String.split_on_char '\n' inp in
  let l = List.rev strl |> List.tl |> List.rev in
  List.map (fun s -> Scanf.sscanf s "%d,%d" (fun x y -> (y,x))) l;;

let len_lines = 7;;
let len_chars = 7;;

type pos = {
  x : int;
  y : int;
  dist : int;
};;

let rec pp_pos_list l = match l with
  | [] -> print_newline ()
  | x::v -> Printf.printf "(%d,%d); " x.x x.y; pp_pos_list v;;

module S = Set.Make(struct
  type t = int*int
  let compare = compare
end);;

let traverse obs start ant =
  let rec inner queue visited steps =
    pp_pos_list queue;
    if List.length queue = 0 then steps else
      let top = List.hd queue in
      let x = top.x in
      let y = top.y in
      let rest = List.tl queue in
      if (x,y) = ant then top.dist else
        let new_q = ref [] in
        if x > 0 && not (S.mem (x-1,y) visited) && not (List.mem (x-1,y) obs) then
          new_q := {x = x-1; y = y; dist = top.dist+1} :: !new_q;

        if y > 0 && not (S.mem (x,y-1) visited) && not (List.mem (x,y-1) obs) then
          new_q := {x = x; y = y-1; dist = top.dist+1} :: !new_q;

        if x < (len_lines - 1) && not (S.mem (x+1,y) visited) && not (List.mem (x+1,y) obs) then
          new_q := {x = x+1; y = y; dist = top.dist+1} :: !new_q;

        if y < (len_chars - 1) && not (S.mem (x,y+1) visited) && not (List.mem (x,y+1) obs) then
          new_q := {x = x; y = y+1; dist = top.dist+1} :: !new_q;

        inner (!new_q @ rest) (S.add (x,y) visited) (top.dist+1)
  in
  inner [{x = fst start; y = snd start; dist = 0}] S.empty 0;;
 
(* Part 1 *)

let part1 file =
  let obs = parse file in
  traverse obs (0,0) (len_lines-1,len_chars-1);;

print_int (part1 "inp.txt")

(* Part 2 *)
