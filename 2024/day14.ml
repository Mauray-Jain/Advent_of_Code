let parse file =
  let inp = In_channel.with_open_text file In_channel.input_all in
  let strl = String.split_on_char '\n' inp in
  let l = List.rev strl |> List.tl |> List.rev in
  List.map (fun s ->
    Scanf.sscanf s "p=%d,%d v=%d,%d" (fun a b c d -> ((a,b), (c,d)))
  ) l;;

let lenx = 101;;
let leny = 103;;

let rec pp_tup_list l = match l with
  | [] -> print_newline ()
  | (a,b)::v -> Printf.printf "(%d,%d); " a b; pp_tup_list v;;

(* Part 1 *)

let part1 file =
  let l = parse file in
  let final_pos = List.map (fun ((px,py), (vx,vy)) ->
    let x = (px + 100 * vx) mod lenx in
    let y = (py + 100 * vy) mod leny in
    (
      (if x < 0 then x + lenx else x),
      (if y < 0 then y + leny else y)
    )
  ) l in
  (*pp_tup_list final_pos;*)
  let quad1 = ref 0 in
  let quad2 = ref 0 in
  let quad3 = ref 0 in
  let quad4 = ref 0 in
  let midx = lenx / 2 in
  let midy = leny / 2 in
  List.iter (fun (x,y) ->
    if x < midx && y < midy then quad1 := 1 + !quad1
    else if x > midx && y < midy then quad2 := 1 + !quad2
    else if x > midx && y > midy then quad3 := 1 + !quad3
    else if x < midx && y > midy then quad4 := 1 + !quad4
  ) final_pos;
  !quad1 * !quad2 * !quad3 * !quad4;;

(*print_int @@ part1 @@ "inp.txt"*)

(* Part 2 *)

(*
Okay, so reddit tells me that there's a bounding box around the tree
1) the tree occurs when all robots at uniq positions
2) calc x mean and y mean and distance of all points when distance least ans
3) find clusters in x coord and in y coord repeat with 101 and 103 periods each find the point when both cluster
*)

let go_step l =
  List.map (fun ((px,py), (vx,vy)) ->
    let x = (px + vx) mod lenx in
    let y = (py + vy) mod leny in
    (
      (
        (if x < 0 then x + lenx else x),
        (if y < 0 then y + leny else y)
      ),
      (vx,vy)
    )
  ) l;;

let calcdist state =
  let positions = List.split state |> fst in
  let (x_pos, y_pos) = List.split positions in
  let x_mean = List.fold_left (+) 0 x_pos in
  let x_mean = x_mean / (List.length x_pos) in
  let y_mean = List.fold_left (+) 0 y_pos in
  let y_mean = y_mean / (List.length y_pos) in

  let almost = List.fold_left2 (fun acc x y ->
    let deltax = x - x_mean in
    let deltay = y - y_mean in
    acc + (deltax * deltax) + (deltay * deltay)
  ) 0 x_pos y_pos in

  almost / (List.length positions);;

let part2 file =
  let l = parse file in
  let max_iter = lenx * leny in

  let rec inner i states min_dev acc =
    if i > max_iter then acc + 1 else
      let new_state = go_step states in
      let new_dev = calcdist new_state in
      if new_dev < min_dev then inner (i+1) new_state new_dev i
      else inner (i+1) new_state min_dev acc
  in

  inner 0 l max_int 0;;

(*print_int (part2 "inp.txt")*)

(*7672 is ans*)

let render_kishmish file =
  let l = parse file in
  let final_pos = List.map (fun ((px,py), (vx,vy)) ->
    let x = (px + 7672 * vx) mod lenx in
    let y = (py + 7672 * vy) mod leny in
    (
      (if x < 0 then x + lenx else x),
      (if y < 0 then y + leny else y)
    )
  ) l in

  let rec print_line y x =
    if x >= lenx then print_newline () else
      (
        if List.mem (x,y) final_pos then print_char '#' else print_char '.';
        print_line y (x+1)
      )
  in

  for y = 0 to leny do
    print_line y 0
  done;;

render_kishmish "inp.txt"
