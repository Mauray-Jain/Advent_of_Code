let split_l c l =
  let rec inner l acc =
    match l with
    | [] -> [List.rev acc]
    | x::v -> if String.equal x c then (List.rev acc) :: (inner v []) else inner v (x::acc)
  in
  inner l [];;

let parse file =
  let inp = In_channel.with_open_text file In_channel.input_all in
  let strl = String.split_on_char '\n' inp in
  let l = List.rev strl |> List.tl |> List.rev |> split_l "" in
  List.map (fun x ->
    let a = Scanf.sscanf (List.nth x 0) "Button A: X+%d, Y+%d" (fun a b -> (a,b)) in
    let b = Scanf.sscanf (List.nth x 1) "Button B: X+%d, Y+%d" (fun a b -> (a,b)) in
    let p = Scanf.sscanf (List.nth x 2) "Prize: X=%d, Y=%d" (fun a b -> (a,b)) in
    (a,b,p)
  ) l;;

(* Part 1 *)

let part1 file =
  let l = parse file in
  List.fold_left (fun acc ((ax,ay), (bx,by), (px,py)) ->
    let det = (ax*by) - (bx*ay) in
    let a = ((px*by) - (bx*py)) / det in
    let b = ((ax*py) - (px*ay)) / det in
    if (a * ax + b * bx, a * ay + b * by) = (px,py) then acc + 3 * a + b else acc
  ) 0 l;;

(*print_int (part1 "inp.txt")*)

(* Part 2 *)

let part2 file =
  let l = parse file in
  List.fold_left (fun acc ((ax,ay), (bx,by), (px,py)) ->
    let px = px + 10000000000000 in
    let py = py + 10000000000000 in
    let det = (ax*by) - (bx*ay) in
    let a = ((px*by) - (bx*py)) / det in
    let b = ((ax*py) - (px*ay)) / det in
    if (a * ax + b * bx, a * ay + b * by) = (px,py) then acc + 3 * a + b else acc
  ) 0 l;;

print_int (part2 "inp.txt")
