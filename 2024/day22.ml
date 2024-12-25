let parse file =
  let inp = In_channel.with_open_text file In_channel.input_all in
  let strl = String.split_on_char '\n' inp in
  let l = List.rev strl |> List.tl |> List.rev in
  List.map Stdlib.int_of_string l;;

(* Part 1 *)

let calc2000sec seed =
  let mixprune num secnum =
    (num lxor secnum) mod 16777216
  in

  let calcnextsec num =
    let num = mixprune (num*64) num in
    let num = mixprune (num/32) num in
    mixprune (num*2048) num
  in

  let rec inner i num =
    if i = 0 then num else inner (i-1) (calcnextsec num)
  in

  inner 2000 seed;;

let part1 file =
  let initsec = parse file in
  List.fold_left (fun acc x ->
    let a = calc2000sec x in
    acc + a
  ) 0 initsec;;

(*print_int (part1 "inp.txt")*)

(* Part 2 *)

module S = Set.Make(struct
  type t = int*int*int*int
  let compare = compare
end);;

let gen2000sec seed =
  let mixprune num secnum =
    (num lxor secnum) mod 16777216
  in

  let calcnextsec num =
    let num = mixprune (num*64) num in
    let num = mixprune (num/32) num in
    mixprune (num*2048) num
  in

  let rec inner i num =
    if i = 0 then [] else num :: (inner (i-1) (calcnextsec num))
  in

  inner 2000 seed |> List.map (fun x -> x mod 10);;

let rec pp_l l = match l with
  | [] -> print_newline ()
  | x::v -> Printf.printf "%d, " x; pp_l v;;

let rec gen_diff l =
  match l with
  | [] -> []
  | [_] -> []
  | x::v -> ((List.hd v) - x) :: (gen_diff v);;

let gen_quad diffs =
  let rec inner i =
    if i > (List.length diffs - 4) then [] else
      (
        List.nth diffs i,
        List.nth diffs (i+1),
        List.nth diffs (i+2),
        List.nth diffs (i+3)
      ) :: (inner (i+1))
  in
  inner 0;;

let per_l secrets tbl =
  let diffs = gen_diff secrets in
  let quads = gen_quad diffs in
  let set = ref S.empty in

  List.iteri (fun i quad ->
    if S.mem quad !set then () else (
      Hashtbl.add tbl quad (List.nth secrets (i+4));
      set := S.add quad !set;
    )
  ) quads;;

let part2 file =
  let inp = parse file in
  let tbl = Hashtbl.create 1024 in

  List.iter (fun seed ->
    let secrets = gen2000sec seed in
    per_l secrets tbl
  ) inp;

  let keys = Hashtbl.to_seq_keys tbl |> S.of_seq |> S.to_list in
  List.fold_left (fun acc key ->
    let bananas = Hashtbl.find_all tbl key in
    let total = List.fold_left (+) 0 bananas in
    max total acc
  ) 0 keys;;

print_int (part2 "inp.txt")
