let parse_line s =
  let stream = Scanf.Scanning.from_string s in
  let a = Scanf.bscanf stream "%d: " (fun x -> x) in
  let rec inner acc =
    match Scanf.bscanf stream " %d " (fun x -> x::acc) with
    | l -> inner l
    | exception _ -> acc
  in
  (a, inner []);;

let explode s =
  let rec inner l i =
    if i < 0 then l else inner (s.[i] :: l) (i-1)
  in
  inner [] (String.length s - 1);;

let rec implode l =
  match l with
  | [] -> ""
  | x::v -> (String.make 1 x) ^ (implode v);;

let parse file =
  let inp = In_channel.with_open_text file In_channel.input_all in
  let a = String.split_on_char '\n' inp in
  let l = List.rev (List.tl (List.rev a)) in
  List.map parse_line l;;

(* Part 1 *)

(* I'm too dumb and tired to implement it this way *)
(*Courtesy: https://github.com/khwilson/advent2024/blob/main/src/advent2024/days/day07.py*)
(*That idea is a fukin genius bhai i cudn't have ever thought of it*)

(*let combo (res, inp) =*)
(*  let rec apply_op operation acc i =*)
(*    (*Printf.printf "\n%d,%d\n" (List.length operation) (List.length inp);*)*)
(*    assert ((List.length operation) = (List.length inp -1));*)
(*    match operation with*)
(*    | [] -> acc*)
(*    | x::v -> ( *)
(*      match x with*)
(*      | '0' -> apply_op v (acc + List.nth inp i) (i+1)*)
(*      | '1' -> apply_op v (acc * List.nth inp i) (i+1)*)
(*      | _ -> assert false*)
(*    )*)
(*  in*)
(*  let rec next_op operation =*)
(*    let s = implode operation in*)
(*    (*print_endline s;*)*)
(*    let inter = "0b" ^ s in*)
(*    let num = Stdlib.int_of_string inter in*)
(*    let bytesss = Bytes.of_string s in*)
(*    let len = Bytes.length bytesss in*)
(*    Bytes.iteri (fun i _ ->*)
(*      let n = len - i - 1 in*)
(*      Bytes.set bytesss i (if (num+1) land (1 lsl n) != 0 then '1' else '0')*)
(*    ) bytesss;*)
(*    explode (Bytes.to_string bytesss)*)
(*  in*)
(*  List.iter print_char (next_op ['1'; '1']);*)
(*  let rec inner res inp operation ans =*)
(*    (*print_int (List.length operation);*)*)
(*    (*List.iter print_char operation;*)*)
(*    print_newline ();*)
(*    if (List.length operation) > (List.length inp - 1) then ans else*)
(*    inner res inp (next_op operation)*)
(*        ((if res = (apply_op operation 0 0) then res else 0) + ans)*)
(*  in*)
(*  inner res inp (List.init ((List.length inp) - 1) (fun _ -> '0')) 0;;*)

let rec doable res inp =
  let hd = List.hd inp in
  if List.length inp = 1 then res = hd else
    let rem = res mod (hd) in
    let diff = res - (hd) in
    if (rem = 0) && doable (res / (hd)) (List.tl inp) then true
    else if (diff >= 0) && doable diff (List.tl inp) then true
    else false;;

let kisi_aur_ka_idea acc (res,inp) =
  if doable res inp then acc + res else acc;;

let part1 file =
  let l = parse file in
  (*print_int (combo (List.hd l));;*)
  (*let ans = List.map combo l in*)
  (*List.fold_left (+) 0 ans;;*)
  List.fold_left kisi_aur_ka_idea 0 l;;

(*print_int (part1 "inp.txt");*)

(* Part 2 *)

let rec doable2 res inp =
  let hd = List.hd inp in
  if List.length inp = 1 then res = hd else
    let rem = res mod hd in
    let diff = res - hd in
    if (rem = 0) && doable2 (res / hd) (List.tl inp) then true
    else if (diff >= 0) && doable2 diff (List.tl inp) then true
    else
      let hd_str = Stdlib.string_of_int hd in
      let res_str = Stdlib.string_of_int res in
      if String.length hd_str < String.length res_str then
        let concat_diff = String.sub res_str (String.length res_str - String.length hd_str) (String.length hd_str) in
        let other = String.sub res_str 0 (String.length res_str - String.length hd_str) in
        if concat_diff = hd_str then doable2 (Stdlib.int_of_string other) (List.tl inp) else false
      else false

let kisi_aur_ka_idea2 acc (res,inp) =
  if doable2 res inp then acc + res else acc;;

let part2 file =
  let l = parse file in
  (*print_int (combo (List.hd l));;*)
  (*let ans = List.map combo l in*)
  (*List.fold_left (+) 0 ans;;*)
  List.fold_left kisi_aur_ka_idea2 0 l;;

print_int (part2 "inp.txt");
