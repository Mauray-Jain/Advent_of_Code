let explode s =
  let rec inner l i =
    if i < 0 then l else inner (s.[i] :: l) (i-1)
  in
  inner [] (String.length s - 1);;

let rep num times =
  let num_times = (Stdlib.int_of_char times) - (Char.code '0') in
  List.init num_times (fun _ -> num);;

let rec pp_list l = match l with
  | [] -> print_newline ()
  | x::v -> print_int x; print_string "; "; pp_list v;;

let parse file =
  let inp = In_channel.with_open_text file In_channel.input_all in
  let split = String.split_on_char '\n' inp in
  let block = List.hd split in
  let l = explode block in
  let rec make_fs i id l acc =
    match l with
    | [] -> acc
    | x::v -> (
      if (i mod 2) = 0 then make_fs (i+1) (id+1) v (List.append acc (rep id x)) else
        make_fs (i+1) id v (List.append acc (rep (-1) x))
    )
  in
  make_fs 0 0 l [];;

(*pp_list (parse "inp.txt")*)

(* Part 1 *)

let arrange1 l =
  let rec inner len l =
    let idx = Array.find_index (fun x -> x = -1) l in
    match idx with
    | None -> l
    | Some i -> (
        if i > len then l else
        (
          let file_id = l.(len) in
          l.(i) <- file_id;
          l.(len) <- (-1);
          inner (len-1) l
        )
    )
  in
  inner ((Array.length l) - 1) l;;

let part1 file =
  let block = parse file in
  let arrange = arrange1 (Array.of_list block) in
  let rec checksum l i acc =
    match l with
    | [] -> acc
    | x::v -> if x = -1 then acc else checksum v (i+1) (acc + (i*x))
  in
  checksum (Array.to_list arrange) 0 0;;

(*print_int (part1 "inp.txt")*)

(* Part 2 *)

let find_block arr num =
  let idx = Array.find_index (fun x -> x = num) arr in
  let i = Option.get idx in
  let rec find_len arr i len =
    if arr.(i) = num then find_len arr (i+1) (len+1) else len
  in
  (i, find_len arr i 0);;

let get_block arr num =
  let new_arr = Array.to_list arr |> List.rev |> Array.of_list in
  let (rev_i, block_len) = find_block new_arr num in
  let i = Array.length arr - rev_i - block_len in
  (i, block_len);;

let rec find_free arr min_len idx =
  let (i, len) = try find_block arr (-1) with Invalid_argument _ -> (-1, -1) in
  if (i,len) = (-1,-1) then (false, -1, -1) else
  if len < min_len then find_free (Array.sub arr (i+len) (Array.length arr - i - len)) min_len (idx+i + len)
  else (true, i + idx, len)

let max_idx arr = arr.(Array.length arr - 1);;

let rec mv arr from_idx to_idx len =
  match len with
  | 0 -> arr
  | x -> (
    arr.(to_idx) <- arr.(from_idx);
    arr.(from_idx) <- (-1);
    mv arr (from_idx + 1) (to_idx + 1) (len - 1)
  );;

let arrange2 arr =
  let rec inner arr num =
    if num = 0 then arr else
      let (block_idx, block_len) = get_block arr num in
      let (isthere, free_idx, free_len) = find_free arr block_len 0 in
      if isthere && (free_idx < block_idx) then
        let new_arr = mv arr block_idx free_idx block_len in
        inner new_arr (num - 1)
      else inner arr (num-1)
  in
  inner arr (max_idx arr);;

let part2 file =
  let block = parse file in
  let arrange = arrange2 (Array.of_list block) in
  let rec checksum l i acc =
    match l with
    | [] -> acc
    | x::v -> if x = -1 then checksum v (i+1) acc else checksum v (i+1) (acc + (i*x))
  in
  checksum (Array.to_list arrange) 0 0;;

print_int (part2 "inp.txt")
