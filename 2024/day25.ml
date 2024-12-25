let split_l c l =
  let rec inner l acc =
    match l with
    | [] -> [List.rev acc]
    | x::v -> if String.equal x c then (List.rev acc) :: (inner v []) else inner v (x::acc)
  in
  inner l [];;

let explode s =
  let rec inner l i =
    if i < 0 then l else inner (s.[i] :: l) (i-1)
  in
  inner [] (String.length s - 1);;

let rec transpose list = match list with
  | [] -> []
  | []::xss -> transpose xss
  | (x::xs)::xss -> List.((x :: map hd xss) :: transpose (xs :: map tl xss))

let count l = List.find_all (fun x -> x = '#') l |> List.length;;

let parse_block l =
  let block = List.tl l |> List.rev |> List.tl |> List.rev in
  let block = List.map explode block |> transpose in
  [
    List.nth block 0 |> count;
    List.nth block 1 |> count;
    List.nth block 2 |> count;
    List.nth block 3 |> count;
    List.nth block 4 |> count
  ];;

let parse file =
  let inp = In_channel.with_open_text file In_channel.input_all in
  let strl = String.split_on_char '\n' inp in
  let l = List.rev strl |> List.tl |> List.rev |> split_l "" in
  let locks = ref [] in
  let keys = ref [] in
  List.iter (fun x ->
    let cnt = List.hd x |> explode |> count in
    let block = parse_block x in
    if cnt = 5 then locks := block :: !locks
    else keys := block :: !keys
  ) l;
  (!locks, !keys);;

(* Part 1 *)

let fits lock key =
  let rec inner i =
    if i = 5 then true else
      let lck = List.nth lock i in
      let ky = List.nth key i in
      if lck + ky > 5 then false else inner (i+1)
  in
  inner 0;;

let cnt_keys lock keys =
  let rec inner keys =
    match keys with
    | [] -> 0
    | key::v -> (if fits lock key then 1 else 0) + (inner v)
  in
  inner keys;;

let part1 file =
  let (locks,keys) = parse file in
  List.fold_left (fun acc lock -> acc + (cnt_keys lock keys)) 0 locks;;

print_int (part1 "inp.txt")

(* Part 2 *)
