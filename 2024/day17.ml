let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a);;

(* Part 1 *)

type combo =
  | NUM of int
  | A
  | B
  | C
;;

type opcodes =
  | ADV of int
  | BXL of int
  | BST of int
  | JNZ of int
  | BXC of int
  | OUT of int
  | BDV of int
  | CDV of int
;;

let disas op =
  match op with
  | ADV x -> Printf.printf "ADV %d\n" x
  | BDV x -> Printf.printf "BDV %d\n" x
  | CDV x -> Printf.printf "CDV %d\n" x
  | OUT x -> Printf.printf "OUT %d\n" x
  | BXC x -> Printf.printf "BXC %d\n" x
  | BXL x -> Printf.printf "BXL %d\n" x
  | JNZ x -> Printf.printf "JNZ %d\n" x
  | BST x -> Printf.printf "BST %d\n" x;;

let make_op op value =
  match op with
  | 0 -> ADV value
  | 1 -> BXL value
  | 2 -> BST value
  | 3 -> JNZ value
  | 4 -> BXC value
  | 5 -> OUT value
  | 6 -> BDV value
  | 7 -> CDV value
  | _ -> assert false;;

let get_opcodes l =
  let rec inner rest =
    match rest with
    | [] -> []
    | op::comb::v -> (make_op op comb) :: (inner v)
    | _ -> assert false
  in
  inner l;;

let parse file =
  let inp = In_channel.with_open_text file In_channel.input_all in
  let strl = String.split_on_char '\n' inp in
  let l = List.rev strl |> List.tl |> List.rev in

  let find_reg_val s =
    Scanf.sscanf s "Register %c: %d" (fun _ x -> x)
  in

  let get_pirogram s =
    Scanf.sscanf s "Program: %s" (
      fun x ->
      let ins = String.split_on_char ',' x |> List.map Stdlib.int_of_string in
      get_opcodes ins
    )
  in

  let a = find_reg_val (List.hd l) in
  let b = find_reg_val (List.nth l 1) in
  let c = find_reg_val (List.nth l 2) in
  let pirogram = get_pirogram (List.nth l 4) in
  (a,b,c,pirogram);;

let get_val (a,b,c) comb =
  match comb with
    | 0|1|2|3 -> comb
    | 4 -> a
    | 5 -> b
    | 6 -> c
    | _ -> assert false

let eval_ins (a,b,c,ip) op =
  (*disas op;*)
  match op with
  | ADV operand -> get_val (a,b,c) operand |> (fun x -> (a / (pow 2 x),b,c,ip+1))
  | BDV operand -> get_val (a,b,c) operand |> (fun x -> (a,a / (pow 2 x),c,ip+1))
  | CDV operand -> get_val (a,b,c) operand |> (fun x -> (a,b,a / (pow 2 x),ip+1))
  | BXL operand -> (a,b lxor operand,c,ip+1)
  | BST operand -> get_val (a,b,c) operand |> (fun x -> (a,x mod 8,c,ip+1))
  | JNZ operand -> if a = 0 then (a,b,c,ip+1) else (a,b,c,operand)
  | BXC _ -> (a,b lxor c,c,ip+1)
  | OUT operand -> get_val (a,b,c) operand |>
    (fun x -> Printf.printf "%d," (x mod 8); (a,b,c,ip+1))
;;

let eval (a,b,c) ops =
  let rec inner (a,b,c,ip) ops =
    if ip >= List.length ops then () else
      let op = List.nth ops ip in
      inner (eval_ins (a,b,c,ip) op) ops
  in
  inner (a,b,c,0) ops;;

let part1 file =
  let (a,b,c,pirogram) = parse file in
  eval (a,b,c) pirogram;;

(*part1 "inp.txt"*)

(* Part 2 *)

let parse2 file =
  let inp = In_channel.with_open_text file In_channel.input_all in
  let strl = String.split_on_char '\n' inp in
  let l = List.rev strl |> List.tl |> List.rev in

  let get_pirogram s =
    Scanf.sscanf s "Program: %s"
      (fun x -> String.split_on_char ',' x |> List.map Stdlib.int_of_string)
  in

  let pirogram = get_pirogram (List.nth l 4) in
  pirogram;;

let step a =
  let b = a mod 8 in
  let b = b lxor 4 in
  let c = a / (pow 2 b) in
  let b = b lxor c in
  let b = b lxor 4 in
  b mod 8;;

let run a =
  let rec inner a =
    if a = 0 then [] else (step a) :: (inner (a / 8))
  in
  inner a;;

let slice l i =
  List.filteri (fun x _ -> x >= i) l;;

let find program =
  let rec inner a i j =
    let new_a = a*8 + j in
    if List.equal (=) (run new_a) (slice program i) then
      if i = 0 then new_a else inner new_a (i-1) 0
    else inner a i (j+1)
  in
  inner 0 (List.length program - 1) 0;;

let part2 file =
  let program = parse2 file in
  find program;;

print_int (part2 "inp.txt")
