(* Parse *)
let parse_content file = In_channel.with_open_text file In_channel.input_all;;

(* Part 1 *)
let get_num str =
  Scanf.sscanf_opt str "mul(%d,%d)%n" (fun x y z -> ([x;y],z));;

let rec inner inp =
  let i = (
    match String.index_opt inp 'm' with
    | None -> -1
    | Some a -> a
  ) in
  if i = -1 then [] else
    let str = String.sub inp i ((String.length inp) - i) in
    (*print_endline str;*)
    let out = get_num str in
    let l = (
      match out with
      | None -> ((0,0),1)
      | Some ([a;b],c) -> ((a,b),c+i)
      | _ -> assert false
    ) in
    let (ans, new_idx) = l in
    let new_str = String.sub inp new_idx ((String.length inp) - new_idx) in
    (*print_endline new_str;*)
    ans :: inner new_str;;

let rec compute l =
  match l with
  | [] -> 0
  | (a,b)::v -> (a*b) + (compute v);;

let part1 inp =
  let l = inner inp in
  print_int (compute l);;

(*part1 (parse_content "inp.txt")*)

(* Part 2 *)

let rec pp_list l = match l with
  | [] -> print_newline ()
  | x::v -> print_int x; print_string "; "; pp_list v;;

type instruction = Do | Dont | Mult of int*int | Nop

let pp_ins ins =
  match ins with
  | Do -> print_endline "do"
  | Dont -> print_endline "dont"
  | Mult (a,b) -> Printf.printf "mult %d %d\n" a b
  | Nop -> print_string "";;

let mult str =
  match Scanf.sscanf_opt str "mul(%d,%d)%n%_s" (fun x y z -> (x*y, z)) with
  | None -> (false, 0, 0)
  | Some (a,b) -> (true, a, b);;

let part2 inp =
  let emit str =
    let (ismult, ans, skip) = mult str in
    if String.starts_with ~prefix:"do()" str then Do
    else if String.starts_with ~prefix:"don't()" str then Dont
    else if ismult then Mult (ans,skip)
    else Nop
  in
  let rec inner enabled str acc =
    let ins = emit str in
    (*pp_ins ins;*)
    if (String.length str) < 8 then acc else
    if ins = Nop then inner enabled (String.sub str 1 ((String.length str)-1)) acc else
      match ins with
      | Do -> inner true (String.sub str 4 ((String.length str) - 4)) acc
      | Dont -> inner false (String.sub str 4 ((String.length str) - 4)) acc
      | Mult (a,b) -> inner enabled (String.sub str b ((String.length str) - b)) (
        if not enabled then acc else (acc+a)
      )
      | _ -> assert false
  in
  inner true inp 0;;

(*pp_list (find_donts "dsajjdon't()dado()do()" 0)*)
print_int (part2 (parse_content "inp.txt"))
