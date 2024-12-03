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

(*let strstr hay needle =*)
(*  let rec inner hay needle s =*)
(*  let idx = String.index_opt hay needle.[0] in*)
(*  match idx with*)
(*  | None -> None*)
(*  | Some i -> if (String.equal needle (String.sub hay i (min ((String.length hay) - i) (String.length needle)))) then Some (s + i) else inner (String.sub hay (i+1) ((String.length hay) - i - 1)) needle (i+1)*)
(*  in*)
(*  inner hay needle 0;;*)
(**)
(*let rec inner2 idx inp =*)
(*  let i = ( *)
(*    match String.index_opt inp 'm' with*)
(*    | None -> -1*)
(*    | Some a -> a*)
(*  ) in*)
(*  if i = -1 then [] else*)
(*    let str = String.sub inp i ((String.length inp) - i) in*)
(*    (*print_endline str;*)*)
(*    let out = get_num str in*)
(*    let l = ( *)
(*      match out with*)
(*      | None -> ((0,0),1)*)
(*      | Some ([a;b],c) -> ((a,b),c+i)*)
(*      | _ -> assert false*)
(*    ) in*)
(*    let (ans, new_idx) = l in*)
(*    let new_str = String.sub inp new_idx ((String.length inp) - new_idx) in*)
(*    (*print_endline new_str;*)*)
(*    (ans, idx + i) :: inner2 (idx+new_idx) new_str;;*)
(**)
(*let rec find_dos str idx =*)
(*  match strstr str "do()" with*)
(*  | None -> []*)
(*  | Some a -> (a+idx) :: (find_dos (String.sub str (a + 4) ((String.length str) - a - 4)) (a+idx+4));;*)
(**)
(*let rec find_donts str idx =*)
(*  match strstr str "don't()" with*)
(*  | None -> []*)
(*  | Some a -> (a+idx) :: (find_donts (String.sub str (a+7) ((String.length str) - a - 7)) (a+idx+7));;*)
(**)
(*let first l =*)
(*  match l with*)
(*  | [] -> max_int*)
(*  | x::v -> x;;*)
(**)
(*let rec pp_list l = match l with*)
(*  | [] -> print_newline ()*)
(*  | x::v -> print_int x; print_string "; "; pp_list v;;*)
(**)
(*let part2 inp =*)
(*  let dos = find_dos inp 0 in*)
(*  let donts = find_donts inp 0 in*)
(*  let l = inner2 0 inp in*)
(*  print_string "dos: "; pp_list dos; print_int (List.length dos); print_newline ();*)
(*  print_string "donts: "; pp_list donts;print_int (List.length donts); print_newline ();*)
(*  let rec cheentapak l dos donts flag =*)
(*    let flg = ref flag in*)
(*    match l with*)
(*    | [] -> 0*)
(*    | ((a,b),c)::v -> ( *)
(*      let do1 = first dos in*)
(*      let dont1 = first donts in*)
(*      let dosl = ref dos in*)
(*      let dontsl = ref donts in*)
(*      if (c > dont1) && (c < do1) then*)
(*        ( flg := 0;*)
(*          dontsl := List.tl donts; )*)
(*      else if (c > do1) && (c < dont1) then (flg:= 1; dosl := List.tl dos;);*)
(*      (*(print_int !flg; print_string " "; print_int a; print_string " "; print_int b;print_string " "; print_int c; print_newline ());*)*)
(*      ((!flg) * a * b) + (cheentapak v !dosl !dontsl !flg)*)
(*    ) in*)
(*  cheentapak l dos donts 1;;*)
(**)
(*print_int (part2 (parse_content "inp.txt"))*)
(*pp_list (find_donts "dsajjdon't()dado()do()" 0)*)

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
