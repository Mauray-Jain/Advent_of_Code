let explode s =
  let rec inner l i =
    if i < 0 then l else inner (s.[i] :: l) (i-1)
  in
  inner [] (String.length s - 1);;

let rec pp_tup_list l = match l with
  | [] -> print_newline ()
  | (a,b)::v -> Printf.printf "(%d,%d); " a b; pp_tup_list v;;

let find_char c l =
  let rec inner c l i =
    match l with
    | [] -> []
    | a::b -> if a = c then i :: (inner c b (i+1)) else (inner c b (i+1))
  in
  inner c l 0;;

let find_bot l =
  let rec inner c i l =
    match l with
    | [] -> None
    | x::v -> if x = c then Some i else inner c (i+1) v
  in
  let rec last_inner c i l =
    match l with
    | [] -> assert false
    | x::v -> match x with
      | None -> last_inner c (i+1) v
      | Some a -> (i,a)
  in
  let optl = List.map (inner '@' 0) l in
  last_inner '@' 0 optl;;

let parse file =
  let inp = In_channel.with_open_text file In_channel.input_all in
  let (map,moves) = Scanf.sscanf inp "%[#\n@.O]%[<>^v\n]" (fun a b -> (a,b)) in

  let moves = explode moves |> List.filter_map (fun c ->
    match c with
    | '<' -> Some (0,-1)
    | '^' -> Some (-1,0)
    | '>' -> Some (0, 1)
    | 'v' -> Some (1,0)
    | _ -> None
  ) in

  let rec convert i l =
    match l with
    | [] -> []
    | a::b -> List.append (List.map (fun y -> (i,y)) a) (convert (i+1) b)
  in

  let split = String.split_on_char '\n' map |> List.map explode in
  let walls = List.map (find_char '#') split |> convert 0 in
  let baxas = List.map (find_char 'O') split |> convert 0 |> Array.of_list in
  let bot = find_bot split in
  (walls, baxas, bot, moves);;

(*let (w,bx,(btx,bty),mv) = parse "inp.txt" in*)
(*print_endline "Walls";*)
(*pp_tup_list w;*)
(*print_endline "Baxas";*)
(*pp_tup_list bx;*)
(*Printf.printf "Bot: (%d,%d)\n" btx bty;*)
(*print_endline "Moves";*)
(*pp_tup_list mv;;*)

(* Part 1 *)

let rec replace pairs arr =
  match pairs with
  | [] -> ()
  | (a,b)::v -> (
    let idx = Array.find_index ((=) a) arr |> Option.get in
    arr.(idx) <- b;
    replace v arr
  );;

let movebaxas walls baxas (x,y) (dx,dy) =
  let rec canmove (x,y) acc =
    if List.mem (x,y) walls then None
    else if Array.mem (x,y) baxas then canmove (x+dx,y+dy) (((x,y), (x+dx,y+dy))::acc)
    else Some acc
  in
  match canmove (x,y) [] with
  | None -> (baxas,(x-dx,y-dy))
  | Some reps -> (
    replace reps baxas;
    (baxas,(x,y))
  );;

let makemove walls baxas (x,y) (dx,dy) =
  let finol = (x+dx,y+dy) in
  if List.mem finol walls then (baxas,(x,y))
  else if Array.mem finol baxas then movebaxas walls baxas finol (dx,dy)
  else (baxas,finol);;

let part1 file =
  let (walls,baxas,bot,moves) = parse file in
  let rec inner baxas bot moves =
    match moves with
    | [] -> (baxas,bot)
    | move::v -> (
      let (bx,finol) = makemove walls baxas bot move in
      inner bx finol v
    )
  in
  let (bx,_) = inner baxas bot moves in
  Array.fold_left (fun acc (x,y) -> acc + 100*x + y) 0 bx ;;

(*let (bx,(btx,bty)) = part1 "inp.txt" in*)
(*pp_tup_list (Array.to_list bx);*)
(*Printf.printf "Bot: %d,%d\n" btx bty;;*)

(*print_int (part1 "inp.txt")*)

(* Part 2 *)

(*https://topaz.github.io/paste/#XQAAAQAZCwAAAAAAAAAzHIoib6pMX4ickc1Mep93pgqtuV6rGB3/t2SKtfwDPt2r1515wsQUmcHJvxOgeyqU0+EtagUyY+1FBgQ3Yb+/Cc8VeSOxADCyiZJn2UXCr+fr6LdRoQ/ig56mck4ITPzK+0dtURzK7Q6pQDlVUYPZ7XCZSzP5+dU9tf6ewARHCQXt4MlIkBDQ2qCHZ4eEabRS3sHsBj45Dslz3U/y8OWEipkQf//4KFlAgTCeEEKiwgGlbXpNpXo2YkW5ZJGsIlCR1O1mkLyno9fHRkWu5iaydbNJpViMXdbMzOptAd6zvrKIioGTFzDNq86Ipeomwouf/ZiVbogbpAjveN/8o4LIRnTyv8231+qc4yABgJXea3sx5wr97U3qRnfK0gIiUSBz9oa4sPme08iAuidXH/SCkRUPRh9oejiYAS49z3L8EQnFk4sUTGzM9Pw4dmk8Gz41NrGfp3xRD6pV1usc1uKs3sXghKi2QLeM8wRB/n24Fd64Ojeo8BMomDbSxpyMyPIpK0FJPOGx+gFlYsBCWpBPSCDmfuZ9tpi5AJecF3dJT+GYzjvwTtjLxgHb6urakUBrsejK2vWSxoqAVEtubs9c5T+Bic/4mREqUstB7t3TyfnZEMMg6g2eJ6qvx1PNqFHnnCwgx3jKuGURZugMCPISygCQ0Obo0nvzcYEkTTOmpa1A96Wc8QKlh0bv7F/5P2iuhsTIiUjc4IIC8+LKBSbs2jDNMAKWYHzxBMNAAMiQxJK8Y3BPlofBqmTb24U8wjFyDjdpY3KMDRWQCHlYQV4RIlJXmamm0qlVOhRS+oqx0LhbJy1fpNzjEmrxezMSjIkXBxHGP9CsYXqjGBtZdlVcn6wJYBWj1AN+Klyl/YJ0SrvYTgYL2zWZBsVp4Ft1BgsRhPP2I1XnwxzabIfW2IMT7LUS76kITPDw9lV9dyxjPi73W7qbAqjLyvoYI22BAK1t7CQwH67IQ2rqcYuzRMnaBjZlJLm8rkLCbysoycnuwfbWSuo6tX3QhtWPHzayyR34m7Y9TZ5h7RWKqtDobXO0pO5c8wgqUsBTtHV+dE0JuMUz7RFI9jsruqf+YeS28Gw0u+3fX15BJf4fuugcOV4R14ddwZh/6GJFvAMKV+4IQw1Volt0SEa9z/2jbnv5TvG/DornKdMxI6lcgldC5ZHrSU1WuHL2e3BF9IMgyliVghv4IxonfZHGfux8sGOn2zavskMkijAWOJ1qnA4D1YoC5R9DtoeTUjDM/8c7OUw=*)

let replace2 arr pairs (dx,dy) =
  let rec get_to_be_reped pairs acc =
    match pairs with
    | [] -> acc
    | x::v -> (
      if Array.exists ((=) x) arr then
        get_to_be_reped v (x::acc)
      else
        get_to_be_reped v acc
    )
  in
  let rec inner pairs (dx,dy) =
    match pairs with
    | [] -> ()
    | (x,y)::v -> (
      let idx = Array.find_index ((=) (x,y)) arr |> Option.get in
      arr.(idx) <- (x+dx,y+dy);
      inner v (dx,dy)
    )
  in

  let l = get_to_be_reped pairs [] in
  inner l (dx,dy);;

let baxamem needle baxas =
  Array.exists (fun (x,y) -> (x,y) = needle || (x,y+1) = needle) baxas;;

let movebaxas2 walls baxas (x,y) (dx,dy) =
  let rec canmove i acc =
    if i >= List.length acc then Some acc else
      let (x,y) = List.nth acc i in
      let (x,y) = (x+dx,y+dy) in
      let l = ref acc in

      if List.mem (x,y) walls then None

      else if baxamem (x,y) baxas then
        (
          if List.mem (x,y) acc |> not then l := !l @ [(x,y)];

          if Array.mem (x,y) baxas then (*[*)
            if List.mem (x,y+1) acc |> not then l := !l @ [(x,y+1)];

          if Array.exists (fun (a,b) -> (a,b+1) = (x,y)) baxas then (*]*)
            if List.mem (x,y-1) acc |> not then l := !l @ [(x,y-1)];

          canmove (i+1) !l
        )

      else canmove (i+1) acc
  in
  canmove 0 [(x,y)];;

let makemove2 walls baxas (x,y) (dx,dy) =
  let finol = (x+dx,y+dy) in
  if List.mem finol walls then (x,y)
  else if baxamem finol baxas then
    match movebaxas2 walls baxas (x,y) (dx,dy) with
    | None -> (x,y)
    | Some acc -> (
      replace2 baxas acc (dx,dy);
      finol
    )
  else finol;;

let part2 file =
  (* Parse *)
  let (walls, baxas, (btx,bty), moves) = parse file in
  let rec convert acc l =
    match l with
    | [] -> acc
    | (x,y)::v -> convert ([(x,2*y); (x,2*y+1)] @ acc) v
  in
  let walls = convert [] walls in
  let rec convert acc l =
    match l with
    | [] -> acc
    | (x,y)::v -> convert ((x,2*y)::acc) v
  in
  let baxas = Array.to_list baxas |> convert [] |> Array.of_list in
  let bot = (btx, 2*bty) in

  (* Main *)
  let rec inner moves bot =
    match moves with
    | [] -> bot
    | move::v -> inner v (makemove2 walls baxas bot move)
  in
  let _ = inner moves bot in
  Array.fold_left (fun acc (x,y) -> acc + 100*x + y) 0 baxas ;;

print_int (part2 "inp.txt")
