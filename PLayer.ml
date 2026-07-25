type cell = int
type board = cell array

let convertir_plateau s =
  if String.length s <> 37 then failwith "bad board length";

  let b = Array.make 37 0 in 
  for i = 0 to 36 do
    b.(i) <- int_of_string (String.make 1 s.[i])
  done;
  b

let voisins : int list array = [|
  [1;2]; [0;5;6;2]; [0;1;6;7];

  [4;10]; [3;10;5;11]; [4;11;6;12;1]; [5;7;13;12;1;2]; [6;8;14;13;2];
  [7;9;14;15]; [8;15];

  [3;4;11;16]; [10;12;17;16;4;5]; [11;13;18;17;5;6];
  [12;14;19;18;6;7]; [13;15;20;19;7;8]; [14;9;20;8];

  [10;11;17;22;21]; [16;11;12;18;23;22];
  [17;13;12;19;24;23]; [18;14;20;25;24;13]; [19;15;26;25;14];

  [16;22;28;27];
  [21;17;23;29;28;16];
  [22;18;24;30;29;17];
  [23;19;25;31;30;18];
  [24;20;26;32;31;19];
  [20;25;32;33];

  [21;28];
  [27;22;29;21];
  [28;23;30;34;22];
  [29;24;31;35;34;23];
  [35;30;25;24;32];
  [31;26;33;25];
  [32;26];
  [29;30;35;36];
  [36;34;30;31];
  [34;35];
|]


let calculer_direction pion dist = 
	if pion = 0 then
		if dist = 1 then 
			1
		else if dist = 2 then 
			2
		else 
			-1
	else if pion = 1 then 
		if dist = 4 then 
			1
		else if dist = 5 then 
			2
		else if dist = 1 then
			3
		else if dist = -1 then 
			4
		else 
			-1
	else if pion = 2 then 
		if dist = 4 then 
			1
		else if dist = 5 then 
			2
		else if dist = -2 then
			5
		else if dist = 1 then 
			6
		else 
			-1
	else if pion >= 3 && pion <= 9 then 
		if dist = 6 then 
			1
		else if dist = 7 then 
			2
		else if dist = 1 then
			3
		else if dist = -4 then
			4
		else if dist = -5 then
			5
		else if dist = -1 then 
			6
		else 
			-1		
	else if pion >= 10 && pion <= 15 then 
		if dist = 5 then 
			1
		else if dist = 6 then 
			2
		else if dist = 1 then
			3
		else if dist = -6 then
			4
		else if dist = -7 then
			5
		else if dist = -1 then 
			6
		else 
			-1	
	else if pion >= 16 && pion <= 20 then 
		if dist = 5 then 
			1
		else if dist = 6 then 
			2
		else if dist = 1 then
			3
		else if dist = -5 then
			4
		else if dist = -6 then
			5
		else if dist = -1 then 
			6
		else 
			-1	
	else if pion >= 21 && pion <= 26 then 
		if dist = 6 then 
			1
		else if dist = 7 then 
			2
		else if dist = 1 then
			3
		else if dist = -5 then
			4
		else if dist = -6 then
			5
		else if dist = -1 then 
			6
		else 
			-1	
	else if pion >= 27 && pion <= 33 then 
		if dist = 4 then 
			1
		else if dist = 5 then 
			2
		else if dist = 1 then
			3
		else if dist = -6 then
			4
		else if dist = -7 then
			5
		else if dist = -1 then 
			6
		else 
			-1	
	else if pion = 34 || pion = 35 then 
		if dist = 1 then 
			1
		else if dist = 2 then 
			2
		else if dist = 1 then
			3
		else if dist = -4 then
			4
		else if dist = -5 then
			5
		else if dist = -1 then 
			6
		else 
			-1
	else if pion = 36 then
		if dist = -1 then 
			4
		else if dist = -2 then 
			5
		else 
			-1
	else 
		-1
type jump = { case_sautee : int; dest : int }




let calculer_sauts () =

  let concat_map f l = List.concat (List.map f l) in

  let tableau_sauts_for_cell i =

    let calc_jumps_for_neighbor case_sautee =

      let d1 = case_sautee - i in 
      let direct1 = calculer_direction i d1 in 

      if not (List.mem case_sautee voisins.(i)) || direct1 = -1 then
        []
      else
        voisins.(case_sautee)
        |> List.filter_map (fun dest ->
             let d2 = dest - case_sautee in
             let direct2 = calculer_direction case_sautee d2 in 

             if dest < 0 || dest >= 37 then
               None
             else if dest = i || dest = case_sautee then
               None
             else if direct2 = -1 || direct1 <> direct2 then
               None
             else
               Some { case_sautee; dest })
    in

    concat_map calc_jumps_for_neighbor voisins.(i)
  in

  Array.init 37 (fun i -> tableau_sauts_for_cell i)

let tableau_sauts = calculer_sauts ()


let sauts_multiples b player from =
  let rec dfs pos visited acc =
    List.fold_left (fun acc j ->
      if b.(j.case_sautee) <> 0 && b.(j.dest) = 0 &&
         not (List.mem j.dest visited)
      then
        dfs j.dest (j.dest :: visited) (j.dest :: acc)
      else acc
    ) acc tableau_sauts.(pos)
  in dfs from [from] []

let simple_moves b player from =
  if b.(from) <> player then []
  else List.filter (fun t -> b.(t) = 0) voisins.(from)

let coups_pour_pion b player from =
  let sm = simple_moves b player from in
  let jm = sauts_multiples b player from in
  List.map (fun d -> (from,d)) (sm @ jm)

let coups_du_joueur b player =
  let moves = ref [] in
  for i = 0 to 36 do
    if b.(i) = player then
      moves := coups_pour_pion b player i @ !moves
  done;
  !moves

let zone_objectif p =
  match p with
  | 1 -> [34;35;36]
  | 2 -> [21;27;28]
  | 3 -> [0;1;2]
  | 4 -> [3;4;10]
  | 5 -> [8;9;15]
  | 6 -> [26;32;33]
  | _ -> []

let distance_hex a b =
  abs (a - b)
let evaluer_coup to_pos player = 
  let gz = zone_objectif player in
  List.fold_left (fun acc g ->
    min acc (distance_hex to_pos g)
  ) 999 gz


let choisir_meilleur_coup b player =
  let moves = coups_du_joueur b player in

  if moves = [] then (0,0)
  else
    let score_coup (f, t) =
      let is_jump =
        List.exists (fun j -> j.dest = t) tableau_sauts.(f)
      in
      let jump_bonus = if is_jump then 20 else 0 in

      let dist_score = - evaluer_coup t player in

      jump_bonus + dist_score
    in

    List.fold_left
      (fun best move ->
         if score_coup move > score_coup best then move else best
      )
      (List.hd moves)
      moves
      
let () =
  if Array.length Sys.argv <> 3 then (
    Printf.printf "0:0\n"; exit 0
  );

  let board = convertir_plateau Sys.argv.(1) in
  let player = int_of_string Sys.argv.(2) in
  let (from_pos, to_pos) = choisir_meilleur_coup board player in
  Printf.printf "%d:%d\n" from_pos to_pos;
  exit 0