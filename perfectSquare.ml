open List;;

let dupExist lst =
  let rec dupExistAux lst b =
    match lst with
    | [] -> false
    | hd::tl -> 
      if (List.exists (fun x -> x = hd) b)
      then 
        true
      else
        dupExistAux tl (hd::b) in
  dupExistAux lst []

let rec find_subdivisions n curr_sum curr_size acc =
  if curr_sum = n*n then
    Some (List.rev acc)
  else if curr_sum > n*n then
    None
  else
    let next_size = curr_size + 1 in
    let max_size = n - (n mod curr_size) in
    let rec try_sizes s =
      if s > max_size then
        None
      else
        let new_sum = curr_sum + s*s in
        match find_subdivisions n new_sum next_size (s::acc) with
        | Some res -> Some res
        | None -> try_sizes (s+1)
    in try_sizes curr_size;;

let rec find_smallest_combination n curr_size curr_sum curr_acc smallest_combination =
  if curr_size >= n then
    match smallest_combination with
    | None -> []
    | Some res -> res
  else
    match find_subdivisions n curr_sum curr_size curr_acc with
    | Some res -> 
        let new_smallest_combination = match smallest_combination with
          | None -> Some res
          | Some smallest_res -> 
              if List.length res < 2 then Some smallest_res 
              else if dupExist res then Some smallest_res 
              else if List.length res < List.length smallest_res then Some res 
              else Some smallest_res
        in find_smallest_combination n (curr_size+1) curr_sum curr_acc new_smallest_combination
    | None -> find_smallest_combination n (curr_size+1) curr_sum curr_acc smallest_combination

let get_subdivision_sizes n =
  find_smallest_combination n 1 0 [] None;;
let rec combinations lst n =
  if n = 0 then [[]]
  else match lst with
  | [] -> []
  | x :: xs -> List.map (fun l -> x :: l) (combinations xs (n - 1)) @ combinations xs n;;

let init n s n_plate =
  let rec loop i =
    if i = n_plate then
      ([n] @ List.rev s @ [1], [1] @ s @ [n])
    else
      loop (i + 1) |> fun (p0, p1) -> ((n - (2 * n_plate)) :: p0, 1 :: p1)
  in
  loop 0;;

let haveSolution p =
  let len = List.length p in
  let rec loop i =
    if i = len - 1 then true
    else
      let a = List.nth p (i - 1) in
      let b = List.nth p i in
      let c = List.nth p (i + 1) in
      let cond1 = b < a - c + b in
      let cond2 = b < c - a + b in
      if cond1 && cond2 then false
      else loop (i + 1)
  in
  loop 1;;

let horizontal_ext p n_plate order =
  let n = List.hd p |> fun x -> x - List.hd (List.rev p) in
  let delta1 = List.nth p (n_plate - 1) - List.nth p n_plate in
  let delta2 = List.nth p (n_plate + 1) - List.nth p n_plate in
  let rec loop i =
    if i < 1 || i > n_plate then false
    else
      let d = if i < n_plate then delta2 else delta1 in
      if d > 0 && d + List.nth p i = n && not (List.mem d order) then
        let new_order = List.sort compare (d :: order) in
        if haveSolution (p, new_order) then true
        else vertical_ext p i new_order
      else
        loop (i + 1)
  in
  loop (n_plate - 1) || loop (n_plate + 1);;

let vertical_ext p n_plate order =
  let indices = List.mapi (fun i x -> (i, x)) (List.tl (List.rev (List.tl p))) |> List.sort (fun (_, a) (_, b) -> compare a b) |> List.map fst in
  let rec loop lst =
    match lst with
    | [] -> false
    | t :: ts ->
        if t = n_plate then loop ts
        else
          let d = List.nth p (t + 1) - List.nth p t in
          if d > 0 && not (List.mem d order) then
            let new_order = List.sort compare (d :: order) in
            if haveSolution (p, new_order) then true
            else loop ts
          else
            loop ts
  in
  loop indices;;

let verif n order p =

  if not (n * List.length p.(0) = List.fold_left (+) 0 p.(0)) then
    false
  else
    let add = ref 0 in
    List.iter (fun x -> add := !add + x*x) order;
    !add == n*n

let rec algo n n_plate = 
  if is_subdivisable n then
    let p = None in
    let li = [] in
    for x = 1 to n - (2 * n_plate) do
      li @ [x]
    done;
    let rec aux st = 
      if List.length st = n_plate && List.fold_left (+) 0 st = n && List.length (List.sort_uniq compare st) = List.length st then
        let order = [] in
        p = init n st n_plate;
        if haveSolution p then
          for x in st do
            order @ [x]
          done;
          let rec try_horizontal_ext p n_plate order = 
            let rec try_vertical_ext p n_plate order = 
              let indices = List.sort (fun (i1, v1) (i2, v2) -> compare v1 v2) (List.mapi (fun i v -> (i, v)) p.1) in
              let indices = List.filter (fun (i, v) -> i != 0 && i != (List.length p.0) - 1) indices in
              let rec loop = function
                | [] -> order
                | (t, v) :: indices' -> if vertical_ext p t order then try_horizontal_ext p n_plate order else loop indices'
              in
              loop indices
            and try_horizontal_ext p n_plate order = 
              let cond = ref false in
              for t = 1 to n_plate do
                let delta = [p.0.(t - 1) - p.0.(t); p.0.(t + 1) - p.0.(t)] in
                if delta.(0) = p.1.(t) && delta.(0) + p.0.(t) = List.fold_left (+) 0 st then
                  if vertical_ext p t order then
                    cond := true;
                    if verif n order p then order
                else if delta.(1) = p.1.(t) && delta.(1) + p.0.(t) = List.fold_left (+) 0 st then
                  if vertical_ext p t order then
                    cond := true;
                    if verif n order p then order
                else if t != n_plate && delta.(1) > 0 then
                  if horizontal_ext p t delta.(1) order 1 then
                    cond := true;
                    if verif n order p then order
                else if t != 1 && delta.(0) > 0 then
                  if horizontal_ext p t delta.(0) order -1 then
                    cond := true;
                    if verif n order p then order
              done;
              if verif n order p then order;
              if cond then try_horizontal_ext p n_plate order else try_vertical_ext p n_plate order
            in
            try_horizontal_ext p n_plate order
          in
          let res_order = try_horizontal_ext p n_plate order in
          if n = p.1.(1) + p.1.(2) + p.1.(3) && verif n res_order p then res_order
    else (print_string "No solution found for value "; print_int n; print_string " !\n"); None

r = algo 112 3;;