(* Pomožni tip, ki predstavlja mrežo *)

type 'a grid = 'a Array.t Array.t

(* Funkcije za prikaz mreže.
   Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)

(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

let string_of_list string_of_element sep lst = (* vzame a) funkcijo, ki element pretvori v string, b) ločilo, c) seznam*)
  lst |> List.map string_of_element |> String.concat sep (*seznam pretvori v seznam stringov in ga združi s ločilom*)

let string_of_nested_list string_of_element inner_sep outer_sep = (*seznam seznamov pretvori v string zdruzenih stringov*)
  string_of_list (string_of_list string_of_element inner_sep) outer_sep

let string_of_row string_of_cell row =
  let string_of_cells =
    row |> Array.to_list |> chunkify 3 (* array row spremeni v seznam in ga spremeni v seznam seznamov dolžine 3, nato tega spremeni v string *)
    |> string_of_nested_list string_of_cell "" "│"
  in
  "┃" ^ string_of_cells ^ "┃\n"

let print_grid string_of_cell grid =
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in (* meja med trojicami vrstic*)
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider (* grid spremeni v seznam, ga chunkifyja po 3, vrstica razdeli z | in nato vsako trojico vrstic razdeli z dividerjem*)
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big


(* Funkcije za dostopanje do elementov mreže *)

let get_row (grid : 'a grid) (row_ind : int) = (* funkcija, ki dobi grid in vrne array določene vrstice*)
  grid.(row_ind)

let rows grid = 
  List.init 9 (get_row grid) (* get_row je curryirana funkcija, ki za indeks vrne določeno vrstico*)

let get_column (grid : 'a grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind))

let columns grid = List.init 9 (get_column grid)

let upper_left_corner (box_ind : int) = 
  (box_ind - (box_ind mod 3)), (box_ind mod 3) * 3

let point_to_box row_ind col_ind =
  (col_ind - col_ind mod 3)/3 + (row_ind - row_ind mod 3)

let rec box_fun (row_ind, col_ind) = function (* POMEMBNO: tukaj naredi memoizacijo *)
  | 0 -> row_ind, col_ind
  | n when n mod 3 = 0 -> box_fun ((row_ind + 1), (col_ind - 2)) (n - 1)
  | n -> box_fun (row_ind, (col_ind + 1)) (n - 1)

let get_box (grid : 'a grid) (box_ind : int) = 
  Array.init 9 (fun box_int_ind -> grid.(fst (box_fun (upper_left_corner box_ind) box_int_ind)).(snd (box_fun (upper_left_corner box_ind) box_int_ind)))
(* v poteku funkcije je box_ind fiksirana *)
let boxes grid = List.init 9 (get_box grid)

(* Funkcije za ustvarjanje novih mrež *)

let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = Array.init 9 (fun row_ind -> Array.map f grid.(row_ind))

let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid

let foldi_grid (f : int -> int -> 'a -> 'acc -> 'acc) (grid : 'a grid)
    (acc : 'acc) : 'acc =
  let acc, _ =
    Array.fold_left
      (fun (acc, row_ind) row ->
        let acc, _ =
          Array.fold_left
            (fun (acc, col_ind) cell ->
              (f row_ind col_ind cell acc, col_ind + 1))
            (acc, 0) row
        in
        (acc, row_ind + 1))
      (acc, 0) grid
  in
  acc

let row_of_string cell_of_char str = (* preslika vsak element stringa v celico v seznamu s funkcijo cell_of_char*)
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char

let grid_of_string cell_of_char str =
  let grid =
    str |> String.split_on_char '\n' (* preslika string v seznam stringov, ki so bili loceni z \n*)
    |> List.map (row_of_string cell_of_char) (* seznam teh stringov se preslika v seznam vrstic*)
    |> List.filter (function [] -> false | _ -> true) (* izloci prazne vrstice *)
    |> List.map Array.of_list |> Array.of_list (*spremeni elemente (vrstice) v arraye, nato se cel seznam spremeni v array*)
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  grid

(* Model za vhodne probleme *)

type problem = { initial_grid : int option grid }

let string_of_int_option (n : int option) = match n with
| None -> " "
| Some k -> string_of_int k

let print_problem problem : unit = 
  print_grid string_of_int_option problem.initial_grid

let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0'))
    | _ -> None
  in
  { initial_grid = grid_of_string cell_of_char str }

(* Model za izhodne rešitve *)

type solution = int grid

let print_solution solution = print_grid string_of_int solution

let rec is_regular = function 
| [] -> true
| list -> let lista, listb = 
  List.partition (fun x -> x = (List.length list)) list in
    if List.length lista = 1 then is_regular listb else false

let rec is_almost_regular acc = function
| [] -> true
| None :: lst -> is_almost_regular acc lst
| Some n :: lst -> if List.mem (Some n) acc then false
else is_almost_regular (Some n :: acc) lst

let is_a_subset_of (problem : problem) (solution : solution) = 
  let fun_comp row_ind col_ind prob_elem acc = match prob_elem with 
  | None -> true && acc
  | Some y -> (solution.(row_ind).(col_ind) = y) && acc 
  in 
    foldi_grid fun_comp problem.initial_grid true

let is_valid_solution problem solution = 
  let bool1 = 
  (columns solution) @ (rows solution) @ (boxes solution)
  |> List.map Array.to_list 
  |> List.map is_regular
  |> List.fold_left ((&&)) true 
  in bool1 && is_a_subset_of problem solution

let is_almost_valid grid =
  (columns grid) @ (rows grid) @ (boxes grid)
  |> List.map Array.to_list 
  |> List.map (is_almost_regular [])
  |> List.fold_left ((&&)) true

  (*---------------------------------------------------------------------------*)

  type available = { loc : int * int; possible : int list }


  (* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
     želeli imeti še kakšno dodatno informacijo *)
  type state = { problem : problem; current_grid : int option grid; available_grid: available grid }
  
  let squares_of_two (grid : int option grid)  (available_grid : available grid) =
    let fun_comp row_ind col_ind (cell : int option) acc = match cell with
    | Some n -> acc
    | None -> if List.length (available_grid.(row_ind).(col_ind).possible) = 2 
      then Array.append [|(row_ind, col_ind)|] acc
    else acc in 
    foldi_grid fun_comp grid [||]
  
  (* spremeni seznam v type grid *)
  let sudokuify list =
    list |> chunkify 9 |> List.map Array.of_list |> Array.of_list
  let print_state (state : state) : unit =
    print_grid
      (function None -> "?" | Some digit -> string_of_int digit)
      state.current_grid
  
  
  
  (* vsaki točki v gridu dodeli možne rešitve *)
  let available_option row_ind col_ind (grid : int option grid) n =
    let elem = [get_row grid row_ind; get_column grid col_ind; 
    get_box grid (point_to_box row_ind col_ind)] 
    |> List.map Array.to_list |> List.concat |> List.mem (Some n) 
  in if elem then false else true
  
  let assign_available row_ind col_ind (grid : int option grid) (n : int option) =
    let lst = List.filter (fun n -> available_option row_ind col_ind grid n) 
      [1; 2; 3; 4; 5; 6; 7; 8; 9] 
    in 
      match n with
      | None -> {loc = (row_ind, col_ind); possible = lst}
      | Some _ -> {loc = (row_ind, col_ind); possible = []}
  
  let create_available_array (grid : int option grid) =
    let folding_function row_ind col_ind (n : int option) (acc : available list) = 
      acc @ [(assign_available row_ind col_ind grid n)]
    in
      sudokuify (foldi_grid folding_function grid [])
  
  
  (* če ima točka v gridu le en sam available, ga zapolni *)
  let match_availables (available : available) (n : int option) = 
    if n = None then
      match available.possible with 
      | x :: [] -> Some x
      | _ -> None
    else n
  
  let fill_in_solutions (grid : int option grid) (available_grid : available grid) =
    let folding_function row_ind col_ind (n : int option) (acc : int option list) =
      acc @ [match_availables (available_grid.(row_ind).(col_ind)) n]
    in 
      sudokuify (foldi_grid folding_function grid [])
  
  
  (* izpolni gotove rešitve *)
  
  
  
  type response = Solved of solution | Unsolved of state | Fail of state
  
  let initialize_state (problem : problem) : state =
    { current_grid = copy_grid problem.initial_grid; problem; available_grid = create_available_array problem.initial_grid }
  
  let validate_state (state : state) : response =
    let unsolved =
      Array.exists (Array.exists Option.is_none) state.current_grid (* pogleda, če je še kakšno nerešeno polje*)
    in
      if is_almost_valid state.current_grid then
          (if unsolved then Unsolved state
          else
            (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
            let solution = map_grid Option.get state.current_grid in
            if is_valid_solution state.problem solution then Solved solution
            else Fail state)
        else Fail state
  let rec update_state (state : state) =      
    print_grid string_of_int_option state.current_grid;
    let new_grid = fill_in_solutions (copy_grid state.current_grid) (copy_grid state.available_grid) in
      let new_available_grid = create_available_array new_grid in 
        let new_state = {problem = state.problem; current_grid = new_grid; available_grid = new_available_grid} in 
          match validate_state new_state with
          | Solved solution -> new_state
          | Fail fail -> new_state
          | _ -> if (Array.length (squares_of_two new_grid new_available_grid) > 0) then 
            new_state
          else 
            update_state new_state

  let insert_into_grid grid row_ind col_ind new_value = 
    let new_grid = copy_grid grid in
      (new_grid).(row_ind).(col_ind) <- new_value;
    new_grid
  
    let branch_state (state : state) : (state * state) option =
      let row_ind, col_ind = (squares_of_two state.current_grid state.available_grid).(0) 
      in
        let new_array = Array.of_list state.available_grid.(row_ind).(col_ind).possible 
        in
          Some ({problem = state.problem; current_grid = (insert_into_grid (state.current_grid) row_ind col_ind (Some new_array.(0))); available_grid = (copy_grid state.available_grid)}, 
          {problem = state.problem; current_grid = (insert_into_grid (state.current_grid) row_ind col_ind (Some new_array.(1))); available_grid = (copy_grid state.available_grid)})
    (* pogledamo, če trenutno stanje vodi do rešitve *)
    let rec solve_state (state : state) =
      (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)

      match validate_state (update_state state) with
      | Solved solution ->
          (* če smo našli rešitev, končamo *)
          Some solution
      | Fail fail ->
          (* prav tako končamo, če smo odkrili, da rešitev ni *)
          None
      | Unsolved state' ->
          (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
          explore_state state'
    
    and explore_state (state : state) =
      (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
      match branch_state state with
      | None ->
          (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
          None
      | Some (st1, st2) -> (
          (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
          match solve_state st1 with
          | Some solution ->
              (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
              Some solution
          | None ->
              (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
              solve_state st2 )
    
    let solve_problem (problem : problem) =
      problem |> initialize_state |> solve_state
    