type available = { loc : int * int; possible : int list }


(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { problem : Model.problem; current_grid : int option Model.grid; available_grid: available Model.grid }

let squares_of_two (grid : int option Model.grid)  (available_grid : available Model.grid) =
  let fun_comp row_ind col_ind (cell : int option) acc = match cell with
  | Some n -> acc
  | None -> if List.length (available_grid.(row_ind).(col_ind).possible) = 2 
    then Array.append [|(row_ind, col_ind)|] acc
  else acc in 
  Model.foldi_grid fun_comp grid [||]

(* spremeni seznam v type grid *)
let sudokuify list =
  list |> Model.chunkify 9 |> List.map Array.of_list |> Array.of_list
let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid



(* vsaki točki v gridu dodeli možne rešitve *)
let available_option row_ind col_ind (grid : int option Model.grid) n =
  let elem = [Model.get_row grid row_ind; Model.get_column grid col_ind; 
  Model.get_box grid (Model.point_to_box row_ind col_ind)] 
  |> List.map Array.to_list |> List.concat |> List.mem (Some n) 
in if elem then false else true

let assign_available row_ind col_ind (grid : int option Model.grid) (n : int option) =
  let lst = List.filter (fun n -> available_option row_ind col_ind grid n) 
    [1; 2; 3; 4; 5; 6; 7; 8; 9] 
  in 
    match n with
    | None -> {loc = (row_ind, col_ind); possible = lst}
    | Some _ -> {loc = (row_ind, col_ind); possible = []}

let create_available_array (grid : int option Model.grid) =
  let folding_function row_ind col_ind (n : int option) (acc : available list) = 
    acc @ [(assign_available row_ind col_ind grid n)]
  in
    sudokuify (Model.foldi_grid folding_function grid [])


(* če ima točka v gridu le en sam available, ga zapolni *)
let match_availables (available : available) (n : int option) = 
  if n = None then
    match available.possible with 
    | x :: [] -> Some x
    | _ -> None
  else n

let fill_in_solutions (grid : int option Model.grid) (available_grid : available Model.grid) =
  let folding_function row_ind col_ind (n : int option) (acc : int option list) =
    acc @ [match_availables (available_grid.(row_ind).(col_ind)) n]
  in 
    sudokuify (Model.foldi_grid folding_function grid [])


(* izpolni gotove rešitve *)



type response = Solved of Model.solution | Unsolved of state | Fail of state

let initialize_state (problem : Model.problem) : state =
  { current_grid = Model.copy_grid problem.initial_grid; problem; available_grid = create_available_array problem.initial_grid }

  let validate_state (state : state) : response =
    let unsolved =
      Array.exists (Array.exists Option.is_none) state.current_grid (* pogleda, če je še kakšno nerešeno polje*)
    in
      if unsolved then 
        if Model.is_almost_valid state.current_grid then Unsolved state
        else Fail state
      else
        (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
        let solution = Model.map_grid Option.get state.current_grid in
          if Model.is_valid_solution state.problem solution then Solved solution
          else Fail state
    let rec update_state (state : state) =      
      let new_grid = fill_in_solutions (Model.copy_grid state.current_grid) (Model.copy_grid state.available_grid) in
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
  let new_grid = Model.copy_grid grid in
    (new_grid).(row_ind).(col_ind) <- new_value;
  new_grid

let branch_state (state : state) : (state * state) option =
  let row_ind, col_ind = (squares_of_two state.current_grid state.available_grid).(0) 
  in
    let new_array = Array.of_list state.available_grid.(row_ind).(col_ind).possible 
    in
      Some ({problem = state.problem; current_grid = (insert_into_grid (state.current_grid) row_ind col_ind (Some new_array.(0))); available_grid = (Model.copy_grid state.available_grid)}, 
      {problem = state.problem; current_grid = (insert_into_grid (state.current_grid) row_ind col_ind (Some new_array.(1))); available_grid = (Model.copy_grid state.available_grid)})
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

let solve_problem (problem : Model.problem) =
  problem |> initialize_state |> solve_state
