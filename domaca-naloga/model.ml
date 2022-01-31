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
  (box_ind mod 3) * 3, (box_ind - (box_ind mod 3))

let rec box_fun (x1, y1) = function (* POMEMBNO: tukaj naredi memoizacijo *)
  | 0 -> x1, y1
  | n when n mod 3 = 0 -> box_fun ((x1 - 2), (y1 + 1)) (n - 1)
  | n -> box_fun ((x1 + 1), y1) (n - 1)

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

let is_a_subset_of (problem : problem) (solution : solution) = 
  let fun_comp row_ind col_ind prob_elem acc = match prob_elem with 
  | None -> true && acc
  | Some y -> (solution.(row_ind).(col_ind) = y) && acc in 
    foldi_grid fun_comp problem.initial_grid true

let is_valid_solution problem solution = 
  let bool1 = 
  (columns solution) @ (rows solution) @ (boxes solution)
  |> List.map Array.to_list 
  |> List.map is_regular
  |> List.fold_left ((&&)) true 
  in bool1 && is_a_subset_of problem solution

