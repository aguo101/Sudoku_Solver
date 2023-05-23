open Base
open Stdio

let sudoku_size = 9

let fill_line puzzle line_count line =
  for i = 0 to (sudoku_size - 1) do
    puzzle.(line_count).(i) <- line.(i)
  done

let rec read_puzzle puzzle line_count = 
  if line_count = sudoku_size then Some puzzle else
    let line = In_channel.input_line In_channel.stdin in
    match line with
    | None -> None
    | Some str -> let line_arr = (Array.of_list (List.map ~f:Int.of_string (String.split ~on:' ' str))) in
                    if Array.length line_arr <> sudoku_size then None
                    else (fill_line puzzle line_count line_arr; read_puzzle puzzle (line_count + 1))

let print_puzzle puzzle =
  for i = 0 to (sudoku_size - 1) do
    for j = 0 to (sudoku_size - 1) do
      printf "%d " puzzle.(i).(j)
    done;
    printf "\n"
  done

let rec row_conflict puzzle row i nums =
  if i = sudoku_size then false
  else if puzzle.(row).(i) <> 0 && Set.mem nums puzzle.(row).(i) then true
  else row_conflict puzzle row (i + 1) (Set.add nums puzzle.(row).(i))

let rec col_conflict puzzle col i nums =
  if i = sudoku_size then false
  else if puzzle.(i).(col) <> 0 && Set.mem nums puzzle.(i).(col) then true
  else col_conflict puzzle col (i + 1) (Set.add nums puzzle.(i).(col))

let rec box_conflict puzzle box i nums =
  if i = sudoku_size then false
  else
    let row = (i / 3) + (3 * (box / 3)) in
    let col = (i % 3) + (3 * (box % 3)) in
    if puzzle.(row).(col) <> 0 && Set.mem nums puzzle.(row).(col) then true
    else box_conflict puzzle box (i + 1) (Set.add nums puzzle.(row).(col))

let has_conflict puzzle row col =
  row_conflict puzzle row 0 (Set.empty (module Int)) ||
  col_conflict puzzle col 0 (Set.empty (module Int)) ||
  box_conflict puzzle ((3 * (row / 3)) + (col / 3)) 0 (Set.empty (module Int))

let rec solve puzzle i =
  if i = (sudoku_size * sudoku_size) then Some puzzle
  else
    let row = i / sudoku_size in
    let col = i % sudoku_size in
    if puzzle.(row).(col) <> 0 then
      solve puzzle (i + 1)
    else
      let solution = ref None in
      let num = ref 1 in
      while (match !solution with None -> true | Some _ -> false) && (!num <= 9) do
        puzzle.(row).(col) <- !num;
        (if not (has_conflict puzzle row col) then
          solution := solve puzzle (i + 1));
        num := !num + 1
      done;
      (if (match !solution with None -> true | Some _ -> false) then
        puzzle.(row).(col) <- 0);
      !solution

let () =
  let puzzle_opt = read_puzzle (Array.make_matrix ~dimx:sudoku_size ~dimy:sudoku_size 0) 0 in
  match puzzle_opt with
    | None -> printf "Error\n"
    | Some puzzle ->
      printf "\n";
      match solve puzzle 0 with
      | None -> printf "No Solution\n"
      | Some solution -> print_puzzle solution
  