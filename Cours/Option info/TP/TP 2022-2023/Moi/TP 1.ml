(* C *)
(* 1 *)
(* Il y en a 2. *)

(* 2 *)
(* a *)


let board = [0;2;1;3];;

let show board =
  let print_row v =
    for i = 0 to (List.length board) - 1 do
      print_string (if i=v then "\u{2655} " else ". ");
    done;
    print_newline()
  in List.iter print_row board; print_newline();;


(* b *)
let rec_show board =
  let rec aux n = function
 | [] -> print_newline();
 | h::q -> for i = 0 to n-1 do 
            print_string (if i==h then "\u{2655}" else ". ");
            done;
            print_newline();
            aux n q;
          in aux (List.length board) board;;

(* 3 *)
(* a *)
let same_col c board = List.mem c board;;


let rec rec_same_col c = function
| [] -> false
| h::q -> if (h==c) then true else rec_same_col c q;;



(* b *)
let rec rec_same_row r board = match r with
| 0 -> (List.hd board == -1);
| r -> rec_same_row (r-1) (List.tl board);;


let same_row r board = (List.nth board r) != -1;;


(* c *)
let up_diag r c board =
  let rc = r + c
  in let diag board = List.mapi (fun index elem -> if elem != -1 then Some (
  index + elem) else None) board
  in List.mem (Some rc) (diag board);;


let rec_up_diag r c board =
  let rc = r + c
  in let rec diag b i = match b with
  | [] -> []
  | head::tail -> if head != -1 then (i + head)::(diag tail (i + 1)) else
  diag tail (i + 1)
  in List.mem rc (diag board 0);;




let down_diag r c board =
  let rc = r-c
in let diag board = List.mapi (fun index elem -> if elem != -1 then Some (index - elem) else None) board
in List.mem (Some rc) (diag board);;


let rec_down_diag r c board =
  let rc = r-c
in let rec diag b i = match b with
| [] -> []
| h::q -> if (h != -1) then (i-h)::(diag q (i+1)) else diag q (i+1)
in List.mem rc (diag board 0);;


(* 4 *)

let under_attack r c board = 
  let rec mask r c board = match r with 
  | 0 -> (if (List.hd board == c) then -1 else (List.hd board))::(List.tl board)
  | h -> (List.hd board)::(mask (r-1) c (List.tl board))
  in let masked_board = mask r c board in ((same_row r masked_board) || (same_col c masked_board) || (down_diag r c masked_board) || (up_diag r c masked_board));;



(* 5 *)

let valid_solution board =
  let rec list_rc r b = match b with
  | [] -> []
  | h::q -> (if (h != -1) then (r,h)::(list_rc (r+1) q) else (list_rc (r+1) q))
in let rec aux cond = function
| [] -> cond
| h::q -> aux (cond || under_attack (fst h) (snd h) board) q
in (aux false (list_rc 0 board));;


(* 6 *)

let quatre_reine =
  let nbr = ref 0 in
  for i = 0 to 3 do
    for j = 0 to 3 do
      for k = 0 to 3 do
        for l = 0 to 3 do
          nbr := !nbr + if (valid_solution [i;j;k;l]) then 1 else 0
        done;
      done;
    done;
  done;
  !nbr;;

(* 7 *)

(* a *)

let rec rm elt = function 
  | [] -> [] 
  | h::q when h == elt -> rm elt q
  | h::q -> h::(rm elt q);;

(* b *)

let create_board n = 
  let rec aux = function
  | k when k == n -> []
  | k -> k::(aux (k+1))
in aux 0;;


(* c *)

let rec permutations = function
| [] -> []
| x::[] -> [[x]]
| l -> List.fold_left (fun acc x -> acc @ List.map (fun p -> x::p) (permutations (rm x l))) [] l;;


(* 8 *)

let brute_force_permutation n =
  let board = create_board n in
  let permuted_boards = permutations board in
  let rec aux acc = function 
  | [] ->  print_int acc
  | h::q -> let cond = valid_solution h in
            if cond then ((show h);
            if cond then (aux (acc+1) q)) else (aux (acc) q);
in aux 0 permuted_boards;;


(* D *)