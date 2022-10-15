(* C *)

let n = 4;;

let test_board = [2;0;3;2];;

(* 2 *)
(* a *)

let show board =
  let print_row v =
    for i = 0 to ((List.length board)-1) do
      print_string (if i==v then "\u{2655} " else ". ");
    done;
    print_newline();
  in List.iter print_row board;;


(* b *)

let rec_show board =
  let print_row v =
    for i = 0 to ((List.length board) - 1) do
      print_string (if i==v then "\u{2655} " else ". ")
    done;
    print_newline();
  in let rec aux = function
  | [] -> print_newline();
  | h::q -> print_row h; aux q;
in aux board;;

(* 3 *)
(* a *)

let same_col (c : int) = fun x-> List.mem c x;;


let rec rec_same_col (c : int) = function
| [] -> false
| h::q -> if (h = c) then true else (rec_same_col c q);;

(* b *)

let same_row r = fun x -> ((List.nth x r) != -1);;


let rec_same_row r board =
  let rec aux i = function
  | [] -> false
  | h::q -> if (i==r && h != -1) then true else aux (i+1) q
in aux 0 board;; 


(* c *)

let up_diag r c board =
  let rc = r+c in 
  let diag board = List.mapi (fun index elt -> (if elt != -1 then Some (index + elt) else None)) board
in List.mem (Some rc) (diag board);;

let rec_up_drag r c board =
  let rc = r + c in
  let rec diag i = function
  | [] -> [] 
  | h::q -> if (h != -1) then (i+h)::(diag (i+1) q) else (diag (i+1) q)
in List.mem rc (diag 0 board);;


(* d *)

let down_diag r c board = 
  let rc = r-c in
  let diag board = List.mapi (fun index elt -> if elt != -1 then Some (index - elt) else None) board 
in List.mem (Some rc) (diag board);;


let rec_down_diag r c board =
  let rc = r-c in
  let rec diag i = function
  | [] -> []
  | h::q -> (if h != -1 then (i-h)::(diag (i+1) q) else (diag (i+1) q))
in List.mem rc (diag 0 board);;


(* 4 *)

let under_attack r c board =
  let masked = List.mapi (fun index elt -> if (index = r && elt = c) then -1 else elt) board
in (same_row r masked) || (same_col c masked) || (up_diag r c masked) || (down_diag r c masked);;


let rec_under_attack r c board =
  let rec aux index = function 
  | [] -> []
  | h::q -> if (index=r && h=c) then -1::(aux (index+1) q) else (aux (index+1) q)
in let masked = aux 0 board in 
(same_row r masked) || (same_col c masked) || (up_diag r c masked) || (down_diag r c masked);;


(* 5 *)

let valid_solution board = 
  let lst_attack = List.mapi (fun r c -> under_attack r c board) board in 
  not (List.mem true lst_attack);;


let rec_valid_solution board = 
  let rec aux index = function 
  | [] -> true 
  | h::q -> (under_attack index h board) || aux (index+1) q
in aux 0 board;;


(* 6 *)

let raw_force_4_queens () =
  let board = []
  in for i=0 to n - 1 do
    let bi = i::board in
    for j=0 to n - 1 do
      let bij = j::bi in
      for k=0 to n - 1 do
        let bijk = k::bij in
        for l=0 to n - 1 do
          let bijkl = l::bijk in
            if valid_solution bijkl then (Printf.printf "[%i,%i,%i,%i]\n"
              i j k l; rec_show bijkl;)
        done;
      done;
    done;
  done;;


(* 7 *)
(* a *)

let rec rm elt = function 
| [] -> [] 
| h::q -> if (elt = h) then rm elt q else h::(rm elt q);;


let rm elt lst = List.filter (fun x -> x != elt) lst;; (* fun x -> x != elt équivaut à fun x -> x != elt) *)


(* b *)

let create_board n =
  let rec aux i l = if (i = n) then l else i::(aux (i+1) l)
in aux 0 [];;

let create_board n = List.init n (fun x -> x);;

