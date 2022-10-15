(* C *)
(* 1 *)

(* Il y en a 2 *)

(* 2 *)

(* a *)

let test_board = [2;0;3;1];;

let show board =
  let print_row c =
    for i = 0 to ((List.length board)-1) do
      print_string (if i=c then "\u{2655} " else ". ");
    done;
    print_newline();
  in List.iter print_row board;;


let rec_show board =
  let print_row c = 
    for i = 0 to ((List.length board)-1) do
      print_string (if i=c then "\u{2655} " else ". ")
    done;
    print_newline();
  in let rec aux = function
  | [] -> print_newline()
  | h::q -> print_row h; aux q
in aux board;;



(* 3 *)

(* a *)
let same_col = (fun x board -> List.mem x board);;

let rec rec_same_col c = function
| [] -> false
| h::q -> if h=c then true else (rec_same_col c q);;

(* b *)

let same_row = (fun x board -> (List.nth board x != -1))


let rec_same_row r board =
  let rec aux acc lst = if (acc=r) then (List.hd lst != -1) else (aux (acc+1) (List.tl lst))
in aux 0 board;;


(* c *)

let down_diag r c board =
  let rc = r-c in
  let diag = (fun board -> List.mapi (fun row col -> if (col != -1) then Some (row - col) else None) board);
in (List.mem (Some (rc)) (diag board));;


