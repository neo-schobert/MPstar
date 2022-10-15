(* A / Liste et récursivité *)


(* 1 *)
let length l =
let rec aux l acc = match l with
| [] -> acc;
| h::q -> aux q (acc+1);
in aux l 0;;


(* 2 *)

let rec at i l = match i with
| 0 -> List.hd l
| i ->  at (i-1) (List.tl l);;



(* 3 *)

let rec mem e l = match (e,l) with 
| (_,[]) -> false
| (a,h::q) when h==a -> true
| (a, h::q) -> mem a q;;

(* 4 *)


let rec concat l1 l2 = match (l1) with
| [] -> l2
| h::q -> h::(concat q l2);;


(* 5 *)
let flatten l = 
let rec aux acc = function
| [] -> acc
| h::q -> aux (concat acc h) q
in aux [] l;;


(* 6 *)
let map f lst = List.fold_right (fun a b -> (f a)::b) lst [];;



(* 7 *)
let rec last = function 
| [] -> failwith "Error"
| h::[] -> failwith "Error"
| a::b::[] -> (a,b)
| h::q -> last q;;



(* 8 *)
let rev l =
  let rec aux acc = function 
  | [] -> acc
  | h::q -> aux (acc @ h) q
in aux [] l;;



(* B / Vecteurs et programmation impérative *)


(* 1 *)

let fibonacci n =
  let tab = Array.make n 0 in
  tab.(0) <- 1;
  tab.(1) <- 1;
  for k = 2 to n-1 do
    tab.(k)<- tab.(k-1) + tab.(k-2);
  done;
  tab.(n-1);;

(* 2 *)
let fibonnaci2 n = 
  let a = ref 1 in
  let b = ref 1 in
  for k = 2 to n-1 do
    a := !a+ !b;
    b := !a- !b;
  done;
  !a;;

(* 3 *)
let mem x tab =
  let n = ref ((Array.length tab)-1) in
  let cond = ref true in
  while !n>=0 && !cond do
    cond := not (tab.(!n) == x);
    n := !n - 1;
  done;
  not !cond;;

(* 3 bis *)
let memtri x tab =
 let a = ref 0 in
 let b = ref ((Array.length tab)-1) in
 let c = ref (!b / 2) in
  while (!a < !b) do
    b := (if tab.(!c) >= x then (!c-1) else !b);
    a := (if tab.(!c) <= x then (!c+1) else !a);
    c := (!b + !a) / 2;
  done;
  (tab.(!a)==x);;


(* 4 *)

let v_concat v1 v2 = 
  let n1 = Array.length v1 and n2 = Array.length v2 in
  let v3 = Array.make (n1+n2) 0 in
  for k = 0 to (n1-1) do
    v3.(k) <- v1.(k);
  done;
  for k = 0 to (n2-1) do
    v3.(n1 + k) <- v2.(k);
  done;
  v3;;


let m_concat m1 m2 = 
  let nl = Array.length m1 in
  let c1 = Array.length m1.(0) in
  let c2 = Array.length m2.(0) in
  let m3 = Array.make_matrix nl (c1+c2) m1.(0).(0) in
  for k = 0 to (nl-1) do
    for j = 0 to (c1-1) do
      m3.(k).(j) <- m1.(k).(j)
    done;
    for j = 0 to (c2-1) do
      m3.(k).(c1+j) <- m2.(k).(j)
    done;
  done;
  m3;;


  let array_map f tab = 
    let n = Array.length tab in
    for k = 0 to (n-1) do 
      tab.(k) <- f tab.(k)
    done;
    tab;;


(* C / Tris *)

(* 1 *)
(* a *)
let rec insert_elem lst e = match lst with
| [] -> [e]
| h::q when h > e -> e::h::q
| h::q -> h::(insert_elem q e);;

(* c *)
let insert_sort lst =
  let rec aux acc = function
  | [] -> acc
  | h::q -> aux (insert_elem acc h) q
in aux [] lst;; 

(* b *)
let tri_insert lst = List.fold_left (fun a b -> (insert_elem a b)) lst [];;


(* 2 *)



(* 3 *)



(* 4 *)
(* a *)
let split lst =
  let rec aux acc1 acc2 n = function
  | [] -> (acc1,acc2)
  | h::q when h>=0 -> aux 