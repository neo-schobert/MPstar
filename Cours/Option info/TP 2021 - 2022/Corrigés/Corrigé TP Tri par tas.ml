(* Tri par tas *)

(* Fonction d'�change *)

let swap t i j =
  let x = t.(i) in t.(i) <- t.(j) ; t.(j) <- x ;;

(* Fonction pour faire monter un �l�ment *)
  
let monte t k =
  let rec aux i = match i with
    | i when i < 2 -> ()
    | i -> let j = i/2 in 			(* j est le p�re de i *)
	   if t.(i) > t.(j) then (swap t i j; aux j)
  in aux k ;;

(* Fonction pour faire descendre un �l�ment *)

let descend t n k = 			(* n est l'indice de fin du tas *)
  let rec aux i  = match i with
    | i when 2*i > n -> () 		(* cas o� i n'a pas de fils *)
    | i -> let j = if 2*i = n || t.(2*i) > t.(2*i+1) then 2*i else 2*i+1 in 
	   if t.(i) < t.(j) then (swap t i j; aux j) (* j est le plus grand fils de i *)
  in aux k;;

(* Fonction qui cr�e un tas par mont�es successives *)

let creer_tas_montee t =
  for k = 2  to Array.length t - 1 do monte t k done ;;

(* Fonction qui cr�e un tas par descentes successives *)

let creer_tas_descente t =
  let n = Array.length t -1 in
  for k = n/2 downto 1 do descend t n k done ;;

(* Fonction de tri par tas *)

let tri_tas t =
  creer_tas_descente t ;
  let n = Array.length t -1 in
  for k = n downto 2 do
    swap t 1 k ;			(* on met le plus grand �l�ment � sa place *)
    descend t (k-1) 1;	       	(* on recr�e le tas form� des k-1 �l�ments restants *)
  done ;;

let t = [|0; 1; 4; 5; 2; 7; 3 |];;

tri_tas t ;;

t;;
