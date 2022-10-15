(* A *)

(* 1 *)


(* Le graphe g est donné sous forme d'une liste d'adjacence *)


(* 2 *)

(* Ce graphe est connexe acyclique, chaque sommet étant de degré au plus 3. Il s'agit donc d'un arbre binaire*)



(* 3 *)

let list_to_matrix g =
  let n = Array.length g in
  let m = Array.make_matrix n n false in
  Array.iteri (fun index voisins -> List.iter (fun voisin -> m.(index).(voisin) <- true) voisins) g;
m;;



(* 4 *)

let matrix_to_list m = 
  let n = Array.length m in
  let g = Array.make n [] in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      if m.(i).(j) then g.(i) <- j::g.(i);
    done;
  done;
g;;



(* 5 *)

let desoriented_list g =
  Array.mapi (fun index voisins -> List.iter (fun voisin -> if not (List.mem index g.(voisin)) then g.(voisin) <- index::g.(voisin) ) voisins; g.(index)) g;;



(* 6 *)
let desoriented_matrix m =
let n = Array.length m in
for i = 0 to n-1 do
  for j = 0 to n-1 do
    m.(i).(j) <- m.(j).(i) || m.(i).(j)
  done;
done;
m;;


(* B *)
(* 1 *)
let bfs g v0 =
  let visited = Array.make (Array.length g) false in
  let rec explore queue = (* queue -> file en anglais FIFO *)
  match queue with
  | [] -> []
  | h::q when visited.(h) -> explore q
  | h::q -> visited.(h) <- true; explore (h::(q @ g.(h)))
in explore [v0] ;;



(* 2 *)

let dfs g v0 =
  let visited = Array.make (Array.length g) false in
  let rec explore stack = match stack with
  | [] -> []
  | h::q when visited.(h) -> explore q
  | h::q -> visited.(h) <- true; explore (h::(g.(h) @ q))
in explore [v0];;

(* C *)

(* 1 *)
(* A chaque itération, on ajoute un élément distinct de ceux qu'on avait déjà dans dela. Le nombre d'éléments qu'on peut ajouter étant fini,
on s'apperçoit qu'à chaque itération, |delta barre| diminie strictement et est à valeur dans N. Donc le tant que termine. Cela conclue la terminaison
de cet algorithme (une boucle for se terminant toujours) *)


(* 2 *)






(* 3 *)
(* On a une boucle while qui itère |V| fois. Dans cette boucle est imbriquée une boucle for qui fait |delta barre| itérations. Au final,
chaque arrete est parcouru une unique fois par boucle for puisqu'on l'enlève de delta barre à chaque fois)
La complexité est alors en O(|E| + |V|) *)


(* 4 *)





















