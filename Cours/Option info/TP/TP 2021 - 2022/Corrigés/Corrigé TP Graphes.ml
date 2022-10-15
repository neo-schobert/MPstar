let g = [| [1;2] ; [0;3;4] ; [0;5;6] ; [1] ; [1] ; [2] ; [2] |] ;;

(* Passage listes d'adjacence/matrice d'adjacence *)

let matadj g =
  let n = Array.length g in
  let m = Array.make_matrix n n false in
  let rec parcours l i = match l with
    |[] -> ()
    |n::q -> m.(i).(n) <- true; parcours q i
  in for i = 0 to n-1 do
      parcours g.(i) i;
  done;
  m ;;

let m = matadj g ;;

(* Passage matrice d'adjacence/listes d'adjacence *)

let listadj m =
  let n = Array.length m in
  let g = Array.make n [] in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      if m.(i).(j) then
	g.(i) <- j::g.(i)
    done;
  done;
  g ;;

listadj m ;;

(* Rendre non orienté un graphe orienté : version listes d'adjacence *)

let desorient1 g =
  let n = Array.length g in
  let rec aux l i = match l with
    |[] -> ()
    |n::q -> if not (List.mem i g.(n)) then g.(n) <- i::g.(n); aux q i
  in for i = 0 to n-1 do
      aux g.(i) i
  done;;

let g1 = [| [1;2] ; [3;4] ; [5;6] ; [] ; [] ; [] ; [] |] ;;

let m1 = matadj g1 ;;

desorient1 g1 ;;

g1 ;;

(* Rendre non orienté un graphe orienté : version matrice d'adjacence  *)

let desorient2 m = 
  let n = Array.length m in
  for i = 0 to n-1 do
    for j = i+1 to n-1 do
      if m.(i).(j) && (not m.(j).(i)) then
          m.(j).(i) <- true;
      if m.(j).(i) && (not m.(i).(j)) then
          m.(i).(j) <- true;
    done ;
 done ;;
      
desorient2 m1 ;;  

m1 ;;

listadj m1 ;;

(* Parcours en largeur *)

let largeur g n0 =
	let visites = Array.make (Array.length g) false in
	let rec explore l = match l with
		|[] -> []
		|n::q when visites.(n) -> explore q
		|n::q -> let voisins = g.(n) in
			 visites.(n) <- true;
			 n::(explore (q @ voisins)) 
	in explore [n0] ;;	

largeur g 0 ;;	

(* Parcours en profondeur *)

let profondeur g n0 =
	let visites = Array.make (Array.length g) false in
	let rec explore l = match l with
		|[] -> []
		|n::q when visites.(n) -> explore q
		|n::q -> let voisins = g.(n) in
			 visites.(n) <- true;
			 n::(explore (voisins @ q)) 
	in explore [n0] ;;	

profondeur g 0 ;;

(* Algorithme de Floyd-Warshall *)

let somme i j = 			(* on réécrit la somme pour inclure l'infini *)
  if i = max_int or j = max_int then max_int else i + j ;;

let floyd_warshall m = 			(* on utilise la matrice d'adjacence *)
  let w = Array.copy m and n = Array.length m in
  for k = 0 to n-1 do 			(* on calcule les d_{k+1} à partir des d_k *)
    for i = 0 to n-1 do
      for j = 0 to n-1 do
	w.(i).(j) <- min (w.(i).(j)) (somme w.(i).(k) w.(k).(j))
      done;
    done;
  done;
  w ;;
	
let m = [|[|0; max_int; max_int;1 |] ; [|1;0;2;max_int|] ; [|3;max_int;0;max_int|] ; [|max_int;7;4;0|]|] ;;

floyd_warshall m ;;

(* Algorithme de Dijkstra *)

(* Avec un tableau de sommets déjà visités : complexité O(n^2) *)

let dijkstra g = 			(* on représente g par listes d'adjacence, et on cherche les distances au sommet 0 *)
  let n = Array.length g in
  let d = Array.make n max_int and visites = Array.make n false in (* d contiendra les distances à 0 et visites les sommets déjà visités *)
  d.(0) <- 0;  			(* au début, tout le monde est à max_int sauf 0 *)
  visites.(0) <- true;
  let s = ref 0 in 		(* sommet courant que l'on considère *)
  let rec modif l = match l with 	(* fonction qui parcourt les voisins de s et modifie le tableau en conséquence *)
    |[] -> ()
    |(i,p)::q when not visites.(i) -> (d.(i) <- min d.(i) (d.(!s)+ p)); modif q;
    |(i,p)::q -> modif q
  in
  for k = 0 to n-1 do
    modif g.(!s);
    let min = ref max_int in
    for i = 1 to n-1 do 		(* on cherche l'élément de distance minimale parmi ceux non encore visités *)
      if (not visites.(i)) && (d.(i) < !min) then
        (s := i;
	min := d.(i);)
    done ;
    visites.(!s) <- true
  done ;
  d ;;

let g = [| [(1,7);(2,1)] ; [(3,4); (5,1)] ; [(1,5);(4,2);(5,7)] ; [] ; [(1,2);(3,5)] ; [(4,3)] |] ;;

dijkstra g ;;

(* Avec une file de priorité (tas) *)

(* Fonctions sur les tas, attention les tas sont constitués de couples (élément, priorité) et on utilise des tas-min *)

(* Fonction qui crée un tas initial à n éléments avec toutes les distances à max_int *)

let creer_tas n =
  let t = Array.make n (0,0) in
  for i = 1 to n-1 do
    t.(i) <- (i,max_int)
  done;
  t ;;

(* Fonction d'échange *)

let swap t i j pos =
  let x = t.(i) in
  t.(i) <- t.(j) ; t.(j) <- x ;
  pos.(fst t.(i)) <- i ; 			(* on met à jour le tableau pos, attention on manipule des couples (élément, priorité) *)
  pos.(fst t.(j)) <- j ;;

(* Fonction pour faire monter un élément *)
  
let monte t k pos =
  let rec aux i = match i with
    | i when i = 0 -> ()
    | i -> let j = i/2 in 			(* j est le père de i *)
	   if snd t.(i) < snd t.(j) then (swap t i j pos; aux j)
  in aux k ;;

(* Fonction pour faire descendre un élément dans le tas *)

let descend t n k pos = 			(* n est l'indice de fin du tas *)
  let rec aux i  = match i with
    | i when 2*i > n -> () 		(* cas où i n'a pas de fils *)
    | i -> let j = if 2*i = n || snd t.(2*i) < snd t.(2*i+1) then 2*i else 2*i+1 in 
	   if snd t.(i) > snd t.(j) then (swap t i j pos; aux j) (* j est le plus petit fils de i *)
  in aux k;;

(* Dijkstra avec un tas *)

let dijkstra_file g = 			(* on représente g par listes d'adjacence, et on cherche les distances au sommet 0 *)
  let n = Array.length g in
  let d = Array.make n max_int and file = creer_tas n in (* d contiendra les distances à 0 et file est la file de priorité *)
  d.(0) <- 0;  			(* au début, tout le monde est à max_int sauf 0 *)
  let pos = Array.make n 0 in	(* tableau indiquant les positions de chaque élément dans le tas *)
  for i = 1 to n-1 do
    pos.(i) <- i 			(* au début chaque élément i est à la position i *)
  done;
  let s = ref 0 in 		(* sommet courant que l'on considère *)
  let rec modif l  = match l with 	(* fonction qui parcourt la liste l des voisins de s et modifie le tableau d et la file en conséquence *)
    |[] -> ()
    |(i,x)::q  -> let p = pos.(i) in 	(* position de i dans le tas *)
    let nd =  min d.(i) (somme d.(!s) x) in 
      (d.(i) <- nd;
       file.(p) <- (i,nd);                       
       monte file p pos ; 		(* comme on diminue la priorité, l'élément ne peut que remonter *)
       modif q)
  in
  for k = n-1 downto 1 do
    modif g.(!s); (* on met à jour les voisins de s *)
    s := fst file.(1); 			(* nouvelle valeur de s *)			
    swap file 1 k pos; 		(* on met l'élément en tête à la fin et on reconstitue le tas *)
    descend file (k-1) 1 pos;
  done ;
  d ;;

dijkstra_file g ;;

(* Complexité : chaque montée/descente dans le tas coûte O(log(n)), on fait autant de remontées que le nombre p d'arêtes, et n descentes, donc le coût est en O((n+p)*log(n)) = O(nlog(n)) si le graphe est peu dense. *)
