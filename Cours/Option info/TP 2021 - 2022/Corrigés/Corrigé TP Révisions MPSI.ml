(* TP de révisions de 1A *)


(* Listes et récursivité *)

(* Fonction d'accès au nième élément d'une liste *)

let rec nieme n l = 			(* le 1er élément a pour indice 0 *)
  match l with
    |[] -> failwith "non trouvé"
    |a::q when n = 0 -> a
    |a::q -> nieme (n-1) q ;;

(* Fonction renvoyant la longueur d'une liste *)

let rec long l = 
  match l with
    |[] -> 0
    |a::q -> 1 + long q ;;

(* Fonction qui teste l'appartenance à une liste *)

let rec appartient x l =
  match l with
    |[] -> false
    |a::q when a = x -> true
    |a::q -> appartient x q ;;

(* Fonction qui concatène deux listes *)

let rec concat l1 l2 =
  match l1 with
    |[] -> l2
    |a::q -> a::(concat q l2) ;;

(* Fonction qui aplatit une liste de listes *)

let rec aplatit l =
  match l with
    |[] -> []
    |a::q -> concat a (aplatit q) ;;

(* Fonction qui applique une fonction à chaque élément d'une liste *)

let rec applique f l =
  match l with
    |[] -> []
    |a::q -> f(a)::(applique f q) ;;


(* Vecteurs et programmation impérative *)

(* Suite de Fibonacci avec un tableau *)

let fibo n =
  let tab = Array.make (n+1) 1 in
  for i = 2 to n do
    tab.(i) <- tab.(i-1) + tab.(i-2)
  done;
  tab.(n) ;;

(* Suite de Fibonacci avec des références *)

let fib n =
  let a = ref 1 and b = ref 1  in
  for i = 2 to n do
    let c = !a in				(* on est obligé de prendre une variable intermédiaire *)
    a := !b;
    b := c + !b ; 
  done;
  !b ;;

(* Fonction d'appartenance à un vecteur *)

let appartient x a =
  let n = Array.length a and b = ref false and i = ref 0 in
  while (not !b) && (!i < n) do
    if a.(!i) = x then
      b := true;
    incr(i) ;
  done;
  !b ;;

(* Si a est trié : recherche dichotomique *)

let dicho x a =
  let n = Array.length a and b = ref false  in
  let g = ref 0 and d = ref (n-1) in 	(* indices de début et de fin *)
  while (not !b) && g <= d do
    let m = ((!g)+(!d))/2 in
    if a.(m) = x then 
      b := true
    else if a.(m) < x then
      g := m+1
    else
      d := m-1
  done ;
  !b ;;

(* Version récursive *)

let dichorec x a =
  let rec aux g d =
    if g > d then false
    else let m = (g+d)/2 in
	 if a.(m) = x then true
	 else if a.(m) < x then aux (m+1) d
	 else aux g (m-1)
  in aux 0 (Array.length a -1) ;;


(* Concaténation *)

let concatene a1 a2 =
  let n1 = Array.length a1 and n2 = Array.length a2 in
  let a = Array.make (n1+n2) a1.(0) in
  for i = 1 to n1-1 do
    a.(i) <- a1.(i)
  done;
  for i = 0 to n2-1 do
    a.(n1+i) <- a2.(i)
  done;
  a ;;

(* Version matrices *)

let concat m1 m2 =
  let n1 = Array.length m1 and n2 = Array.length m2 in
  if n1 <> n2 then 
    failwith "tailles incompatibles" 
  else 
    let m = Array.make n1 [| |] in
    for i = 0 to n1-1 do
      m.(i) <- concatene m1.(i) m2.(i)
    done;
  m ;;

(* Fonction map_array *)

let map_array f a =
  let n = Array.length a in
  let b = Array.make n (f(a.(0))) in
  for i = 0 to n-1 do
    b.(i) <- f(a.(i))
  done;
  b ;;


(* Structures de données et types *)

type 'a arbre = Nil | Noeud of 'a arbre * 'a * 'a arbre ;;

(* Hauteur d'un arbre binaire *)

let rec hauteur a = 
  match a with
    |Nil -> -1
    |Noeud(fg, x, fd) -> 1 + max (hauteur fg) (hauteur fd) ;;

(* Nombre total de noeuds d'un arbre binaire *)

let rec nb_noeuds a =
  match a with
    |Nil -> 0
    |Noeud(fg, x, fd) -> 1 + nb_noeuds fg + nb_noeuds fd ;;

(* Nombre de feuilles d'un arbre binaire *)

let rec nb_feuilles a =
  match a with
    |Nil -> 0
    |Noeud(Nil, x, Nil) -> 1
    |Noeud(fg, x, fd) -> nb_feuilles fg + nb_feuilles fd ;;

(* Nombre de noeuds internes d'un arbre binaire *)

(* Avec les fonctions précédentes *)

let nb_ninternes1 a = nb_noeuds a - nb_feuilles a ;; 

(* Sans les fonctions précédentes *)

let rec nb_ninternes2 a =
  match a with
    |Nil -> 0
    |Noeud(Nil,x,Nil) -> 0
    |Noeud(fg,x,fd) -> 1 + nb_ninternes2 fg + nb_ninternes2 fd ;;

(* Ajout d'un élément *)

let rec ajoute e a =
  match a with
    |Nil -> Noeud(Nil,e,Nil)
    |Noeud(fg,x,fd) -> Noeud((ajoute e fg), x, fd) ;;


(* Diviser pour régner et programmation dynamique *)

(* Tri fusion *)

(* Fonction qui coupe en deux une liste *)

let rec coupe_en_deux l =
  match l with
    |[] -> ([],[])
    |[a] -> ([a],[])
    |a::b::q -> let (l1,l2) = coupe_en_deux q in (a::l1, b::l2) ;;

(* Fonction de fusion *)

let rec fusion l1 l2 = 			(* on suppose l1 et l2 triées *)
  match (l1,l2) with
    |([],l2) -> l2
    |(l1,[]) -> l1
    |(a1::q1, a2::q2) when a1 <= a2 -> a1::(fusion q1 l2)
    |(a1::q1, a2::q2) -> a2::(fusion l1 q2) ;;

(* Fonction qui réalise le tri fusion *)

let rec tri_fusion l =
  match l with
    |[] -> []
    |[a] -> [a]
    |l -> let (l1,l2) = coupe_en_deux l in
	  fusion (tri_fusion l1) (tri_fusion l2) ;;


(* Sac à dos *)

(* On représente les valeurs et les volumes des objets dans des tableaux c et v *)

let sacados c v wmax =
  let n = Array.length c in
  let f = Array.make_matrix (n+1) (wmax+1) 0 in
  for i = 1 to n do
    for w = 0 to wmax do
	if v.(i-1) > w then
	  f.(i).(w) <- f.(i-1).(w)
        else
	  f.(i).(w) <- max (c.(i-1)+f.(i-1).(w-v.(i-1))) (f.(i-1).(w))
    done;
  done;
  f.(n).(wmax) ;;



