(* TP de révision avant les concours *)

(* Fonctions sur les listes *)

(* 1. *)

let rec longueur list =
  match list with
    |[] -> 0
    |x::q -> 1 + longueur q ;;

(* Complexité : O(n) *)

(* 2. *)

let rec appartient x list =
  match list with
    |[] -> false
    |y::q -> (x = y) || (appartient x q) ;;

(* 3. *)

let rec supprime x list =
  match list with
    |[] -> []
    |y::q -> if x = y then (supprime x q) else y::(supprime x q) ;;

(* 4.*)

let ajoute x list =
  if appartient x list then list else x::list ;;

(* 5. *)

let rec union l1 l2 =
  match l1 with
    |[] -> l2
    |x::q -> ajoute x (union q l2) ;;

(* Complexité : on appelle n1 fois la fonction ajoute qui a un coût linéaire en la longueur de la liste, or celle-ci augmente au fur et à mesure, au maximum elle est de taille n1+n2 donc coût en O(n1*(n1+n2)). *)

(* 6. *)

let rec fusion ll =
  match ll with
    |[] -> []
    |l::q -> union l (fusion q) ;;

(* Complexité : si on note n_i la longueur de la ième liste, lors de l'étape où doit faire l'union de la ième liste avec la fusion des suivantes, le coût de cette étape sera n_i*(n_i+n_{i+1}+n_{i+2}+...) et en ajoutant tout on obtient la somme de tous les produits n_i * n_j pour i <= j (essayer avec 3 ou 4 listes pour s'en rendre compte). On peut aussi dire que c'est un O(n**2) où n est la somme des n_i.*)
(* 7. *)

let rec produit l1 l2 =
  let rec dis x l2 = 		(* fonction auxiliaire qui distribue un élément *)
    match l2 with 
      |[] -> []
      |y::q -> (x,y)::(dis x q)
  in match l1 with
    |[] -> []
    |x::q -> (dis x l2)@(produit q l2) ;;

(* Tri par insertion *)

(* 8. *)

let rec insertion x l =
  match l with
    |[] -> [x]
    |y::q -> if x<=y then x::y::q else y::(insertion x q) ;;

(* 9. *)

let rec tri_insertion l =
  match l with
    |[] -> []
    |x::q -> insertion x (tri_insertion q) ;;

(* 10. *)

(* Complexité dans le meilleur des cas : O(n) (si la liste est déjà triée dans l'ordre croissant) *)

(* Complexité dans le pire des cas : O(n^2) (si la liste est triée dans l'ordre décroissant) *)

(* Remarque : si on écrit la fonction sous forme récursive terminale avec un accumulateur et qu'on insère les éléments un à un dans l'accumulateur, le meilleur et le pire cas sont inversés *)

(* 11. *)

let rec mem1 x l =
  match l with
    |[] -> false
    |(y,z)::q -> (y = x) || (mem1 x q) ;;

(* 12. *)

let rec assoc x l =
  match l with
    |[] -> failwith "x non présent"
    |(z,y)::q -> if x = z then y else assoc x q ;;

(* 13. *)

let plus_petit_absent l =
  let i = ref 0 in
  while appartient (!i) l do
    incr(i)
  done;
  !i ;;

(* 14. *)

let rec aplatir ll =
  match ll with
    |[] -> []
    |(x,l)::q -> (x::l)@(aplatir q) ;;

(* 15. Tri fusion *)

(* Fonction qui sépare en deux *)

let rec coupe_en_deux l =
  match l with
    |[] -> ([],[])
    |[a] -> ([a],[])
    |a::b::q -> let (l1,l2) = coupe_en_deux q in (a::l1, b::l2) ;;

(* Fonction de fusion de deux listes triées : attention on trie selon la 2nde variable dans l'ordre décroissant *)

let rec fusion l1 l2 =
  match (l1,l2) with
    |(l1,[]) -> l1
    |([],l2) -> l2
    |((x1,y1)::q1, (x2,y2)::q2) when y1 >= y2 -> (x1,y1)::(fusion q1 l2)
    |((x1,y1)::q1, (x2,y2)::q2) -> (x2,y2)::(fusion l1 q2) ;;

(* Fonction de tri fusion *)

let rec tri_fusion l =
  match l with
    |[] -> []
    |[x] -> [x]
    |l -> let (l1,l2) = coupe_en_deux l in fusion (tri_fusion l1) (tri_fusion l2) ;;

(* 16. *)

let rec circulaire l =
  match l with
    |[] -> []
    |[x] -> [x]
    |x::q -> let q2 = circulaire q in (List.hd q2)::x::(List.tl q2) ;;

(* Fonctions sur les tableaux/vecteurs *)

(* 1. *)

let dichotomie a t =
  let deb = ref 0 and fin = ref (Array.length t - 1) in
  while !fin - !deb > 1 do
    let mil = (!fin + !deb)/2 in
    if t.(mil) > a then
      fin := mil
    else 
      deb := mil
  done ;
  !deb ;;

(* Tri rapide *)

(* 2. *)

let echange v i j =
  let a = v.(i) in
  v.(i) <- v.(j);
  v.(j) <- a ;; 

(* 3. *)

let separation v i1 i2 =
  let p = v.(i1) and c = ref i1 in 	(* c sera la place du pivot *)
  for j = i1+1 to i2 do
    if v.(j) <= p then 			(* si on a un élément plus petit que le pivot, on incrémente c et on échange v.(j) avec le premier des éléments plus grands que p *)
      ( incr(c);
	echange v j (!c) )
  done;
  echange v i1 (!c) ;			(* on met le pivot à sa place *)
  !c ;;
  
(* 4. *)

let tri_rapide v =
  let rec aux v i j =
    if j-i > 0 then 
      let p = separation v i j in 
	 aux v i (p-1) ;
	 aux v (p+1) j 
  in aux v 0 (Array.length v -1) ;;

(* 5. Quand le tableau est déjà trié (par ordre croissant ou décroissant), la complexité est quadratique car la taille des sous-listes ne diminue que de 1 à chaque appel récursif *)

(* 6. et 7. Si n = 2**k et que la séparation est équilibrée, on a la relation de récurrence c(n) = 2*c(n/2) + n qui a pour solution c(n) = O(nlog(n)). Si n n'est plus une puissance de 2, on l'encadre par 2 puissances de 2 successives et on utilise que la complexité est croissante. *)

(* Fonctions sur les arbres binaires : cf TP sur les ABR  *)

(* Fonctions sur les automates *)

type automate = { n: int ; i : int ; t : int list ; gamma : int -> char ->
int } ;;

(* 1. *)

let delta_etoile aut i m =
  let n = String.length m and etat = ref i in
  for j = 0 to n-1 do
    etat := aut.gamma (!etat) m.[i]
  done;
  !etat ;;
    
(* 2. *)

let est_acceptant aut m =
  let i = aut.i and t = aut. t in
  let etat = delta_etoile aut i m in
  appartient etat t ;;

(* Fonctions sur les graphes *)

(* 1. *)

let rec est_clique graphe list =
  let rec aux l1 l2 = 			(* renvoie true ssi tous les éléments de l1 sont présents dans l2 *)
    match l1 with
      |[] -> true
      |x::q -> (appartient x l2) && (aux q l2)
  in match list with
    |[] -> true
    |x::q -> (aux list graphe.(x)) && (est_clique graphe q) ;;

(* 2. *)

let voisins_inferieurs graphe x =
  let rec aux l x = 			(* renvoie la liste des éléments de l qui sont strictement inférieurs à x *)
    match l with
      |[] -> []
      |y::q -> if y < x then y::(aux q x) else aux q x
  in aux graphe.(x) x ;;

(* Fonctions sur les chaînes de caractères *)

(* 1. *)

let est_present m t s =
  let i = ref 0 in
  while !i < String.length m &&  m.[!i] = t.[!i + s] do
    i := !i + 1
  done;
  !i = String.length m ;;

(* Complexité : Dans le pire des cas, on lit tout le mot m (il est présent) : complexité O(len(m)). Dans le meilleur des cas, on s'arrête dès la 1ère lettre : O(1). *)

(* 2.*)

let recherche_naive m t =
  let l = ref [] and lm = String.length m and lt = String.length t in
  for s = 0 to lt-lm do
    if est_present m t s then
      l := s::(!l)
  done;
  !l ;;

(* 3. Dans le pire des cas, toutes les positions sont dans la liste, il y a (lt-lm+1) tours de boucle for et chacune a complexité O(lm) donc complexité finale en O(lm*(lt-lm+1)). Par exemple si m et t ne sont composés que de a, cette complexité est atteinte. *)

(* Matrices booléennes  *)

let mult a b =
  let n = Array.length a in
  let c = Array.make_matrix n n true in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      let s = ref false in
      for k = 0 to n-1 do
	s := !s || (a.(i).(k) && b.(k).(j))
      done;
      c.(i).(j) <- !s;
    done;
  done;
  c ;;
