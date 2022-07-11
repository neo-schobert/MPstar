(* DS1 Option Info MP/MP* : Sujet CCINP  *)

(* Partie I : Logique *)

(* 1. La formule F cherchée est le OU des formules (Mi équivalent à Di). *)

(* 2. DA = A ET C ET NON(B)
      DB = NON(C)
      DC = B OU (NON(A)). *)

(* 3. En écrivant la table de vérité, on trouve que le seul cas qui satisfait la formule F est : A et B mentent, C dit la vérité. *)

(* 4. DD = NON(F)
      DE = D ET F
      DF = E.  *)

(* 5. A l'aide des formules de De Morgan, on met la formule sous FND et on enlève les contradictions, on trouve qu'elle est équivalente à (D ET NON(E) ET NON(F)). Donc D dit la vérité et E et F mentent. *)


(* Partie II : Algorithmique et programmation en Caml *)

(* Voir corrigé joint pour les questions 1 à 14 *)

(* 2. Arbres binaires de recherche  *)

type arbre =
  | Vide
  | Noeud of arbre * int * arbre ;;

(* 2.2 Tri d'une séquence d'entiers *)

type sequence = int list ;;

let rec ajouter v a =
  match a with
    | Vide -> Noeud(Vide,v,Vide)
    | Noeud (g,e,d) ->
      if (v = e)
      then Noeud(Noeud(g,e,Vide),v,d)
      else
	(if (v < e)
	 then Noeud((ajouter v g),e,d)
	 else Noeud(g,e,(ajouter v d)));;

let trier s =
  let rec aux1 l r =
    match l with
      | [] -> r
      | t::q -> (aux1 q (ajouter t r)) in
  let rec aux2 a =
    match a with
      | Vide -> []
      | Noeud(g,v,d) -> (aux2 g) @ (v :: (aux2 d)) in
  (aux2 (aux1 s Vide));;

let exemple = [2 ; 1 ; 3] ;;

(* 3. Représentation de systèmes creux *)

(* 3.1. Arbres binaires partiels de réels *)

type arbre =
  | Vide
  | Fourche of arbre * arbre
  | Noeud of arbre * float * arbre;;

(* Question III.15 *)

let rec taille a =
  match a with
    | Vide -> 0
    | Fourche(g,d) -> taille g + taille d
    | Noeud(g,v,d) -> 1 + taille g + taille d ;;

(* Question III.16 *)

type paire = int*int ;;

(* On doit garder en mémoire le numéro courant ainsi que la profondeur. On écrit une fonction auxiliaire parcours qui prend en entrée 4 arguments : le couple mm=(min,max) courant, le numéro n du noeud courant et pi = 2^p où p est la profondeur *)

let bornes a =
  let minimax (a,b) (c,d) = (min a c, max b d)
  in
  let rec parcours mm n pi = function
    | Vide -> mm
    | Fourche(g,d) -> parcours (parcours mm (n+pi) (2*pi) g) (n+2*pi) (2*pi) d
    | Noeud(g,_,d) -> parcours (parcours (minimax mm (n,n)) (n+pi) (2*pi) g)
(n+2*pi) (2*pi) d
  in
  parcours (max_int,0) 1 1 a ;;

(* Question III.17 *)

(* On cherche le chemin jusqu'à i en calculant sa décomposition binaire : selon la parité de i on va à gauche ou à droite, puis on le divise par 2 jusqu'à atteindre 1 *)

let rec remplacer i e a =
  match a with
    | Vide -> Vide
    | Fourche(g,d) -> if i = 1 then Fourche(g,d) (* pas de valeur à l'indice i *)
      else if i mod 2 = 0 then Fourche(remplacer (i/2) e g, d) 
      else Fourche(g, remplacer (i/2) e d)		(* si i est pair on doit aller le chercher à gauche, sinon à droite *)
    | Noeud(g,v,d) -> if i = 1 then Noeud(g,e,d)	(* on a trouvé l'élément d'indice i *)
      else if i mod 2 = 0 then Noeud(remplacer (i/2) e g, v ,d)
      else Noeud(g,v, remplacer (i/2) e g) ;;

(* 3.2. Représentation d'un vecteur creux par un arbre partiel  *)

type vecteur = int * int * int * float * arbre;;

(* Question III.19 *)

(* Il suffit de vérifier qu'il n'y a pas de fourche avec les deux fils vides dans l'arbre, que Vmin = 1, Vmax = taille de l'arbre et que Vt = taille de l'arbre *)

let valider (imin,imax,t,v,a) =
  let rec pasdefourche a =
    match a with
      | Vide -> true
      | Fourche(Vide,Vide) -> false
      | Fourche(g,d) -> (pasdefourche g) && (pasdefourche d)
      | Noeud(g,_,d) -> (pasdefourche g) && (pasdefourche d)
  in (pasdefourche a) && (imin = 1) && (imax = taille a) && (t = taille a) ;;

(* Question III.20 *)

(* On procède comme dans la question III.17 pour calculer le chemin jusqu'à i, et on exploite la structure de vecteur creux en testant d'abord si i est entre imin et imax *)

let lire i (imin,imax,t,v,a) =
  let rec chercher i a =
    match a with
      | Vide -> failwith "indice trop grand" (* on a un indice plus grand que la taille de l'arbre *)
      | Fourche(g,d) -> if i = 1 then v 	(* si on est sur une fourche on renvoie la valeur par défaut *) 
	else if i mod 2 = 0 then chercher (i/2) g
	else chercher (i/2) d
      | Noeud(g,e,d) -> if i = 1 then e
	else if i mod 2 = 0 then chercher (i/2) g
	else chercher (i/2) d
  in 
  if i < imin || i > imax then v
  else chercher i a ;;

(* Question III.21 *)

(* Attention : on peut modifier la valeur de t (lui ajouter 1) si on remplace un élément qui valait la valeur par défaut (une fourche ou un vide dans l'arbre qui devient un noeud), et modifier aussi imin ou imax si i est hors de l'intervalle *)

let ecrire i e (imin,imax,t,v,a)=
  let rec modifie i e a = 		(* renvoie le nouvel arbre et un entier 0 ou 1 suivant s'il faut ajouter 1 à t ou non *)
    match a with
      | Vide -> if i = 1 then (Noeud(Vide,e,Vide),1) else failwith "indice trop grand"
      | Fourche(g,d) -> if i = 1 then (Noeud(g,e,d),1)
	else if i mod 2 = 0 then let (g',b) = modifie (i/2) e g in (Fourche(g',d),b)
	else let (d',b) = modifie (i/2) e d in (Fourche(g,d'),b)
      | Noeud(g,w,d) -> if i = 1 then (Noeud(g,e,d),0)
        else if i mod 2 = 0 then let (g',b) = modifie (i/2) e g in (Noeud(g',w,d),b)
	else let (d',b) = modifie (i/2) e d in (Noeud(g,w,d'),b)
  in let (a',b) = modifie i e a in 
  (min i imin,max i imax,t+b,v,a') ;;

(* Question III.22 *)

(* L'énoncé ne précise pas les règles d'addition concernant notamment la valeur par défaut, on suppose ici que les deux vecteurs ont la même taille, la même valeur par défaut, que celle-ci est élément neutre pour l'addition et qu'on ne peut pas tomber sur la valeur par défaut en ajoutant deux valeurs qui ne sont pas égales à elle *)

let somme (im1,iM1,t1,v1,a1) (im2,iM2,t2,v2,a2) =
  let rec ajoute a1 a2 = 		(* renvoie le couple (arbre, nb d'élts différents de la valeur par défaut) *)
    match (a1,a2) with
      | (Vide, Vide) -> (Vide,0)
      | (a, Vide) -> failwith "tailles incompatibles"
      | (Vide, a) -> failwith "tailles incompatibles"
      | (Fourche(g1,d1), Fourche(g2,d2)) -> let (g,t1) = ajoute g1 g2 and (d,t2) = ajoute d1 d2 in (Fourche(g,d),t1+t2)
      | (Fourche(g1,d1), Noeud(g2,v,d2)) -> let (g,t1) = ajoute g1 g2 and (d,t2) = ajoute d1 d2 in (Noeud(g,v,d),t1+t2+1)
      | (Noeud(g1,v,d1), Fourche(g2,d2)) ->  let (g,t1) = ajoute g1 g2 and (d,t2) = ajoute d1 d2 in (Noeud(g,v,d),t1+t2+1)
      | (Noeud(g1,e1,d1), Noeud(g2,e2,d2)) ->  let (g,t1) = ajoute g1 g2 and (d,t2) = ajoute d1 d2 in (Noeud(g, e1+.e2, d),t1+t2)
  in if v1 <> v2 then failwith "pas la même valeur par défaut"
  else let (a,t) = ajoute a1 a2 in
  (min im1 im2, max iM1 iM2, t, v1, a) ;;
  
