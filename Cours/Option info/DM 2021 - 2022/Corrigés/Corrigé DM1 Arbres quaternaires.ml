(* DM1 Option Info MP/MP* 2020-2021 *)

(* Programmation : Représentation d'images par des arbres quaternaires (CCP 2014) *)

type quater = 
  |Division of int*int*int*quater*quater*quater*quater
  |Bloc of int*int*int*int ;;
type position = SO|SE|NO|NE ;;

(* Exemples pour les tests *)

let b11_2 = Bloc(1,1,2,0);;
let b31_1 = Bloc(3,1,1,0);;
let b41_1 = Bloc(4,1,1,2);;
let b32_1 = Bloc(3,2,1,1);;
let b42_1 = Bloc(4,2,1,3);;
let d31_2 = Division(3,1,2,b31_1,b41_1,b32_1,b42_1) ;;
let b13_2 = Bloc(1,3,2,1);;
let b33_2 = Bloc(3,3,2,2);;
let a = Division(1,1,4,b11_2,d31_2,b13_2,b33_2) ;;

(* Question 7 *)

let scinder a =
  match a with
    |Bloc(x,y,t,c) -> Division(x,y,t, Bloc(x,y,t/2,c), Bloc(x+t/2,y,t/2,c),
			       Bloc(x,y+t/2,t/2,c), Bloc(x+t/2,y+t/2,t/2,c))
    |a -> a ;;

scinder a ;;
scinder b11_2 ;;

(* Question 8 : attention au cas où on fusionne quatre blocs de même couleur ! *)

let fusionner so se no ne =
  match (so,se,no,ne) with
    |(Bloc(x,y,t,c0), Bloc(_,_,_,c1), Bloc(_,_,_,c2), Bloc(_,_,_,c3)) when c0=c1 && c2=c3 && c0=c2 -> Bloc(x,y,2*t,c1)
    |(Bloc(x,y,t,c),_,_,_) -> Division(x,y,2*t,so,se,no,ne)
    |(Division(x,y,t,_,_,_,_),_,_,_) -> Division(x,y,2*t,so,se,no,ne) ;;

a = fusionner b11_2 d31_2 b13_2 b33_2 ;;

(* Question 9 *)

let rec profondeur a =
  match a with
    |Bloc(x,y,t,c) -> 0
    |Division(x,y,t,so,se,no,ne) -> 1 + max (max (profondeur so) (profondeur se))
                                            (max (profondeur no) (profondeur ne)) ;;

profondeur a ;;

(* Question 10 : on fait une "recherche dichotomique en 2D" *)

let rec consulter x y a =
  match a with
    |Bloc(xp,yp,t,c) -> c
    |Division(xp,yp,t,so,se,no,ne) ->
      if x<xp+t/2 && y<yp+t/2 then consulter x y so
      else if x<xp+t/2 then consulter x y no
      else if y<yp+t/2 then consulter x y se
      else consulter x y ne ;;

consulter 1 4 a ;;
consulter 3 2 a ;;
consulter 4 2 a ;;
consulter 2 1 a ;;
consulter 4 4 a ;;

(* Question 11 : on fait appel à scinder et fusionner pour pouvoir renvoyer un arbre valide *)

let rec peindre x y c a =
  match a with
    |Bloc(xp,yp,1,cp) when x = xp && y = yp -> Bloc(x,y,1,c) (* cas de base : bloc de taille 1 avec les bonnes coordonnées *)
    |Bloc(xp,yp,t,cp) when c = cp -> a			     (* on ne scinde pas un bloc si on veut le colorier avec la même couleur *)
    |Bloc(xp,yp,t,cp) -> peindre x y c (scinder a)
    |Division(xp,yp,t,so,se,no,ne) ->
      if x<xp+t/2 && y<yp+t/2 then fusionner (peindre x y c so) se no ne (* attention, on doit faire appel à fusionner au cas où on se retrouve avec 4 blocs de même couleur *)
      else if x<xp+t/2 then fusionner so se (peindre x y c no) ne 
      else if y<yp+t/2 then fusionner so (peindre x y c se) no ne
      else fusionner so se no (peindre x y c ne)  ;; 

let a2 = peindre 2 4 0 a ;;
a = peindre 2 4 1 a2 ;;

(* Question 12 : pour plus de lisibilité on écrit des fonctions auxiliaires qui renvoient la taille, l'abscisse et l'ordonnée *)

let taille a = match a with
  |Bloc(x,y,t,c) -> t
  |Division(x,y,t,so,se,no,ne) -> t;;

let abs a = match a with
  |Bloc(x,y,t,c) -> x
  |Division(x,y,t,so,se,no,ne) -> x;;

let ord a = match a with
  |Bloc(x,y,t,c) -> y
  |Division(x,y,t,so,se,no,ne) -> y;;

let rec valider a =
  match a with
    |Bloc(x,y,t,c) -> (x>0) && (y>0) && (t>0)
    |Division(_,_,_, Bloc(_,_,_,c0), Bloc(_,_,_,c1), Bloc(_,_,_,c2), Bloc(_,_,_,c3)) when c0=c1 && c2=c3 && c0=c2 -> false
    |Division(x,y,t,so,se,no,ne) -> (x>0) && (y>0) && (t>0) 
                                    && (taille so = t/2) && (taille se = t/2)
                                    && (taille no = t/2) && (taille ne = t/2)
                                    && (abs so = x) && (ord so = y)
                                    && (abs se = x+t/2) && (ord se = y)
                                    && (abs no = x) && (ord no = y+t/2)
                                    && (abs ne = x+t/2) && (ord ne = y+t/2)
                                    && valider so && valider se && valider no && valider se ;;
                             
valider a ;;
valider (scinder b11_2) ;;

(* Question 13 *)

(* On utilise une fonction auxiliaire récursive aux qui prend en entrée l'arbre courant a, la liste courante l et un compteur n, et renvoie le couple (nouvelle liste, nouveau compteur) après avoir parcouru tous les fils si c'est une division, et si c'est un bloc de taille plus grande que 1 on le scinde avant de le parcourir. Attention les références sont interdites ! On parcourt les entiers par ordre décroissant pour les ajouter en tête de liste à chaque fois. On réutilise aussi la fonction taille de la question précédente. *)

let sauvegarder a =
  let rec aux a l n =
    match a with
      |Bloc(x,y,1,c) -> ((n,c)::l, n-1)
      |Bloc(x,y,t,c) -> aux (scinder a) l n
      |Division(x,y,t,so,se,no,ne) -> let (l1,n1) = aux ne l n in
				      let (l2,n2) = aux no l1 n1 in
				      let (l3,n3) = aux se l2 n2 in
				      aux so l3 n3
  in let t = taille a in let (l,n) =  aux a [] (t*t) in l ;;

let l = sauvegarder a ;;

(* Question 14 *)

(* On utilise une fonction auxiliaire aux qui prend en entrée la liste courante l, la taille t de l'image courante à reconstituer, et les coordonnées courantes x et y, et qui renvoie l'arbre reconstitué ainsi que la liste restant à parcourir. On commence par créer les 4 fils de taille moitié avant de les fusionner. On écrit aussi une fonction int_sqrt qui renvoie la racine carrée d'un entier de la forme 2^(2*n). *)

(* Cette version parcourt deux fois la liste à cause de l'appel à List.length mais cela ne change pas la complexité car on ne rappelle pas List.length à chaque appel récursif. *)

let rec int_sqrt n =
  match n with
    |1 -> 1
    |n -> 2 * int_sqrt(n/4) ;;

let restaurer l =
  let rec aux l t x y =
    match (l,t) with
      |((n,c)::q,1) -> (Bloc(x,y,1,c),q)
      |(l,t) -> let (so,q1) = aux l (t/2) x y  in
		let (se,q2) = aux q1 (t/2) (x+t/2) y  in
		let (no,q3) = aux q2 (t/2) x (y+t/2)  in
		let (ne,q4) = aux q3 (t/2) (x+t/2) (y+t/2)  in
		(fusionner so se no ne, q4)
  in let t = List.length l in let (a,l1) = aux l (int_sqrt t) 1 1 in a ;;
      
restaurer l = a ;;
