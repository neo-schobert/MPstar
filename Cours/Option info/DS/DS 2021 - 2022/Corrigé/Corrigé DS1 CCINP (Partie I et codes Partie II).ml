(* DS1 Option Info MP/MP* : Sujet CCINP  *)

(* Partie I : Logique *)

(* 1. La formule F cherch�e est le OU des formules (Mi �quivalent � Di). *)

(* 2. DA = A ET C ET NON(B)
      DB = NON(C)
      DC = B OU (NON(A)). *)

(* 3. En �crivant la table de v�rit�, on trouve que le seul cas qui satisfait la formule F est : A et B mentent, C dit la v�rit�. *)

(* 4. DD = NON(F)
      DE = D ET F
      DF = E.  *)

(* 5. A l'aide des formules de De Morgan, on met la formule sous FND et on enl�ve les contradictions, on trouve qu'elle est �quivalente � (D ET NON(E) ET NON(F)). Donc D dit la v�rit� et E et F mentent. *)


(* Partie II : Algorithmique et programmation en Caml *)

(* Voir corrig� joint pour les questions 1 � 14 *)

(* 2. Arbres binaires de recherche  *)

type arbre =
  | Vide
  | Noeud of arbre * int * arbre ;;

(* 2.2 Tri d'une s�quence d'entiers *)

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

(* 3. Repr�sentation de syst�mes creux *)

(* 3.1. Arbres binaires partiels de r�els *)

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

(* On doit garder en m�moire le num�ro courant ainsi que la profondeur. On �crit une fonction auxiliaire parcours qui prend en entr�e 4 arguments : le couple mm=(min,max) courant, le num�ro n du noeud courant et pi = 2^p o� p est la profondeur *)

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

(* On cherche le chemin jusqu'� i en calculant sa d�composition binaire : selon la parit� de i on va � gauche ou � droite, puis on le divise par 2 jusqu'� atteindre 1 *)

let rec remplacer i e a =
  match a with
    | Vide -> Vide
    | Fourche(g,d) -> if i = 1 then Fourche(g,d) (* pas de valeur � l'indice i *)
      else if i mod 2 = 0 then Fourche(remplacer (i/2) e g, d) 
      else Fourche(g, remplacer (i/2) e d)		(* si i est pair on doit aller le chercher � gauche, sinon � droite *)
    | Noeud(g,v,d) -> if i = 1 then Noeud(g,e,d)	(* on a trouv� l'�l�ment d'indice i *)
      else if i mod 2 = 0 then Noeud(remplacer (i/2) e g, v ,d)
      else Noeud(g,v, remplacer (i/2) e g) ;;

(* 3.2. Repr�sentation d'un vecteur creux par un arbre partiel  *)

type vecteur = int * int * int * float * arbre;;

(* Question III.19 *)

(* Il suffit de v�rifier qu'il n'y a pas de fourche avec les deux fils vides dans l'arbre, que Vmin = 1, Vmax = taille de l'arbre et que Vt = taille de l'arbre *)

let valider (imin,imax,t,v,a) =
  let rec pasdefourche a =
    match a with
      | Vide -> true
      | Fourche(Vide,Vide) -> false
      | Fourche(g,d) -> (pasdefourche g) && (pasdefourche d)
      | Noeud(g,_,d) -> (pasdefourche g) && (pasdefourche d)
  in (pasdefourche a) && (imin = 1) && (imax = taille a) && (t = taille a) ;;

(* Question III.20 *)

(* On proc�de comme dans la question III.17 pour calculer le chemin jusqu'� i, et on exploite la structure de vecteur creux en testant d'abord si i est entre imin et imax *)

let lire i (imin,imax,t,v,a) =
  let rec chercher i a =
    match a with
      | Vide -> failwith "indice trop grand" (* on a un indice plus grand que la taille de l'arbre *)
      | Fourche(g,d) -> if i = 1 then v 	(* si on est sur une fourche on renvoie la valeur par d�faut *) 
	else if i mod 2 = 0 then chercher (i/2) g
	else chercher (i/2) d
      | Noeud(g,e,d) -> if i = 1 then e
	else if i mod 2 = 0 then chercher (i/2) g
	else chercher (i/2) d
  in 
  if i < imin || i > imax then v
  else chercher i a ;;

(* Question III.21 *)

(* Attention : on peut modifier la valeur de t (lui ajouter 1) si on remplace un �l�ment qui valait la valeur par d�faut (une fourche ou un vide dans l'arbre qui devient un noeud), et modifier aussi imin ou imax si i est hors de l'intervalle *)

let ecrire i e (imin,imax,t,v,a)=
  let rec modifie i e a = 		(* renvoie le nouvel arbre et un entier 0 ou 1 suivant s'il faut ajouter 1 � t ou non *)
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

(* L'�nonc� ne pr�cise pas les r�gles d'addition concernant notamment la valeur par d�faut, on suppose ici que les deux vecteurs ont la m�me taille, la m�me valeur par d�faut, que celle-ci est �l�ment neutre pour l'addition et qu'on ne peut pas tomber sur la valeur par d�faut en ajoutant deux valeurs qui ne sont pas �gales � elle *)

let somme (im1,iM1,t1,v1,a1) (im2,iM2,t2,v2,a2) =
  let rec ajoute a1 a2 = 		(* renvoie le couple (arbre, nb d'�lts diff�rents de la valeur par d�faut) *)
    match (a1,a2) with
      | (Vide, Vide) -> (Vide,0)
      | (a, Vide) -> failwith "tailles incompatibles"
      | (Vide, a) -> failwith "tailles incompatibles"
      | (Fourche(g1,d1), Fourche(g2,d2)) -> let (g,t1) = ajoute g1 g2 and (d,t2) = ajoute d1 d2 in (Fourche(g,d),t1+t2)
      | (Fourche(g1,d1), Noeud(g2,v,d2)) -> let (g,t1) = ajoute g1 g2 and (d,t2) = ajoute d1 d2 in (Noeud(g,v,d),t1+t2+1)
      | (Noeud(g1,v,d1), Fourche(g2,d2)) ->  let (g,t1) = ajoute g1 g2 and (d,t2) = ajoute d1 d2 in (Noeud(g,v,d),t1+t2+1)
      | (Noeud(g1,e1,d1), Noeud(g2,e2,d2)) ->  let (g,t1) = ajoute g1 g2 and (d,t2) = ajoute d1 d2 in (Noeud(g, e1+.e2, d),t1+t2)
  in if v1 <> v2 then failwith "pas la m�me valeur par d�faut"
  else let (a,t) = ajoute a1 a2 in
  (min im1 im2, max iM1 iM2, t, v1, a) ;;
  
