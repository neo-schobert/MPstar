(* DS1 MP/MP* 2021/2022 Sujet Mines-Ponts *)

type noeud = {lettre : char; poids : int; fg : int; fd : int};;

let noeud_vide =
{lettre = '\000'; poids = 0; fg = -1; fd = -1};;

type foret = {
  mutable nb_arbres : int;
  mutable nb_noeuds : int;
  table : noeud array };;

let max = 100;;

let f_ex =
let table_F_ex = Array.make max noeud_vide in
table_F_ex.(0) <- {lettre = 'g'; poids = 10; fg = -1; fd = 6};
table_F_ex.(1) <- {lettre = 'a'; poids = 20; fg = 4; fd = 3};
table_F_ex.(2) <- {lettre = 'b'; poids = 13; fg = -1; fd = -1};
table_F_ex.(3) <- {lettre = 'e'; poids = 9; fg = 5; fd = -1};
table_F_ex.(4) <- {lettre = 'f'; poids = 15; fg = -1; fd = -1};
table_F_ex.(5) <- {lettre = 'c'; poids = 12; fg = -1; fd = -1};
table_F_ex.(6) <- {lettre = 'd'; poids = 8; fg = -1; fd = -1};
{nb_arbres = 3; nb_noeuds = 7; table = table_F_ex};;

(* Première partie : fonctions de base pour l’algorithme de Huffman  *)

(* 11. *)

let indice_du_min f k =
  let tab = f.table in
  let imin = ref 0 in
  for i = 1 to k-1 do
    if tab.(i).poids < tab.(!imin).poids then
      imin := i
  done;
  !imin ;;

(* 12. *)

(* On écrit une fonction auxiliaire qui échange deux éléments d'un tableau *)

let echange tab i j =
  let aux = tab.(i) in 			(* on a besoin d'une variable intermédiaire pour faire l'échange *)
  tab.(i) <- tab.(j) ;
  tab.(j) <- aux ;;

let deux_plus_petits f =
  let tab = f.table and n = f.nb_arbres in
  let i = indice_du_min f n in
  echange tab i (n-1) ;
  let i2 = indice_du_min f (n-1) in
  echange tab i2 (n-2) ;; 		(* ne renvoie rien) *)

(* 13. On place le nouveau noeud en position n_arbres-2 et son fils droit (anciennement en position n_arbres-2) en position nb_noeuds *)

let assemblage f =
  deux_plus_petits f ;
  let na = f.nb_arbres and nn = f.nb_noeuds and tab = f.table in
  let nouv_noeud = {lettre = '\000'; poids = tab.(na-1).poids + tab.(na-2).poids; fg = na-1; fd = nn} in
  tab.(nn) <- tab.(na-2) ;
  tab.(na-2) <- nouv_noeud ;
  f.nb_arbres <- na - 1; 		(* on n'oublie pas de mettre à jour nb_arbres et nb_noeuds *)
  f.nb_noeuds <- nn + 1;; 		(* ne renvoie rien *)
  

(* Voir corrigé joint pour les autres questions *)
