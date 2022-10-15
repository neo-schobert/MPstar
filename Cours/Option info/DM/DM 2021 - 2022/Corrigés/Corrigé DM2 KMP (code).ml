(* DM2 Option Info 2021-2022 : Mines/Ponts 2018 *)

(* Attention, plusieurs pi�ges ici : 

- on repr�sente les mots par des listes d'entiers et non des cha�nes de caract�res
- on num�rote les lettres � partir de 1
- on m�morise une occurrence d'un motif par l'indice du dernier caract�re *)

(* 1. Recherche na�ve d'un mot *)

(* 1. Oui c'est possible : si s = aaa et t = aaaa, s est de longueur k = 3 et y = 3 et y' = 4 sont deux occurrences de s dans t, avec y<y' et  y'-k+1 = 2 < y.  *)

(* 2. La premi�re occurrence de s dans t ne peut pas arriver avant le k-i�me caract�re, et on peut en avoir jusqu'au n-i�me, donc il y a au maximum n-k+1 occurrences de s dans t.
Cette borne peut �tre atteinte, si on reprend l'exemple pr�c�dent : s = aaa et t = aaaa, k = 3 et n = 4, et il y a n-k+1 = 2 occurrences de s dans t.  *)

(* 3. *)

let rec longueur l =
  match l with
    |[] -> 0
    |t::q -> 1 + longueur(q) ;;

(* Complexit� lin�aire en la longueur n de l car on parcourt la liste une seule fois. *)

(* 4. *)

let rec prefixe s t = 
  match (s,t) with
    |([],_) -> true
    |(_,[]) -> false
    |(a::q, b::r) -> (a = b) && (prefixe q r) ;;

(* Complexit� en O(min(k,n)), on s'arr�te d�s que l'une des deux listes est vide. *)

(* 5. *)

let recherche_naive s t =
  let k = longueur s in
  let rec parcours i t =
    match t with
      |[] -> []
      |a::q -> if (prefixe s t) then (i+k-1)::(parcours (i+1) q)
        else parcours (i+1) q 
  in parcours 1 t ;;

let s = [0;1;2] ;;
let t = [0;1;2;0;1;2;3;0;1;0;1;2;3;0;1;2;3;0;1;3;4] ;;
recherche_naive s t ;;

(* 6. On appelle une fois la fonction longueur sur s : O(k), puis on effectue au plus n fois la foncton parcours qui appelle prefixe, donc co�t total en O(n*min(n,k)). *)


(* 2. Automates finis d�terministes � repli *)

type afdr = {
final : bool array;
transition : int array array;
repli: int array;
};;

let a1 = {final = [|false;false;false;true|] ; transition = [|[|1;0|] ; [|-1;2|] ; [|3;-1|] ; [|-1;-1|]|] ; repli = [|0;0;0;1|]} ;;


(* 7. La suite des rho^j(q) est une suite d'entiers strictement d�croissante donc va atteindre 0 � un moment, or la fonction de transition est toujours d�finie � partir de l'�tat initia, donc il existe j tel que delta(rho^j(q),alpha) est toujours d�fini. *)

(* 8. On ajoute les transitions manquantes obtenues en suivant les replis: *)

let a2 = {final = [|false;false;false;true|] ; transition = [|[|1;0|] ; [|1;2|] ; [|3;0|] ; [|1;2|]|] ; repli = [|0;0;0;0|]} ;;

(* 9. Le langage reconnu par A1 (et A2) est l'ensemble des mots se terminant par le motif aba. *)

(* 10. *)

let copie_afdr a = 
  let k = Array.length (a.final) in
  let copie_trans = Array.make k [||] in
  for i = 0 to k-1 do
    copie_trans.(i) <- Array.copy (a.transition.(i))
  done;
  {final = Array.copy (a.final) ; transition = copie_trans ;
   repli = Array.copy (a.repli) } ;;

(* 11. La fonction de repli envoyant toujours sur un �tat strictement plus petit, on peut remplir le nouveau tableau de transition par ordre croissant. *)

let enleve_repli a =
  let a2 = copie_afdr a in
  let k = Array.length (a.transition) and la = Array.length (a.transition.(0)) in
  for i = 1 to k-1 do
    for j = 0 to la-1 do
      if a.transition.(i).(j) = -1 then
	a2.transition.(i).(j) <- a2.transition.(a.repli.(i)).(j) (* comme on parcourt par ordre croissant, � ce moment-l� le terme de droite ne vaut jamais -1 *)
    done;
  done;
  a2 ;;

(* La complexit� est bien en O(k*lambda) car on a deux boucles for imbriqu�es de tailles k et lambda avec des op�rations de co�t constant � l'int�rieur. *)

let a2p = enleve_repli a1 ;; 
a2.final = a2p.final ;;
a2.transition = a2p.transition ;;			
(* on retrouve a2 sauf pour le tableau repli qui n'a aucune importance *)

(* 12. Comme l'automate est d�terministe complet, il suffit de lire une seule fois le mot, � chaque �tape on regarde si on se trouve dans un �tat final, si oui on ajoute l'indice correspondant � la liste des occurrences.  *)

(* 13. *)

let occurrences a s =
  let rec parcours i e s = 		(* i est l'indice courant et e l'�tat courant dans l'automate *)
    match s with
      |[] -> []
      |t::q -> let e2 = a.transition.(e).(t) in
	       if a.final.(e2) then i::(parcours (i+1) e2 q)
	       else parcours (i+1) e2 q 
  in parcours 1 0 s ;;

(* Complexit� : on ne parcourt qu'une seule fois la liste s, � chaque �tape on ne fait que des op�rations de co�t constant (et l'appel r�cursif sur la queue de la liste) donc le co�t est bien en O(n) et ne d�pend pas de k et de lambda. (c'est le co�t de construction de l'automate qui d�pend de k et lambda) *)

let t2 = [0;1;0;0;1;0;0;0;1;0;1;0;0;0;1;0;0;0;1;0;0] ;;
occurrences a2 t2 ;;


(* 3. Automate de Knuth-Morris-Pratt *)

(* 14. *)

let a3 = {final = [|false;false;false;false;false;true|] ; transition =[|[|1;0;0|] ;[|-1;2;-1|] ; [|3;-1;-1|] ; [|-1;4;-1|] ; [|-1;-1;5|]; [|-1;-1;-1|]|] ; repli = [|0;0;0;1;2;0|]} ;;

(* 15. L'automate de KMP reconna�t l'ensemble des mots se terminant par le motif s. *)

(* 16. voir corrig� joint *)

(* 17. *)

let automate_kmp s lambda = 		(* lambda est la taille de l'alphabet *)
  let k = longueur s in
  let fin = Array.make (k+1) false in (* construction du tableau final *)
  fin.(k) <- true ;
  let delta = Array.make_matrix (k+1) lambda (-1) in (* construction du tableau des transitions *)
  for j = 0 to lambda-1 do					  (* on met des 0 pour les transitions partant de l'�tat initial *)
    delta.(0).(j) <- 0; 
  done;
  let rec remplir i s = 			(* parcourt s pour remplir le tableau des transitions, i est l'indice courant *)
    match s with
      |[] -> ()
      |u::q -> delta.(i-1).(u) <- i ; remplir (i+1) q
  in remplir 1 s ;
  let rho = Array.make (k+1) 0 in 	(* construction du tableau des replis *)
  let rec remplir2 i s = 		(* pour remplir rho, on utilise la caract�risation de la fonction pr�c�dente *)
    match s with
      |[] -> ()
      |u::q -> let e = ref rho.(i-1) in (* on utilise une r�f�rence pour simuler les rho^j(i-1) *)
	       while delta.(!e).(u) = -1 do
		 e := rho.(!e)
               done;
               rho.(i) <- delta.(!e).(u);
               remplir2 (i+1) q 
  in remplir2 2 (List.tl s) ; 		(* attention la caract�risation n'est valable qu'� partir de i = 2, il faut donc d�marrer � 2 en enlevant la premi�re lettre de s *)
  {final = fin ; transition = delta ; repli = rho} ;;

a3 = automate_kmp [0;1;0;1;2] 3 ;; 		(* on retrouve bien a3 *)

(* 18-19 : voir corrig� joint *)

(* 20. *)

let recherche_kmp s t lambda =
  let a = enleve_repli(automate_kmp s lambda) in
  occurrences a t ;;

(* Complexit� en O(n + k*lambda), si k << n c'est mieux que recherche_naive qui est en O(n*k). *)

recherche_kmp s t 5 = recherche_naive s t ;; (* on retrouve la m�me liste d'occurrences *)
