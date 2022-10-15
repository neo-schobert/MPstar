(* TP sur les arbres binaires de recherche *)

type 'a arbre = Nil | Noeud of 'a arbre  * 'a  * 'a arbre ;;

(* Question 1 *)

let g x = (x*x + 234) mod 32069 ;;

let u = Array.make 1000 2014 ;;

for i = 1 to 999 do
  u.(i) <- g u.(i-1)
done ;;

u.(100);;
u.(500);;

(* Question 2 *)

let rec recherche c a = match a with
  | Nil -> false
  | Noeud(_, x, _) when x = c  -> true
  | Noeud(fg, x, fd) when c < x -> recherche c fg
  | Noeud(fg, x, fd) -> recherche c fd ;;

(* Insertion aux feuilles *)

let rec insere_feuille c a = match a with
  | Nil -> Noeud(Nil, c, Nil)
  | Noeud(fg, x, fd) when c = x -> Noeud(fg, x, fd)
  | Noeud(fg, x, fd) when c < x -> Noeud(insere_feuille c fg, x, fd)
  | Noeud(fg, x, fd) -> Noeud(fg, x, insere_feuille c fd) ;; 

(* Insertion à la racine (bonus) *)

let insere_racine c a =
  let rec partition a  = match a with
    | Nil -> Nil, Nil
    | Noeud(_, x, _) when c = x -> failwith "clé déjà présente"
    | Noeud(fg, x, fd) when c < x -> let a1, a2 = partition fg in a1, Noeud(a2, x, fd)
    | Noeud(fg, x, fd) -> let a1, a2 = partition fd in Noeud(fg, x, a1), a2 
  in let a1, a2 = partition a in Noeud(a1, c, a2) ;;

(* Question 3 *)

(* Avec une référence d'ABR *)

let a = ref Nil;;
for i = 0 to 999 do
  a := insere_feuille u.(i) !a
done ;;

recherche 25019 !a ;;

recherche 25020 !a ;;

(* Avec une fonction auxiliaire récursive *)

let rec remplit i acc = 			(* i est l'indice de parcours du tableau u, acc est un accumuateur contenant l'arbre à construire *)
  match i with
    |1000 -> acc
    |i -> remplit (i+1) (insere_feuille u.(i) acc) ;;

let a2 = remplit 0 Nil ;;

!a = a2 ;;

(* Question 4 *)

let rec maximum a =
  match a with
    |Nil -> failwith "arbre vide"
    |Noeud(fd,x,Nil) -> x 		(* ne pas oublier ce cas sinon on a toujours une erreur *)
    |Noeud(fg,x,fd) -> maximum fd ;;

maximum !a ;;

(* Question 5 *)

let rec infixe a = match a with
  | Nil -> []
  | Noeud(fg, x, fd) -> (infixe fg) @ x::(infixe fd);;

infixe !a ;;

(* Question 6 *)

(* Fonctio auxiliaire qui supprime et renvoie le minimum *)

let rec supprime_min a = match a with
  | Nil -> failwith "arbre vide"
  | Noeud(Nil, x, fd) -> x, fd
  | Noeud(fg, x, fd) -> let m, b = supprime_min fg in m, Noeud(b, x, fd) ;;

let rec supprime c a = match a with
  | Nil -> failwith "clé non présente"
  | Noeud(fg, x, fd) when c < x -> Noeud(supprime c fg, x, fd)
  | Noeud(fg, x, fd) when c > x -> Noeud(fg, x, supprime c fd)
  | Noeud(Nil, x, fd) -> fd
  | Noeud(fg, x, Nil) -> fg
  | Noeud(fg, x, fd) -> let m, b = supprime_min fd in Noeud(fg, m, b) ;;

let b = supprime 25019 !a ;;

taille b ;;

hauteur b ;;

(* Question 7 *)

let rec valider_abr a = match a with   (* renvoie true/false et le min et le max de l'ABR *)
  | Nil -> failwith "arbre vide"
  | Noeud(Nil, x, Nil) -> true, x, x
  | Noeud(fg, x, Nil) -> let bg, ming, maxg = valider_abr fg in bg&& maxg < x, ming, x
  | Noeud(Nil, x, fd) -> let bd, mind, maxd = valider_abr fd in bd&& mind > x, x, maxd
  | Noeud(fg, x, fd) -> let bg, ming, maxg = valider_abr fg and bd, mind, maxd = valider_abr fd in bg && bd && maxg < x && mind > x, ming, maxd ;; 

(* Autre solution : regarder si le parcours infixe est croissant *)

let valide_abr a =
  let rec croissant l =
    match l with
      |[] -> true
      |[a] -> true
      |a::b::q -> (a<=b) && (croissant (b::q))
  in croissant (infixe a) ;;

let a1 = Noeud(Noeud(Noeud(Nil,1,Nil) ,3, Nil) , 5, Noeud(Noeud(Nil,6,Nil) ,9,Nil));;

valider_abr a1 ;;

let a2 = Noeud(Noeud(Noeud(Nil,1,Nil) , 3, Nil) , 7 ,Noeud(Noeud(Nil,6,Nil) , 9, Nil) );;

valider_abr a2 ;;

(* Question 8 *)

let rec prefixe a = match a with
  | Nil -> []
  | Noeud(fg, x, fd) -> x::(prefixe fg)@(prefixe fd);;

prefixe !a ;;

(* Question 9 *)

let l =  ref (prefixe !a) ;;

let b = ref Nil;;

while !l <> [] do
  b := insere_feuille (List.hd !l) !b;
  l := List.tl !l
done ;;

!a = !b ;;

(* Question 10 *)

let rec insere_feuille_repet c a = match a with
  | Nil -> Noeud(Nil, c, Nil)
  | Noeud(fg, x, fd) when c < x -> Noeud(insere_feuille_repet c fg, x, fd)
  | Noeud(fg, x, fd) -> Noeud(fg, x, insere_feuille_repet c fd) ;;

(* Question 11 *)

let tri_tableau t = 
  let n = Array.length t and abr = ref Nil in
  for i = 0 to n-1 do
    abr := insere_feuille_repet t.(i) !abr
  done;
  let liste_triee = ref (infixe(!abr)) in
  for i = 0 to n-1 do
    t.(i) <- List.hd !liste_triee ;
    liste_triee := List.tl !liste_triee
  done;;


let t = [|4; 5; 1; 3; 9; 5|];;

tri_tableau t;;

t;;
