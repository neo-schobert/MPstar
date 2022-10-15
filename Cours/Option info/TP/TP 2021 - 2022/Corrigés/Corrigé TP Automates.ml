(* TP sur les automates *)

(* 1. Automates déterministes *)

type automate = {n : int ; i : int ; t : int list ; gamma : (int*char*int) list} ;;

(* Quelques exemples *)

(* Automate reconnaissant les mots écrits avec un nombre pair de 0 et impair de 1 *)

let a1 = { n = 4 ; i = 0 ; t = [3] ; gamma = [(0,'0',1);(1,'0',0);(1,'1',2);(2,'1',1);(2,'0',3);(3,'0',2);(3,'1',0);(0,'1',3)] };;

(* Automate reconnaissant le langage défini par a + b*c  *)

let a2 = { n = 4; i = 0; t =[1;3] ; gamma = [(0,'a',1);(0,'b',2);(0,'c',3);(2,'b',2);(2,'c',3)]};;

(* Automate reconnaissant le langage défini par (aa+ab)*ba* *)

let a3 = { n = 6; i = 0; t=[2;3] ; gamma = [(0,'a',1);(0,'b',2);(2,'a',3);(3,'a',3);(1,'a',4);(1,'b',5);(4,'a',1);(5,'a',1);(4,'b',2);(5,'b',2)]};;

(* Calculs : Méthode 1 *)

let rec chercheTransition e c l = match l with
  | [] -> -1
  | (e',c',f)::q -> if e=e' && c=c' then f else chercheTransition e c q ;;

chercheTransition 3 'b' a3.gamma ;;

let calcul a s = 
  let n = String.length s and etat = ref a.i in 
  for i = 0 to n-1 do
    if !etat != -1 then
      etat := chercheTransition !etat s.[i] a.gamma;
  done;
  List.mem !etat a.t ;;

calcul a1 "011001" ;;
calcul a3 "aaabaabaaaba" ;;

(* Calculs : Méthode 2 *)

let table_de_transition a =
  let table = Array.make_matrix (a.n) 256 (-1) in
  let rec remplissage l = match l with
    |[] -> ()
    |(e,c,f)::q -> table.(e).(int_of_char c) <- f; remplissage q
  in remplissage a.gamma;
  table ;;

let calcul_bis a s =
  let table = table_de_transition a and n = String.length s and etat = ref a.i in
  for i = 0 to n-1 do
    if !etat != -1 then
      etat := table.(!etat).(int_of_char s.[i]);
  done;
  List.mem !etat a.t ;;

calcul_bis a1 "01101";;
calcul_bis a3 "aaabaabaaaba" ;;

(* 2. Automates non déterministes *)

(* 2.1. Opérations ensemblistes *)

let appartient i t = t.(i) ;;

let vide n = Array.make n false ;;

let t = vide 5 ;;
appartient 4 t ;;

let union t1 t2 =
  let n = Array.length t1 in
  let t = vide n in 
  for i = 0 to n-1 do
    t.(i) <- t1.(i) || t2.(i)
  done;
  t ;;

let intersection t1 t2 =
  let n = Array.length t1 in
  let t = vide n in
  for i = 0 to n-1 do
    t.(i) <- t1.(i) && t2.(i)
  done;
  t ;;

let t1 = [| true ; false ; true ; true |];;
let t2 = [| false ; true ; true ; false |];;
union t1 t2;;
intersection t1 t2;;

let ajoute t1 t2 =			
  let n = Array.length t1 in
  for i = 0 to n-1 do
    t1.(i) <- t1.(i) || t2.(i)
  done;;

ajoute t1 t2;;
t1 ;;

let inclusion t1 t2 = 
  let n = Array.length t1 and rep = ref true in
  for i = 0 to n-1 do
    rep := !rep && (t2.(i) || not t1.(i))
  done;
  !rep ;;

inclusion t1 t2;;
inclusion t2 t1;;

(* 2.2 Calculs avec un automate non déterministe *)

type automate_nd = {n_nd : int ; i_nd : bool array ; t_nd : bool array ; gamma_nd : (int*char*int) list } ;;

let a = {n_nd = 4 ; i_nd = [|true;false;false;false|] ; t_nd = [|false;false;false;true|] ; gamma_nd = [(0,'a',0);(0,'b',0);(0,'a',1);(1,'a',1);(1,'b',1);(0,'b',2);(2,'a',2);(2,'b',2);(2,'b',3);(1,'a',3)] };;

let gamma a e c =
  let n = a.n_nd in 
  let t = vide n in
  let rec remplissage l = match l with
    |[] -> ()
    |(e',c',f)::q -> if (e=e' && c=c') then t.(f) <- true; remplissage q
  in remplissage a.gamma_nd;
  t ;;

gamma a 0 'a';;
gamma a 1 'b';;

let gamma_e a t c =
  let n = a.n_nd in
  let tp = vide n in
  for i = 0 to n-1 do
    if t.(i) then
      ajoute tp (gamma a i c);
  done;
  tp ;;

let t = [|true; true; false; false|];;
gamma_e a t 'a';;

let gamma_etoile a t s =
  let m = String.length s and tp = ref t in
  for i = 0 to m-1 do
    tp := gamma_e a (!tp) s.[i]
  done;
  !tp ;;
    
gamma_etoile a t "ab" ;;
gamma_etoile a t "" ;;

let calcul_nd a s =
  let n = a.n_nd and t = gamma_etoile a (a.i_nd) s and rep = ref false in
  for i = 0 to n-1 do
    rep := !rep || (t.(i) && (a.t_nd).(i)) (* on teste si t contient un état terminal *)
  done;
  !rep ;;

calcul_nd a "abaab" ;;
calcul_nd a "ba" ;;
calcul_nd a "ab" ;;

(* Déterminisation *)

let numerote t =
  let n = Array.length t and rep = ref 0 in
  for i = 0 to n-1 do
    if t.(i) then 
      rep := !rep + (1 lsl i)
  done;
  !rep ;;

numerote t1 ;;
numerote t2 ;;

let partie taille n =
  let t = vide taille and q = ref n and i = ref 0 in
  while !q > 0 do
    t.(!i) <- ((!q mod 2)=1);
    q := !q / 2;
    incr(i);
  done;
  t ;;

partie 4 13 ;;
partie 4 6 ;;

let determinise aut =
  let m = aut.n_nd in
  let term = ref [] in			(* on cherche les états terminaux *)
  for i = 0 to (1 lsl m)-1 do
    let q = partie m i in
    if numerote (intersection q aut.t_nd) != 0 then
      term := (numerote q)::(!term)
  done;
  let trans = ref [] in			(* on cherche les transitions *)
  for i = 0 to (1 lsl m)-1 do
    for j = 0 to 255 do
      let d = (gamma_e aut (partie m i) (char_of_int j)) in
      if numerote d != 0 then
	trans := (i,char_of_int j,numerote d)::(!trans);
    done;
  done;
  {n = 1 lsl m ; i = numerote aut.i_nd ; t = !term ; gamma = !trans} ;;

let ad = determinise a ;;

calcul ad "ab" ;;
calcul ad "ba" ;;
calcul ad "abaab" ;;
