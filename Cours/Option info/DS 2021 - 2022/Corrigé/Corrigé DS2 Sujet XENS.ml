(* X-ENS 2016 : Satisfiabilit� de formules bool�ennes *)

(* Pr�liminaires *)

type formule =
  |Var of int
  |Non of formule
  |Et of formule*formule
  |Ou of formule*formule ;;

(* Question 1 *)

(* a) Non satisfiable � cause Et(x1, Non(x1)). *)

(* b) Satisfiable, par exemple x0 = x1 = x2 = Vrai. *)

(* c) Satisfiable, par exemple x0 = x1 = x2 = Vrai. *)

(* d) Non satisfiable. *)

type litteral = 
  |V of int
  |NV of int ;;

type clause = litteral list ;;

type fnc = clause list ;;

(* Question 2 *)

let var_max f =
  let rec aux1 c m = 			(* aux1 renvoie le minimum d'une clause c, m est le maximum courant *)
    match c with
      |[] -> m
      |V(i)::q -> aux1 q (max i m)
      |NV(i)::q -> aux1 q (max i m)
  in let rec aux2 f m = 		(* aux2 renvoie le minimum d'une formule, m est le minimum courant *)
       match f with
	 |[] -> m
	 |c::q -> aux2 q (aux1 c m)
     in aux2 f 0 ;; 			(* les indices sont suppos�s positifs donc on peut d�marrer le min � 0, sinon on peut prendre min_int *)

let f = [[V(1);NV(2)] ; [NV(0)] ; [V(2);V(1)]] ;;

var_max f;;

(* Partie I. R�solution de 1-SAT *)

type trileen =
  |Vrai
  |Faux
  |Indetermine ;;

(* Question 3 *)

let un_sat f =
  let n = var_max f in
  let tab = Array.make (n+1) Indetermine in
  let rec aux f  = 			
    match f with
      |[] -> true 			(* on renvoie true quand on a atteint la fin de la liste sans avoir crois� de contradiction *)
      |c::q -> match List.hd c with 	(* on suppose que les clauses sont de taille 1, pour acc�der � cet �l�ment on utilise hd *)
	  |V(i) -> if tab.(i) = Faux then false (* on renvoie false d�s qu'on croise une contradiction, pas d'appel r�cursif � faire dans ce cas *)
                   else (tab.(i) <- Vrai ; aux q)
	  |NV(i) -> if tab.(i) = Vrai then false
                   else (tab.(i) <- Faux ; aux q)
  in aux f ;;
                    

let f1 = [[V(1)];[NV(0)];[NV(1)];[V(2)]] ;;
let f2 = [[V(1)];[NV(0)];[V(1)];[V(2)]] ;;

un_sat f1 ;;
un_sat f2 ;;

(* Partie II. R�solution de 2-SAT *)

(* II.1. Recherche de composantes fortement connexes dans un graphe orient� *)

type graphe = int list array ;;

let g = [| [] ; [4;3] ; [3] ; [2;0] ; [0;1] ; [2;7] ; [5;1;2] ; [6] |] ;;

let rec do_list f l =
  match l with
    |[] -> ()
    |x::q -> f x ; do_list f q ;;

let dfs_tri g =
  let deja_vu = Array.make (Array.length g) false in
  let resultat = ref [] in
  let rec dfs_rec i =
    if not deja_vu.(i) then begin
      deja_vu.(i) <- true;
      do_list dfs_rec g.(i) ;
      resultat := i :: (!resultat) ;
    end in
  for i = 0 to Array.length g - 1 do dfs_rec i done ;
  !resultat ;;

dfs_tri g ;;

(* Question 4 : Il y a tout d'abord un co�t de O(|V|) pour la cr�ation du tableau deja_vu. Comme on marque les sommets d�j� visit�s � l'aide de ce tableau, chaque liste d'adjacence n'est parcourue qu'une seule fois. La somme des longueurs des listes valant |E|, et pour tout i on effectue au plus 1 + li (o� li est la longueur de sa liste d'adjacence) appels r�cursifs (l'�l�ment i + ses voisins), cela fait au plus |V|+|E| appels r�cursifs. Les autres op�rations effectu�s dans chaque appel r�cursif sont � co�t constant, donc la complexit� est en O(|V|+|E|)=O(|G|).*)

(* Question 5 *)

let rec renverser_graphe g =
  let n = Array.length g in
  let gp = Array.make n [] in
  let rec aux i l = 			(* parcourt la liste d'adjacence de i et met � jour gp *)
    match l with
      |[] -> ()
      |j::q -> gp.(j) <- i::(gp.(j)) ; aux i q
  in for i = 0 to n-1 do aux i g.(i) done ;
  gp ;;
   
renverser_graphe g ;;

(* Question 6 *)

let dfs_cfc g l =
  let deja_vu = Array.make (Array.length g) false in
  let cfc = ref [] in 			(* cfc contiendra une composante fortement connexe *)
  let rec composante i = 		(* construit la cfc de i *)
    if not deja_vu.(i) then begin
      deja_vu.(i) <- true;
      do_list composante g.(i);
      cfc := i::(!cfc);
    end in
  let resultat = ref [] in
  let rec parcours l = 			(* parcours la liste l donn�e par le premier parcours et applique composante � chaque sommet non visit� *)
    match l with
      |[] -> ()
      |i::q -> if not deja_vu.(i) then begin
	         cfc := [] ; 		(* il faut remettre cfc � [] � chaque nouveau sommet *)
	         composante i ;
                 resultat := (!cfc)::(!resultat); 
               end ; parcours q
  in parcours l ; 
  !resultat ;;
    
      
dfs_cfc (renverser_graphe g) (dfs_tri g) ;;

(* Question 7 *)

let cfc g =
  dfs_cfc (renverser_graphe g) (dfs_tri g) ;;

cfc g ;;

(* Question 8 *)

(* Relation r�flexive : C est subordonn�e � elle-m�me, il suffit de prendre un chemin trivial d'un sommet v de C � lui-m�me. *)

(* Relation transitive : si C est subordonn�e � C' et C' est subordonn�e � C", il existe un chemin de v" � v1' et de v2' � v avec v dans C, v1' et v2' dans C' et v" dans C", comme v1' et v2' sont dans la m�me cfc il existe un chemin entre les deux, on concat�ne les trois chemins pour obtenir un chemin de v" � v, donc C est subordonn�e � C". *)

(* Relation antisym�trique : si C est subordonn�e � C' et C' est subordonn� � C, cela veut dire qu'il existe un chemin entre tout sommet v de C et tout sommet v' de C' et r�ciproquement, donc par d�finition de la cfc cela veut dire qu'ils sont dans la m�me composante fortement connexe, donc C=C'. *)

(* Question 10 :  On montre d'abord le r�sultat interm�diaire suivant : si i et j sont deux sommets tels qu'il existe un chemin de i vers j mais pas de j vers i, alors t_j < t_i. On traite deux cas : 
   1) i est visit� avant j lors du parcours : comme il y a une ar�te de i vers j, il y aura un appel r�cursif � j lors de celui de i (principe de pile : "j est empil� au-dessus de i"), donc le traitement de i se finira apr�s celui de j, donc t_j < t_i.
   2) j est visit� avant i : comme il n'y a pas de chemin de j vers i, il n'y aura pas d'appel r�cursif � i lors du traitement de j, donc le traitement de j se terminera avant que celui de i ait commenc�, donc � nouveau t_j < t_i. *)

(* On fait ensuite l'observation suivante :  si C est une cfc, t_C = t_x o� x est le premier sommet de C rencontr� (tous les autres sommets seront visit�s lors de l'appel r�cursif dfc_tri x). *)

(* Soient C et C' deux cfc distinctes avec C subordonn�e � C'. Cela veut dire qu'il existe un chemin entre un i de C et un j de C' (mais pas de chemin de j vers i). On traite d'abord le cas o� ce chemin est r�duit � une ar�te. Par ce qui pr�c�de on a alors t_j < t_i <= t_C. Soit k le premier sommet de C' visit�. On a donc t_C' = t_k. On traite deux cas :
 -Si i est visit� avant k : comme j est visit� lors du parcours de i, et que k doit �tre visit� avant j, n�cessairement k est visit� lors du parcours de i donc t_k < t_i, d'o� t_C' < t_C.
 -Si k est visit� avant i : il n'y a pas de chemin entre k et i donc i ne sera pas visit� lors du parcours de k, donc � nouveau t_k < t_i et t_C' < t_C.  *)

(* Si le chemin entre i et j est de longueur > 1, on le d�compose et on montre la m�me chose par r�currence sur la longueur du chemin. *)

(* Question 11 : Soient C1,...,Cp les cfc ordonn�es par temps de fin de traitement croissant (de sorte que si Ci est subordonn�e � Cj dans G, alors i <= j).. Dans le graphe G', les relations de subordination sont invers�es. Lors de l'�tape b), on d�marre avec le sommet i avec t_i maximal, il appartient donc � Cp. Lors du parcours de i dans G', on ne pourra pas croiser de sommets issus d'une autre cfc Cj, car sinon cela voudrait dire que Cj est subordonn�e � Cp dans G', donc Cp subordonn�e � Cj dans G, et donc par la question 10 t_Cp < t_Cj ce qui contredit la d�finition des Ci. Donc on ne croisera que des sommets issus de Cp lors du parcours de i. On les croise n�cessairement tous car par d�finition de la cfc, ils sont tous atteignables depuis i. Donc quand le parcours de i se termine, on a obtenu sa cfc Cp. On continue ensuite avec les autres Cj pour j d�croissant. On obtient donc successivement toutes les cfc. *)

(* II.2. Des composantes fortement connexes � 2-SAT *)

(* Question 12 : On supprime la clause Ou(x2, Non(x2)), on obtient un graphe � 6 sommets et 5 ar�tes : (1,0), (5,2),(3,4), (4,0), (1,5). *)

(* Question 13 *)

let deux_sat_vers_graphe f =
  let n = var_max f in
  let g = Array.make (2*n+2) [] in 	(* attention au nombre de sommets *)
  let rec parcours f =
    match f with
      |[] -> ()
      |c::q -> (if List.length c = 1 then match List.hd c with (* clause � 1 litt�ral *)
	  |V(i) ->  g.(2*i+1) <- (2*i)::(g.(2*i+1))
          |NV(i) -> g.(2*i) <- (2*i+1)::(g.(2*i))
        else match (List.hd(c), List.hd(List.tl c)) with (* clause � 2 litt�raux *)
	  |(V(i),V(j)) -> g.(2*i+1) <- (2*j)::(g.(2*i+1));  g.(2*j+1) <- (2*i)::(g.(2*j+1))
	  |(V(i),NV(j)) ->  g.(2*i+1) <- (2*j+1)::(g.(2*i+1));  g.(2*j) <- (2*i)::(g.(2*i))          |(NV(i),V(j)) ->  g.(2*i) <- (2*j)::(g.(2*i)) ;  g.(2*j+1) <- (2*i+1)::(g.(2*j+1) )
	  |(NV(i),NV(j)) -> g.(2*i) <- (2*j+1)::(g.(2*i)) ; g.(2*j) <- (2*i+1)::(g.(2*j)));
        parcours q
  in parcours f ;
  g ;;

let f1 = [[V(1);V(2)];[V(0)];[NV(2);V(0)]] ;;

deux_sat_vers_graphe f1 ;;

(* Question 14 *)

(* On fait d'abord la remarque suivante : si li et lj sont les litt�raux associ�s aux sommets i et j, et s'il existe un chemin de i vers j dans G, alors la valuation sigma satisfait la formule li => lj. Cela est vrai si i et j sont reli�s par une ar�te par construction de G, et pour un chemin de longueur arbitraire il suffit de le d�composer. *)

(* Donc, si i et j sont dans la m�me composante fortement connexe de G, on a � la fois li => lj et lj => lj donc li et lj ont m�me valeur de v�rit�. Si i-j est pair, cela signifie que li et lj ont m�me signe (ce sont soit 2 variables soit 2 n�gations de variables), donc les variables correspondantes ont m�me valeur, sinon elles ont des valeurs oppos�es. *)

(* Question 15 : Si f est satisfiable et si v_i et v_{i+1} sont dans la m�me cfc, par ce qui pr�c�de x_i aurait m�me valeur de v�rit� que sa n�gation, or c'est impossible. Donc cela ne peut pas arriver. *)

(* Question 16 : Attention il faut une complexit� lin�aire ! Comme les sommets des cfc ne sont pas tri�s, il faut v�rifier efficacement qu'on n'a pas de sommets cons�cutifs dans une m�me cfc en ne parcourant qu'une seule fois chaque cfc. Pour cela on peut cr�er un tableau o� l'on r�f�rence le num�ro de la composante de chaque sommet.*)

let deux_sat f =
  let n = var_max f in
  let g = deux_sat_vers_graphe f in
  let comp = cfc g in
  let num = Array.make (2*n+2) 0 in 		(* tableau des num�ros des composantes *)
  let rec rempli_comp c i =				(* fonction qui parcourt la composante i *)
    match c with
      |[] -> ()
      |j::q -> num.(j) <- i ; rempli_comp q i
  in let rec rempli_cfc l i = 			(* fonction qui parcourt la liste des cfc et remplit le tableau num *)
    match l with
      |[] -> ()
      |c::q -> rempli_comp c i ; rempli_cfc q (i+1)
  in rempli_cfc comp 0 ;
  let b = ref true in
  for i = 0 to n do
    b := !b && (num.(2*i) != num.(2*i+1)) (* on regarde si la composante de 2*i et la m�me que celle de 2*i+1 *)
  done;
  !b ;;
  
let f1 = [[V(1);V(2)];[V(0)];[NV(2);V(0)]] ;;  
deux_sat f1 ;;

let f2 = [[V(0)];[NV(0)]] ;;
deux_sat f2 ;;

let f3 = [[V(0);V(1)];[NV(0);V(1)];[V(0);NV(1)];[NV(0);NV(1)]] ;;
deux_sat f3 ;;


(* Partie III. R�solution de k-SAT pour k arbitraire *)

(* Question 17 *)

let et t1 t2 =
  if t1 = Faux || t2 = Faux then Faux
  else if t1 = Vrai && t2 = Vrai then Vrai
  else Indetermine ;; 			(* plus rapide d'exploiter les sym�tries que de faire un matching sur le couple *)

let ou t1 t2 =
  if t1 = Vrai || t2 = Vrai then Vrai
  else if t1 = Faux && t2 = Faux then Faux
  else Indetermine ;;

let non t =
  match t with
    |Vrai -> Faux
    |Faux -> Vrai
    |Indetermine -> Indetermine ;;

(* Question 18 *)

let eval f t =
  let rec eval_clause c = 		(* �value une clause *)
    match c with
      |[] -> Faux 			(* �lement neutre du ou *)
      |V(i)::q -> if t.(i) = Vrai then Vrai else ou t.(i) (eval_clause q)
      |NV(i)::q -> if t.(i) = Faux then Vrai else ou (non t.(i)) (eval_clause q)
  in let rec eval_for f =
       match f with
	 |[] -> Vrai 			(* �l�ment neutre de et *)
	 |c::q -> let b = eval_clause c in
		  if b = Faux then Faux else et b (eval_for q)
     in eval_for f ;;

(* Question 19 : le plus efficace est de faire appel � une exception qui est lev�e lorsqu'on atteint une valuation qui satisfait la formule. *)

exception Satisf ;;

let k_sat f =
  let n = var_max f in
  let t = Array.make (n+1) Indetermine in
  let rec parcours f k = 		(* k est le niveau de ligne courante *)
    match k with
      |k when eval f t = Vrai -> raise Satisf
      |k when eval f t = Faux -> () 	(* quand on trouve Faux on interrompt l'exploration de la branche courante *)
      |k -> t.(k) <- Vrai; parcours f (k+1);
            t.(k) <- Faux; parcours f (k+1)
  in try parcours f 0 ; false
  with Satisf -> true ;;				(* si aucune exploration n'est satisfaisante, on renvoie false *)

k_sat f1 ;;
k_sat f2 ;;
k_sat f3 ;;


(* Partie IV. De k-SAT � SAT *)

(* Question 20 : on obtient apr�s calculs (on donne les r�sultats sous forme de listes de litt�raux en utilisant le typage pr�c�dent) : *)

(* a) [ [V(1);NV(0)] ; [NV(4);V(3)] ; [NV(4);V(2)] ]  *)

(* b) [ [V(0);V(2);V(4)] ;[V(0);V(2);V(5)] ; [V(0);V(3);V(4)] ; [V(0);V(3);V(5)] ;
        [V(1);V(2);V(4)] ;[V(1);V(2);V(5)] ; [V(1);V(3);V(4)] ; [V(1);V(3);V(5)] ] *)


(* Question 21 : Notons V(f) l'ensemble des variables de f. On montre par induction structurelle sur f* le r�sultat suivant:
  
    "Si f* est satisfiable par une valuation sigma, il existe une valuation sigma' qui prolonge sigma � V(f') et satisfait f'. R�ciproquement si f' est satisfaite par sigma, sa restriction � V(f* ) satisfait f*. " *)


(* Question 22 *)

let rec neg_en_bas f =
  match f with
    |Var(i) -> Var(i)
    |Et(f1,f2) -> Et(neg_en_bas f1, neg_en_bas f2)
    |Ou(f1,f2) -> Ou(neg_en_bas f1, neg_en_bas f2)
    |Non(Var(i)) -> Non(Var(i))
    |Non(Non(f1)) -> neg_en_bas f1
    |Non(Et(f1,f2)) -> Ou(neg_en_bas (Non(f1)), neg_en_bas (Non(f2)))
    |Non(Ou(f1,f2)) -> Et(neg_en_bas (Non(f1)), neg_en_bas (Non(f2)));;


(* Question 23 *)

let rec var_max f = 			(* suppos�e �crite *)
  match f with
    |Var(i) -> i
    |Non(f1) -> var_max f1
    |Et(f1,f2) -> max (var_max f1) (var_max f2)
    |Ou(f1,f2) -> max (var_max f1) (var_max f2) ;;

let formule_vers_fnc f =
  let num = ref(var_max f) in 		(* servira pour introduire les nouvelles variables *)
  let rec aux f =
    match f with
      |Var(i) -> [[V(i)]]
      |Non(Var(i)) -> [[NV(i)]]
      |Non(f) -> failwith "pas sous la bonne forme"
      |Et(f1,f2) -> (aux f1)@(aux f2)
      |Ou(f1,f2) -> let x = !num + 1 in incr(num) ; 
                    let f3 = aux f1 and f4 = aux f2 in
		    let f5 = List.map (fun c -> V(x)::c) f3
                    and f6 = List.map (fun c -> NV(x)::c) f4
		    in f5@f6 
  in aux f ;;
  

let f = Et(Ou(Var(1),Non(Var(0))),Non(Et(Var(4),Non(Et(Var(3),Var(2)))))) ;;

let fe = neg_en_bas f ;;

formule_vers_fnc fe ;;

(* Question 24 *)

(* La fonction neg_en_bas r�alise un parcours en profondeur de l'arbre associ� � la formule donc son c�ut est lin�aire en la taille de la formule.  *)

(* Pour la fonction formule_vers_fnc, il y a �galement un parcours en profondeur de l'arbre de la formule mais il faut prendre en compte le co�t des concat�nations et des appels � map. On montre par induction structurelle sur f* le r�sultat indiqu� par l'�nonc�. Donc le nombre de concat�nations et d'appels � map est lin�aire en le nombre de litt�raux, donc en la taille de f*. Chaque concat�nation/appel � map a un co�t lin�aire donc le co�t total est quadratique en la longueur de f*.  *)
