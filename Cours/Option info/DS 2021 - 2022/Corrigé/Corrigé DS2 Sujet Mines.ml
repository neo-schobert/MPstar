(* Mines-Ponts 2012 *)

(* Exercice sur les langages rationnels *)

(* On note l'alphabet A au lieu de Sigma *)

(* 1) L1 est d�crit par l'expression rationnelle A*.f.A* donc est rationnel. *)

(* 2) De m�me, L2 est d�crit par l'expression rationnelle A*.f.A*.f.A* donc est rationnel. *)

(* 3) Il nous faut d�crire le fait d'avoir deux occurrences imbriqu�es de f. Notons Lp et Ls les langages des mots ayant respectivement f comme pr�fixe et comme suffixe (ils sont rationnels car d�crits par les expressions rationnelles f.A* et A*.f). Le langage des mots ayant � la fois f comme pr�fixe et comme suffixe est l'intersection de Lp et Ls, il est rationnel car l'intersection de deux langages rationnels est rationnelle (r�sultat du cours). Si l'on veut exclure les mots dont les occurrences sont disjointes, il faut �liminer le mot f et les mots de L2. On obtient un langage Lps qui est toujours rationnel car l'ensemble des langages rationnels est stable par compl�mentation. Enfin, le langage L3 est �gal � A*.L3.A* donc est rationnel. *)

(* 4) L4 = L1 \ (L2 + L3) donc est rationnel par les r�sultats de cl�ture du cours. *)

(* Probl�me d'algorithmique et programmation *)

(* Premi�re partie : g�n�ralit�s *)

(* 5) Les ar�tes {0,0}, {1,3} et {2,2} forment un couplage de cardinal 3 dans G0. *)

(* 6) Il n'y a pas de couplage de cardinal 4 dans G0, en effet si tel �tait le cas, chaque sommet appara�trait dans ce couplage. Or 1A et 3A ne sont reli�s qu'� une seule ar�te, et les deux ont pour extr�mit� 3B, il n'est pas possible qu'il soit donc tous les deux dans le m�me couplage. *)

(* 7) Version 1: avec deux boucles while imbriqu�es, permettant de s'arr�ter d�s qu'il y a une erreur *)
  
let verifie g c =
  let n = Array.length g and b = ref true and i = ref 0 in
  while !i < n && !b do
    if c.(!i) = -1 then incr(i)		       (* on ne fait rien si c.(!i)=-1, on passe directement au suivant *)
    else if not(g.(!i).(c.(!i))) then b:=false (* l'ar�te n'existe pas *)
    else let j = ref (!i + 1) in	       (* on v�rifie qu'il n'y a pas une ar�te adjacente *)
	 while !j < n && !b do
	   if c.(!j) = c.(!i) then b := false
	   else incr(j)
	 done ;
    incr(i);
  done;
  !b ;;

let g = [| [| true; true; true; false |] ;
           [| false; false; false; true |] ;
           [| true; true; true; true |] ;
           [| false; false; false; true |] |] ;;

let c = [|0;3;2;-1|] ;;

verifie g c ;;

(* Complexit� : dans le pire des cas, la deuxi�me boucle while co�te (n-i) � chaque �tape, donc le co�t total est de l'ordre de la somme des entiers de 0 � n-1 donc quadratique en n (cela arrive si on a un couplage de taille n). *)

(* Version 2 : on utilise un tableau de bool�ens pour m�moriser les sommets d�j� utilis�s *)

let verifie g c =
  let n = Array.length g and b = ref true and i= ref 0 in
  let visites = Array.make n false in
  while !i < n && !b do
    if c.(!i) = -1 then incr(i)
    else if (not(g.(!i).(c.(!i))) || visites.(c.(!i))) then b:=false (* l'ar�te n'existe pas ou est adjacente � une autre *)
    else (visites.(c.(!i)) <- true; incr(i)) (* on marque le sommet visit� *)
  done;
  !b ;;

verifie g c ;;

(* Cette fois la complexit� est lin�aire en n gr�ce au tableau visites, qui induit un co�t spatial suppl�mentaire de l'ordre de n.  *)

(* 8) *)

let cardinal c =
  let n = Array.length c and k = ref 0 in
  for i = 0 to n-1 do
    if c.(i) <> - 1 then
      incr(k)
  done ;
  !k ;;

cardinal c ;;

(* Complexit� : il y a une seule boucle for de taille n et des op�rations de co�t constant � l'int�rieur, donc lin�aire en n *)

(* Deuxi�me partie : un algorithme pour d�terminer un couplage maximal *)

(* 9) En appliquant l'algorithme � G0, on voit qu'il y a deux ar�tes de poids minimal : {1,3} et {3,3}. On choisit par exemple {3,3} et on supprime les ar�tes adjacentes. Toutes les ar�tes restantes ont le m�me poids, on choisit par exemple {0,0}. Il ne reste que deux ar�tes de m�me poids, {2,1} et {2,2}. On choisit par exemple {2,1} et on obtient un couplage de taille 3 qui est bien maximal (et de plus de taille maximale).*)

(* 10) L'ar�te de poids minimal dans G1 est l'ar�te {3,2}, c'est elle qui sera donc s�lectionn�e. En la retirant ainsi que les ar�tes adjacentes on obtient une r�union disjointes de deux graphes bipartis que l'on appelle G2 et G3. L'algorithme va donc renvoyer un couplage maximal de G2 (de taille 2) et un maximal de G3 (de taille 2), donc on obtient un couplage maximal de G1 de taille 5. Mais celui-ci n'est pas de taille maximale, en effet le couplage form� des ar�tes (i,i) pour tout i entre 0 et 5 est de taille 6. *)

(* 11) *)

let arete_min g a =
  let n = Array.length g in
  let degA = Array.make n 0 and degB = Array.make n 0  in 		(* on construit deux tableaux contenant les degr�s des �l�ments de A et B*)
  for i = 0 to n-1 do
    let dA = ref 0 and dB = ref 0 in 			(* on calcule les degr�s de i_A et i_B *)
    for j = 0 to n-1 do
      if g.(i).(j)  then incr(dA) ; 	(* pour i_A on regarde la ligne i *)
      if g.(j).(i)  then incr(dB) ;	(* pour i_B on regarde la colonne i *)
    done;				
    degA.(i) <- !dA;
    degB.(i) <- !dB;
  done; 
  let mini = ref (2*n+1) and rep = ref false in 		(* on d�marre avec une valeur plus grande que toute somme possible, la variable rep contiendra true ssi il reste au moins une ar�te dans le graphe *)
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      if g.(i).(j) then
        (rep := true ;
	if !mini > (degA.(i) + degB.(j)) then
	  (a.(0) <- i; a.(1) <- j; mini := (degA.(i)+degB.(j))) )
    done;
  done;
  !rep ;;
(* on renvoie true ou false suivant s'il restait des ar�tes comme le demande l'�nonc� *)

(* Complexit� : quadratique pour construire les deux tableaux degA et degB, puis � nouveau quadratique pour chercher le min (dans les deux cas on a deux boucles for de taille n imbriqu�es), donc co�t total quadratique. Il y a en outre un co�t spatial lin�aire avec la cr�ation des deux tableaux degA et degB. *)
  
let a = [|0;0|] ;;
arete_min g a;;
a ;;

(* 12) *)

let supprimer g a =
  let i = a.(0) and j = a.(1) in
  for k = 0 to Array.length g -1 do
    if g.(i).(k) then
      g.(i).(k) <- false;
    if g.(k).(j) then
      g.(k).(j) <- false;
  done;; 
(* on ne renvoie rien, on modifie g *)

(* Complexit� lin�aire en n *)

(* 13) *)

(* L'�nonc� propose d'utiliser une fonction dupliquer_matrice pour cr�er une copie d'une matrice sans l'�crire, on donne ici un code possible de cette fonction, attention la fonction Array.copy ne marche pas sur les tableaux de dimension 2 (il faudrait l'appliquer � chaque ligne du tableau). *)

let dupliquer_matrice g =
  let n = Array.length g in
  let g2 = Array.make_matrix n n (g.(0).(0)) in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      g2.(i).(j) <- g.(i).(j)
    done;
  done;
  g2 ;;

let algo_approche g =
  let g2 = dupliquer_matrice g and n = Array.length g in
  let c = Array.make n (-1) and a = [|0;0|] and rep = ref true in (* le tableau c contiendra le couplage *)
  while !rep do
    rep := arete_min g2 a ;
    c.(a.(0)) <- a.(1) ;
    supprimer g2 a ; 			(* on agit sur la copie et pas sur g *)
  done;
  c ;;

 (* Complexit� : il y a au plus n �tapes dans le while car � chaque �tape on ajoute une ar�te, et par ce qui pr�c�de arete_min est quadratique et supprimer lin�aire, donc le co�t total est cubique. *)
  
algo_approche g ;;

(* Troisi�me partie : recherche exhaustive d'un couplage de cardinal maximum *)

(* 14) *)

let une_arete g a =
  let n = Array.length g in 
  let rep = ref false and i = ref 0 in
  while (not(!rep)) && (!i < n) do
    let j = ref 0 in
    while (not(!rep)) && (!j < n) do
      if g.(!i).(!j) then 
	(a.(0) <- !i ; a.(1) <- !j ; rep := true);
      incr(j);
    done;
  incr(i);
  done;
  !rep ;;

une_arete g a ;;
a ;;

(* 15) *)

let rec meilleur_couplage g =
  let n = Array.length g in
  let a = [|0;0|] and c = Array.make n (-1) in
    if not(une_arete g a) then c
    else 
     (let g1 = dupliquer_matrice g and g2 = dupliquer_matrice g in (* la copie g1 servira pour le cas "on ajoute l'ar�te a" et la copie g2 pour le cas "on ne l'ajoute pas" *)
      supprimer g1 a; 		(* on supprime l'ar�te a dans g1 et toutes les ar�tes adjacentes, car on a ajout� l'ar�te dans le couplage *)
      g2.(a.(0)).(a.(1)) <- false ;	(* dans g2 on ne supprime que l'ar�te a, on ne l'ajoute pas au couplage *)
      let c1 = meilleur_couplage g1 and c2 = meilleur_couplage g2 in
      if cardinal c2 > (cardinal c1 + 1) then c2 
      else
        (c1.(a.(0)) <- a.(1); c1) ) ;;
(* Remarque 1: comme dans l'appel r�cursif on recr�e un tableau rempli de -1, on ajoute l'ar�te dans c1 seulement � la fin dans le cas o� on conserve c1, et pour la comparaison on compare le cardinal de c2 avec le cardinal de 1 ajout� de 1 pour prendre en compte cette ar�te a. *)
(* Remarque 2 : une autre possibilit� aurait �t� de faire appel � une fonction auxiliaire r�cursive qui prend aussi en entr�e le couplage courant et renvoyer le nouveau couplage... *)
      
meilleur_couplage g ;;

(* Quatri�me partie : l'algorithme hongrois *)

(* 16) Le seul sommet de A non coupl� est le 2, donc c'est n�cessairement l'origine de la cha�ne. De m�me le seul sommet de B non coupl� est le 4 donc c'est l'extr�mit� de la cha�ne. On trouve la cha�ne 2A-2B-3A-3B-4A-4B. *)

(* 17) En partant d'une cha�ne altern�e augmentante relativement � C, on inverse le statut de ses ar�tes : celles appartenant � C sont sorties de C, et celles hors de C entrent dans C. On obtient bien un nouveau couplage car d'une part les deux extr�mit�s n'appartenaient pas au couplage d'origine, et d'autre part les sommets composant la cha�ne sont distincts et alternent entre A et B (et deux sommets cons�cutifs sont reli�s). De plus, par construction de la chaine, il y avait initialement une ar�te de plus n'appartenant pas � C donc lorsqu'on inverse, il y a une ar�te de plus dans C, on a bien augment� le cardinal de 1. *)

(* 18) En utilisant les marquages on reconstitue la cha�ne � l'envers, on obtient en la remettant � l'endroit la cha�ne 1A-0B-0A-1B-2A-2B-3A-3B. Il s'agit bien d'une cha�ne altern�e augmentante relativement � C1'. *)

(* 19) *)

let actualiser c r mA mB numero =
  let jB = ref numero and iA = ref (mB.(numero)) in
  c.(!iA) <- !jB ;
  r.(!jB) <- !iA ;
  while mA.(!iA) != -1 do
     jB := mA.(!iA) ;
     iA := mB.(!jB) ;
     c.(!iA) <- !jB ;
     r.(!jB) <- !iA ;
  done ;;
    
(* 20) Le seul sommet de d�part possible est 1A. On peut ensuite soit atteindre 3B soit 4B. Si on choisit 3B, on obtient la cha�ne 1A-3B-3A-4B-4A et on est bloqu�s, ce n'est pas une cha�ne augmentante (elle ne finit pas par un �l�ment de B). De m�me si on choisit 4B on obtient la cha�ne 1A-4B-4A-3B-3A qui n'est pas augmentante. *)

(* 21) La r�cursivit� crois�e est le fait de d�finir en m�me temps plusieurs fonctions r�cursives qui font appel les unes aux autres. Il faut utiliser le "and" entre chaque fonction pour les d�finir en Caml. *)

let rec chercherA g c r mA mB numero =
  let n = Array.length g and i = ref 0 and b = ref true and k = ref 0 in
  while !b && (!i < n) do 		    (* on cherche les voisins non marqu�s de numero, pour chacun d'eux on essaie de les explorer *)
    if g.(numero).(!i) && mB.(!i) = -1 then 	(* on a trouv� un voisin non marqu� *)
      (mB.(!i) <- numero;  		(* on marque le sommet trouv� *)
       let j = chercherB g c r mA mB (!i) in (* on lance chercherB pour tenter l'exploration depuis i *)
       if j != -1 then 			(* si l'exploration est satisfaisante, on arr�te l� et on sauvegarde le r�sultat dans k*)
	 (b := false ;
          k := j)
       else incr(i)) 			(* sinon on passe au i suivant *)
    else incr(i)
  done;
  if !i = n then -1 else !k
and chercherB g c r mA mB numero =
  if r.(numero) = -1 then numero 	(* on a trouv� un sommet non coupl� de B, on le renvoie *)
  else let i = r.(numero) in 		(* sinon on n'a pas le choix on doit suivre l'ar�te issue de numero dans le couplage *)
       (mA.(i) <- numero;
       chercherA g c r mA mB i) ;;

(* 22) Attention � chaque �tape il faut remettre des -1 dans les tableaux mA et mB.*)

let chaine_alternee g c r mA mB =
  let n = Array.length g and i = ref 0 and rep = ref (-1) in
  while (!i < n) && (!rep = -1) do 	(* i parcourt les sommets de A et rep contiendra le r�sultat de chercherA *)
    if c.(!i) = -1 then 			(* on ne part que des sommets non coupl�s de A *)
      (for k = 0 to n-1 do mA.(k) <- -1; mB.(k) <- -1; done;
      rep := chercherA g c r mA mB (!i));
    incr(i)
  done;
  !rep ;;

(* 23) *)

let algorithme_hongrois g =
  let n = Array.length g in
  let c = Array.make n (-1) and r = Array.make n (-1) in
  let b = ref true in
  while !b do
    let  mA = Array.make n (-1) and mB = Array.make n (-1) in
    let numero = (chaine_alternee g c r mA mB) in
    if numero != -1 then
       actualiser c r mA mB (numero)
    else 
       b := false
  done;
  c ;;
  
algorithme_hongrois g ;;

let g1 = [| [| true; true; false; false; false; false |] ;
            [| true; true; false; false; false; false |] ;
            [| true; true; true; false; false; false |] ;
            [| false; false; true; true; false; false |] ;
            [| false; false; false; true; true; true |] ;
            [| false; false; false; true; true; true |] |] ;;

algorithme_hongrois g1 ;;


let g2 = [| [| true; true; true; false; false |] ;
            [| false; false; false; true; true |] ;
            [| true; true; true; true; false|] ;
            [| false; false; false; true; true |] ;
            [| false; false; false; true; true |]  |] ;;

algorithme_hongrois g2 ;;
