(* Mines-Ponts 2012 *)

(* Exercice sur les langages rationnels *)

(* On note l'alphabet A au lieu de Sigma *)

(* 1) L1 est décrit par l'expression rationnelle A*.f.A* donc est rationnel. *)

(* 2) De même, L2 est décrit par l'expression rationnelle A*.f.A*.f.A* donc est rationnel. *)

(* 3) Il nous faut décrire le fait d'avoir deux occurrences imbriquées de f. Notons Lp et Ls les langages des mots ayant respectivement f comme préfixe et comme suffixe (ils sont rationnels car décrits par les expressions rationnelles f.A* et A*.f). Le langage des mots ayant à la fois f comme préfixe et comme suffixe est l'intersection de Lp et Ls, il est rationnel car l'intersection de deux langages rationnels est rationnelle (résultat du cours). Si l'on veut exclure les mots dont les occurrences sont disjointes, il faut éliminer le mot f et les mots de L2. On obtient un langage Lps qui est toujours rationnel car l'ensemble des langages rationnels est stable par complémentation. Enfin, le langage L3 est égal à A*.L3.A* donc est rationnel. *)

(* 4) L4 = L1 \ (L2 + L3) donc est rationnel par les résultats de clôture du cours. *)

(* Problème d'algorithmique et programmation *)

(* Première partie : généralités *)

(* 5) Les arêtes {0,0}, {1,3} et {2,2} forment un couplage de cardinal 3 dans G0. *)

(* 6) Il n'y a pas de couplage de cardinal 4 dans G0, en effet si tel était le cas, chaque sommet apparaîtrait dans ce couplage. Or 1A et 3A ne sont reliés qu'à une seule arête, et les deux ont pour extrémité 3B, il n'est pas possible qu'il soit donc tous les deux dans le même couplage. *)

(* 7) Version 1: avec deux boucles while imbriquées, permettant de s'arrêter dès qu'il y a une erreur *)
  
let verifie g c =
  let n = Array.length g and b = ref true and i = ref 0 in
  while !i < n && !b do
    if c.(!i) = -1 then incr(i)		       (* on ne fait rien si c.(!i)=-1, on passe directement au suivant *)
    else if not(g.(!i).(c.(!i))) then b:=false (* l'arête n'existe pas *)
    else let j = ref (!i + 1) in	       (* on vérifie qu'il n'y a pas une arête adjacente *)
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

(* Complexité : dans le pire des cas, la deuxième boucle while coûte (n-i) à chaque étape, donc le coût total est de l'ordre de la somme des entiers de 0 à n-1 donc quadratique en n (cela arrive si on a un couplage de taille n). *)

(* Version 2 : on utilise un tableau de booléens pour mémoriser les sommets déjà utilisés *)

let verifie g c =
  let n = Array.length g and b = ref true and i= ref 0 in
  let visites = Array.make n false in
  while !i < n && !b do
    if c.(!i) = -1 then incr(i)
    else if (not(g.(!i).(c.(!i))) || visites.(c.(!i))) then b:=false (* l'arête n'existe pas ou est adjacente à une autre *)
    else (visites.(c.(!i)) <- true; incr(i)) (* on marque le sommet visité *)
  done;
  !b ;;

verifie g c ;;

(* Cette fois la complexité est linéaire en n grâce au tableau visites, qui induit un coût spatial supplémentaire de l'ordre de n.  *)

(* 8) *)

let cardinal c =
  let n = Array.length c and k = ref 0 in
  for i = 0 to n-1 do
    if c.(i) <> - 1 then
      incr(k)
  done ;
  !k ;;

cardinal c ;;

(* Complexité : il y a une seule boucle for de taille n et des opérations de coût constant à l'intérieur, donc linéaire en n *)

(* Deuxième partie : un algorithme pour déterminer un couplage maximal *)

(* 9) En appliquant l'algorithme à G0, on voit qu'il y a deux arêtes de poids minimal : {1,3} et {3,3}. On choisit par exemple {3,3} et on supprime les arêtes adjacentes. Toutes les arêtes restantes ont le même poids, on choisit par exemple {0,0}. Il ne reste que deux arêtes de même poids, {2,1} et {2,2}. On choisit par exemple {2,1} et on obtient un couplage de taille 3 qui est bien maximal (et de plus de taille maximale).*)

(* 10) L'arête de poids minimal dans G1 est l'arête {3,2}, c'est elle qui sera donc sélectionnée. En la retirant ainsi que les arêtes adjacentes on obtient une réunion disjointes de deux graphes bipartis que l'on appelle G2 et G3. L'algorithme va donc renvoyer un couplage maximal de G2 (de taille 2) et un maximal de G3 (de taille 2), donc on obtient un couplage maximal de G1 de taille 5. Mais celui-ci n'est pas de taille maximale, en effet le couplage formé des arêtes (i,i) pour tout i entre 0 et 5 est de taille 6. *)

(* 11) *)

let arete_min g a =
  let n = Array.length g in
  let degA = Array.make n 0 and degB = Array.make n 0  in 		(* on construit deux tableaux contenant les degrés des éléments de A et B*)
  for i = 0 to n-1 do
    let dA = ref 0 and dB = ref 0 in 			(* on calcule les degrés de i_A et i_B *)
    for j = 0 to n-1 do
      if g.(i).(j)  then incr(dA) ; 	(* pour i_A on regarde la ligne i *)
      if g.(j).(i)  then incr(dB) ;	(* pour i_B on regarde la colonne i *)
    done;				
    degA.(i) <- !dA;
    degB.(i) <- !dB;
  done; 
  let mini = ref (2*n+1) and rep = ref false in 		(* on démarre avec une valeur plus grande que toute somme possible, la variable rep contiendra true ssi il reste au moins une arête dans le graphe *)
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      if g.(i).(j) then
        (rep := true ;
	if !mini > (degA.(i) + degB.(j)) then
	  (a.(0) <- i; a.(1) <- j; mini := (degA.(i)+degB.(j))) )
    done;
  done;
  !rep ;;
(* on renvoie true ou false suivant s'il restait des arêtes comme le demande l'énoncé *)

(* Complexité : quadratique pour construire les deux tableaux degA et degB, puis à nouveau quadratique pour chercher le min (dans les deux cas on a deux boucles for de taille n imbriquées), donc coût total quadratique. Il y a en outre un coût spatial linéaire avec la création des deux tableaux degA et degB. *)
  
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

(* Complexité linéaire en n *)

(* 13) *)

(* L'énoncé propose d'utiliser une fonction dupliquer_matrice pour créer une copie d'une matrice sans l'écrire, on donne ici un code possible de cette fonction, attention la fonction Array.copy ne marche pas sur les tableaux de dimension 2 (il faudrait l'appliquer à chaque ligne du tableau). *)

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

 (* Complexité : il y a au plus n étapes dans le while car à chaque étape on ajoute une arête, et par ce qui précède arete_min est quadratique et supprimer linéaire, donc le coût total est cubique. *)
  
algo_approche g ;;

(* Troisième partie : recherche exhaustive d'un couplage de cardinal maximum *)

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
     (let g1 = dupliquer_matrice g and g2 = dupliquer_matrice g in (* la copie g1 servira pour le cas "on ajoute l'arête a" et la copie g2 pour le cas "on ne l'ajoute pas" *)
      supprimer g1 a; 		(* on supprime l'arête a dans g1 et toutes les arêtes adjacentes, car on a ajouté l'arête dans le couplage *)
      g2.(a.(0)).(a.(1)) <- false ;	(* dans g2 on ne supprime que l'arête a, on ne l'ajoute pas au couplage *)
      let c1 = meilleur_couplage g1 and c2 = meilleur_couplage g2 in
      if cardinal c2 > (cardinal c1 + 1) then c2 
      else
        (c1.(a.(0)) <- a.(1); c1) ) ;;
(* Remarque 1: comme dans l'appel récursif on recrée un tableau rempli de -1, on ajoute l'arête dans c1 seulement à la fin dans le cas où on conserve c1, et pour la comparaison on compare le cardinal de c2 avec le cardinal de 1 ajouté de 1 pour prendre en compte cette arête a. *)
(* Remarque 2 : une autre possibilité aurait été de faire appel à une fonction auxiliaire récursive qui prend aussi en entrée le couplage courant et renvoyer le nouveau couplage... *)
      
meilleur_couplage g ;;

(* Quatrième partie : l'algorithme hongrois *)

(* 16) Le seul sommet de A non couplé est le 2, donc c'est nécessairement l'origine de la chaîne. De même le seul sommet de B non couplé est le 4 donc c'est l'extrémité de la chaîne. On trouve la chaîne 2A-2B-3A-3B-4A-4B. *)

(* 17) En partant d'une chaîne alternée augmentante relativement à C, on inverse le statut de ses arêtes : celles appartenant à C sont sorties de C, et celles hors de C entrent dans C. On obtient bien un nouveau couplage car d'une part les deux extrémités n'appartenaient pas au couplage d'origine, et d'autre part les sommets composant la chaîne sont distincts et alternent entre A et B (et deux sommets consécutifs sont reliés). De plus, par construction de la chaine, il y avait initialement une arête de plus n'appartenant pas à C donc lorsqu'on inverse, il y a une arête de plus dans C, on a bien augmenté le cardinal de 1. *)

(* 18) En utilisant les marquages on reconstitue la chaîne à l'envers, on obtient en la remettant à l'endroit la chaîne 1A-0B-0A-1B-2A-2B-3A-3B. Il s'agit bien d'une chaîne alternée augmentante relativement à C1'. *)

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
    
(* 20) Le seul sommet de départ possible est 1A. On peut ensuite soit atteindre 3B soit 4B. Si on choisit 3B, on obtient la chaîne 1A-3B-3A-4B-4A et on est bloqués, ce n'est pas une chaîne augmentante (elle ne finit pas par un élément de B). De même si on choisit 4B on obtient la chaîne 1A-4B-4A-3B-3A qui n'est pas augmentante. *)

(* 21) La récursivité croisée est le fait de définir en même temps plusieurs fonctions récursives qui font appel les unes aux autres. Il faut utiliser le "and" entre chaque fonction pour les définir en Caml. *)

let rec chercherA g c r mA mB numero =
  let n = Array.length g and i = ref 0 and b = ref true and k = ref 0 in
  while !b && (!i < n) do 		    (* on cherche les voisins non marqués de numero, pour chacun d'eux on essaie de les explorer *)
    if g.(numero).(!i) && mB.(!i) = -1 then 	(* on a trouvé un voisin non marqué *)
      (mB.(!i) <- numero;  		(* on marque le sommet trouvé *)
       let j = chercherB g c r mA mB (!i) in (* on lance chercherB pour tenter l'exploration depuis i *)
       if j != -1 then 			(* si l'exploration est satisfaisante, on arrête là et on sauvegarde le résultat dans k*)
	 (b := false ;
          k := j)
       else incr(i)) 			(* sinon on passe au i suivant *)
    else incr(i)
  done;
  if !i = n then -1 else !k
and chercherB g c r mA mB numero =
  if r.(numero) = -1 then numero 	(* on a trouvé un sommet non couplé de B, on le renvoie *)
  else let i = r.(numero) in 		(* sinon on n'a pas le choix on doit suivre l'arête issue de numero dans le couplage *)
       (mA.(i) <- numero;
       chercherA g c r mA mB i) ;;

(* 22) Attention à chaque étape il faut remettre des -1 dans les tableaux mA et mB.*)

let chaine_alternee g c r mA mB =
  let n = Array.length g and i = ref 0 and rep = ref (-1) in
  while (!i < n) && (!rep = -1) do 	(* i parcourt les sommets de A et rep contiendra le résultat de chercherA *)
    if c.(!i) = -1 then 			(* on ne part que des sommets non couplés de A *)
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
