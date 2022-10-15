(* CCP 2018 *)

(* Partie I : Logique *)

(* 1ère épreuve *)

(* 1. B1 = P1 OU P2 *)

(* 2. B2 = NON(P1) *)

(* 3. La formule ((B1 ET B2) OU (NON(B1) ET NON(B2))) est équivalente après calcul à (P2 ET NON(P1)). *)

(* On doit choisir la boîte 2. *)

(* 2e épreuve *)

(* 1. B1 = NON(P1) OU P2
      B2 = P1 *)

(* 2. Cette fois on obtient la formule (P2 ET P1), donc les 2 boîtes contiennent une clé verte, on peut choisir n'importe laquelle. *)

(* 3e épreuve : voir corrigé joint *)

(* Partie II : Automates *)

(* 1. Définitions *)

(* 11. a^{-1}L = {b , ab}. *)

(* 12. On montre facilement que c'est une relation d'équivalence (réflexive, symétrique, transitive). Pour la 2e partie, on montre que si u^{-1}L = v^{-1}L, alors
(uw)^{-1}L = {m | wm in u^{-1}L} = (vw)^{-1}L. *)

(* 13. (i) b appartient à L donc le mot vide appartient à b^{-1}L. Mais ab n'appartient pas à L donc le mot vide n'appartien pas à (ab)^{-1}L, par conséquent les mots ne sont pas équivalents.

(ii) Les mots ne sont pas équivalents car a appartient à (aba)^{-1}L mais pas à (bab)^{-1}L. 

(iii) Les mots sont équivalents, leurs résiduels sont égaux à L. *)

(* 14. Il faut montrer que L possède un nombre fini de résiduels. Comme L est régulier, il est reconnu par un automate fini L, notons Q son nombre d'états.
On définit l'application phi de Q dans Q_L qui à un état q associe q^{-1}L. D'après  un résultat admis par l'énoncé, c'est bien un résiduel de la forme u^{-1}L où u est mot tel que delta*(q0,u)=q (un tel u existe car l'automate minimal est accessible, c'est admis par l'énoncé après la définition 7). De plus phi est surjective : si u est un mot, on considère l'état q=delta*(q0,u). Alors u^{-1}L= q^{-1}L = phi(q). Comme l'ensemble de départ Q est fini et phi est surjective, l'ensemble d'arrivée Q_L est également fini.  *)

(* 2. Construction de l'automate minimal *)

(* 15. Le mot vide appartient à p^{-1}L mais pas à q^{-1]L donc la paire (p,q) est distinguée. *)

(* 16. On raisonne par l'absurde : on suppose que Ni est vide et Nj est non vide avec j>i, on prend une paire (p,q) distinguée par un mot m de longueur j. On sépare ce mot en un mot v de longueur j-i et un mot w de longueur i, et on note (p',q') les états atteints depuis (p,q) en lisant le mot v. Alors cette paire est distinguée par le mot w de longueur i, donc appartient à Ni ce qui est absurde.  *)

(* 17-18-19. Voir corrigé joint. *)


(* Partie III : Algorithmique et programmation *)

(* III.1. Transformation de Burrows-Wheeler *)

(* 20. Il s'agit de la matrice dont la première ligne est ['t','u','r','l','u','t','u','t','u','|'] (ne pas oublier le symbole de fin '|'), la deuxième ['|','t','u','r','l','u','t','u','t','u'], la troisième ['u','|','t','u','r','l','u','t','u','t'] etc... (il y a 10 lignes en tout) *)

(* 21. Attention on veut une fonction récursive ! L'astuce consiste à remarquer que si permute(q) est la permutation circulaire de q, pour obtenir permute(t::q) il suffit d'échanger t et la tête de permute(q)... *)

let rec circulaire l =
  match l with
    |[] -> []
    |[x] -> [x]
    |t::q -> let l1 = circulaire q in (List.hd l1)::t::(List.tl l1) ;;

(* 22. Remarque : ici on représente une matrice par une liste de listes et non un array de dimension 2 *)

let matrice_mot l =
  let n = List.length l in
  let mat = ref [l] and mot = ref l in 	(* mat est la matrice qui sera renvoyée, mot sert à énumérer les décalages successifs *)
  for i = 1 to n-1 do
    mot := circulaire (!mot) ;
    mat := (!mot)::(!mat);
  done;
  !mat ;;

(* 23. On obtient la permutation (1 2 3 4 5 6 7 8  9 10)
                                 (2 8 9 4 1 6 3 10 5 4 ) *)

(* 24. Tri insertion, c'est du cours ! On commence par écrire une fonction auxiliaire qui insère un élément dans une liste déjà triée *)

let rec insere x l =
  match l with
    |[] -> [x]
    |t::q when x <= t -> x::t::q
    |t::q -> t::(insere x q) ;;

let rec tri l =
  match l with
    |[] -> []
    |t::q -> insere t (tri q) ;;

(* 25. Remarque : Caml sait comparer les caractères dans l'ordre alphabétique avec <=, de même il sait comparer les listes dans l'ordre lexicographique avec le même opérateur. Il y a une erreur d'énoncé : pour correspondre au typage annoncé il faut prendre en entrée le mot et non la matrice M. *)

let matrice_mot_triee mot =
  tri (matrice_mot mot) ;;

(* 26. Dans le pire des cas, pour comparer deux permutations circulaires, on doit aller jusqu'à la fin des mots donc complexité linéaire en la taille des mots (les deux mots font la même taille vu que ce sont deux permutations circulaires). *)

(* 27. Chaque insertion d'un mot de taille k dans une liste de taille p coûte O(pk) car dans le pire des cas on parcourt toute la liste. Par conséquent la complexité dans le pire des cas du tri est O((somme des p) * k) = O(k(k+1)/2 * k) = O(k^3). *)

(* 28. Comme indiqué par l'énoncé on écrit d'abord une fonction récursive permettant de récupérer le dernier élément d'une liste. *)

let rec dernier l =
  match l with
    |[] -> failwith "liste vide"
    |[x] -> x
    |t::q -> dernier q ;;

let codageBWT mot =
  let mot2 = mot@['0'] in 			(* on ajoute le symbole de fin, je ne sais pas si l'énoncé suppose qu'il a déjà été ajouté, j'ai pris '0' à la place de '|' car avec l'opérateur <=, le caractère '|' n'est pas inférieur aux lettres alors que '0' si *)
  let mat = matrice_mot_triee mot2 in
  let rec parcours mat = 		(* on écrit une fonction auxiliaire qui parcourt mat pour récupérer à chaque fois le dernier symbole *)
    match mat with 
      |[] -> []
      |l::q -> (dernier l)::(parcours q)
  in parcours mat ;;

(* Le codage de 'turlututu' est 'uruu|utttl'. *)

(* 29. Il suffit de trier les lettres de la dernière colonne, on obtient "|adeegnnv". *)

(* 30. La dernière et la première colonne de M' nous donnent les facteurs de longueur 2 de mu : il s'agit pour l'exemple de [ e|, da, nd, ge, ve, ng, en, an, |v ]. Il suffit de les ordonner dans l'ordre lexicographique pour obtenir la 2e colonne :[ |v, an, da, e|, en, ge, nd, ng, ve ]. La deuxième colonne est donc : 'vna|nedge'.  *)

(* 31. On suit le même principe : construit les facteurs de longueur n du mot en plaçant la dernière colonne devant les (n-1) premières, puis on ordonne ces facteurs, la nème colonne sera formée des dernières lettres des facteurs triés. *)

(* 32. On reconstitue la matrice M' en suivant le principe ci-dessus pour former une à une ses colonnes. A la fin, les lignes de la matrice M' forment toutes les permutations circulaires du mot, il suffit de prendre celle qui se termine par "|". On voit ici l'intérêt d'avoir introduit ce symbole de fin ! *)

(* 33. On obtient le mot "vendange" ! *)
