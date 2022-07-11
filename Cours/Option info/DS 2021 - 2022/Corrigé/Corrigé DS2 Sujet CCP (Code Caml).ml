(* CCP 2018 *)

(* Partie I : Logique *)

(* 1�re �preuve *)

(* 1. B1 = P1 OU P2 *)

(* 2. B2 = NON(P1) *)

(* 3. La formule ((B1 ET B2) OU (NON(B1) ET NON(B2))) est �quivalente apr�s calcul � (P2 ET NON(P1)). *)

(* On doit choisir la bo�te 2. *)

(* 2e �preuve *)

(* 1. B1 = NON(P1) OU P2
      B2 = P1 *)

(* 2. Cette fois on obtient la formule (P2 ET P1), donc les 2 bo�tes contiennent une cl� verte, on peut choisir n'importe laquelle. *)

(* 3e �preuve : voir corrig� joint *)

(* Partie II : Automates *)

(* 1. D�finitions *)

(* 11. a^{-1}L = {b , ab}. *)

(* 12. On montre facilement que c'est une relation d'�quivalence (r�flexive, sym�trique, transitive). Pour la 2e partie, on montre que si u^{-1}L = v^{-1}L, alors
(uw)^{-1}L = {m | wm in u^{-1}L} = (vw)^{-1}L. *)

(* 13. (i) b appartient � L donc le mot vide appartient � b^{-1}L. Mais ab n'appartient pas � L donc le mot vide n'appartien pas � (ab)^{-1}L, par cons�quent les mots ne sont pas �quivalents.

(ii) Les mots ne sont pas �quivalents car a appartient � (aba)^{-1}L mais pas � (bab)^{-1}L. 

(iii) Les mots sont �quivalents, leurs r�siduels sont �gaux � L. *)

(* 14. Il faut montrer que L poss�de un nombre fini de r�siduels. Comme L est r�gulier, il est reconnu par un automate fini L, notons Q son nombre d'�tats.
On d�finit l'application phi de Q dans Q_L qui � un �tat q associe q^{-1}L. D'apr�s  un r�sultat admis par l'�nonc�, c'est bien un r�siduel de la forme u^{-1}L o� u est mot tel que delta*(q0,u)=q (un tel u existe car l'automate minimal est accessible, c'est admis par l'�nonc� apr�s la d�finition 7). De plus phi est surjective : si u est un mot, on consid�re l'�tat q=delta*(q0,u). Alors u^{-1}L= q^{-1}L = phi(q). Comme l'ensemble de d�part Q est fini et phi est surjective, l'ensemble d'arriv�e Q_L est �galement fini.  *)

(* 2. Construction de l'automate minimal *)

(* 15. Le mot vide appartient � p^{-1}L mais pas � q^{-1]L donc la paire (p,q) est distingu�e. *)

(* 16. On raisonne par l'absurde : on suppose que Ni est vide et Nj est non vide avec j>i, on prend une paire (p,q) distingu�e par un mot m de longueur j. On s�pare ce mot en un mot v de longueur j-i et un mot w de longueur i, et on note (p',q') les �tats atteints depuis (p,q) en lisant le mot v. Alors cette paire est distingu�e par le mot w de longueur i, donc appartient � Ni ce qui est absurde.  *)

(* 17-18-19. Voir corrig� joint. *)


(* Partie III : Algorithmique et programmation *)

(* III.1. Transformation de Burrows-Wheeler *)

(* 20. Il s'agit de la matrice dont la premi�re ligne est ['t','u','r','l','u','t','u','t','u','|'] (ne pas oublier le symbole de fin '|'), la deuxi�me ['|','t','u','r','l','u','t','u','t','u'], la troisi�me ['u','|','t','u','r','l','u','t','u','t'] etc... (il y a 10 lignes en tout) *)

(* 21. Attention on veut une fonction r�cursive ! L'astuce consiste � remarquer que si permute(q) est la permutation circulaire de q, pour obtenir permute(t::q) il suffit d'�changer t et la t�te de permute(q)... *)

let rec circulaire l =
  match l with
    |[] -> []
    |[x] -> [x]
    |t::q -> let l1 = circulaire q in (List.hd l1)::t::(List.tl l1) ;;

(* 22. Remarque : ici on repr�sente une matrice par une liste de listes et non un array de dimension 2 *)

let matrice_mot l =
  let n = List.length l in
  let mat = ref [l] and mot = ref l in 	(* mat est la matrice qui sera renvoy�e, mot sert � �num�rer les d�calages successifs *)
  for i = 1 to n-1 do
    mot := circulaire (!mot) ;
    mat := (!mot)::(!mat);
  done;
  !mat ;;

(* 23. On obtient la permutation (1 2 3 4 5 6 7 8  9 10)
                                 (2 8 9 4 1 6 3 10 5 4 ) *)

(* 24. Tri insertion, c'est du cours ! On commence par �crire une fonction auxiliaire qui ins�re un �l�ment dans une liste d�j� tri�e *)

let rec insere x l =
  match l with
    |[] -> [x]
    |t::q when x <= t -> x::t::q
    |t::q -> t::(insere x q) ;;

let rec tri l =
  match l with
    |[] -> []
    |t::q -> insere t (tri q) ;;

(* 25. Remarque : Caml sait comparer les caract�res dans l'ordre alphab�tique avec <=, de m�me il sait comparer les listes dans l'ordre lexicographique avec le m�me op�rateur. Il y a une erreur d'�nonc� : pour correspondre au typage annonc� il faut prendre en entr�e le mot et non la matrice M. *)

let matrice_mot_triee mot =
  tri (matrice_mot mot) ;;

(* 26. Dans le pire des cas, pour comparer deux permutations circulaires, on doit aller jusqu'� la fin des mots donc complexit� lin�aire en la taille des mots (les deux mots font la m�me taille vu que ce sont deux permutations circulaires). *)

(* 27. Chaque insertion d'un mot de taille k dans une liste de taille p co�te O(pk) car dans le pire des cas on parcourt toute la liste. Par cons�quent la complexit� dans le pire des cas du tri est O((somme des p) * k) = O(k(k+1)/2 * k) = O(k^3). *)

(* 28. Comme indiqu� par l'�nonc� on �crit d'abord une fonction r�cursive permettant de r�cup�rer le dernier �l�ment d'une liste. *)

let rec dernier l =
  match l with
    |[] -> failwith "liste vide"
    |[x] -> x
    |t::q -> dernier q ;;

let codageBWT mot =
  let mot2 = mot@['0'] in 			(* on ajoute le symbole de fin, je ne sais pas si l'�nonc� suppose qu'il a d�j� �t� ajout�, j'ai pris '0' � la place de '|' car avec l'op�rateur <=, le caract�re '|' n'est pas inf�rieur aux lettres alors que '0' si *)
  let mat = matrice_mot_triee mot2 in
  let rec parcours mat = 		(* on �crit une fonction auxiliaire qui parcourt mat pour r�cup�rer � chaque fois le dernier symbole *)
    match mat with 
      |[] -> []
      |l::q -> (dernier l)::(parcours q)
  in parcours mat ;;

(* Le codage de 'turlututu' est 'uruu|utttl'. *)

(* 29. Il suffit de trier les lettres de la derni�re colonne, on obtient "|adeegnnv". *)

(* 30. La derni�re et la premi�re colonne de M' nous donnent les facteurs de longueur 2 de mu : il s'agit pour l'exemple de [ e|, da, nd, ge, ve, ng, en, an, |v ]. Il suffit de les ordonner dans l'ordre lexicographique pour obtenir la 2e colonne :[ |v, an, da, e|, en, ge, nd, ng, ve ]. La deuxi�me colonne est donc : 'vna|nedge'.  *)

(* 31. On suit le m�me principe : construit les facteurs de longueur n du mot en pla�ant la derni�re colonne devant les (n-1) premi�res, puis on ordonne ces facteurs, la n�me colonne sera form�e des derni�res lettres des facteurs tri�s. *)

(* 32. On reconstitue la matrice M' en suivant le principe ci-dessus pour former une � une ses colonnes. A la fin, les lignes de la matrice M' forment toutes les permutations circulaires du mot, il suffit de prendre celle qui se termine par "|". On voit ici l'int�r�t d'avoir introduit ce symbole de fin ! *)

(* 33. On obtient le mot "vendange" ! *)
