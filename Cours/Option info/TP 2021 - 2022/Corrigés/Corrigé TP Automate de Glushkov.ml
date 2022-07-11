(* TP algorithme de Berry-Sethi *)

type automate = {n : int ;  i : int ; t : int list ; gamma : (int*char*int) list };;

type exprat =
  | Lettre of char
  | Lettre_n of char*int 		(* pour l'expression linéarisée *)
  | Somme of exprat * exprat
  | Concat of exprat * exprat
  | Etoile of exprat ;;

(* Fonction de linéarisation d'une exp rat *)

let rec lineariser exp compt = match exp with
  | Lettre(a) -> Lettre_n(a, compt), compt+1
  | Lettre_n(a,i) -> failwith "expression avec des lettres chiffrées"
  | Somme(e1,e2) -> let e3,c = lineariser e1 compt in
		    let e4,c2 = lineariser e2 c in
		    Somme(e3,e4), c2
  | Concat(e1,e2) -> let e3,c = lineariser e1 compt in
		    let e4,c2 = lineariser e2 c in
		    Concat(e3,e4), c2
  | Etoile(e) -> let e1,c = lineariser e compt in Etoile(e1),c ;;

let linearise e = fst (lineariser e 1) ;;

let e = Concat(Etoile(Somme(Lettre('a'), Lettre('b'))) , Lettre('c'));;

linearise e;;

(* Fonction qui détermine si le mot vide est dans le langage d'une exp rat *)

let rec motvide exp = match exp with 
  | Lettre(a) -> false
  | Lettre_n(a,i) -> false
  | Somme(e1, e2) -> motvide(e1) || motvide(e2)
  | Concat(e1, e2) -> motvide(e2) && motvide(e2)
  | Etoile(e) -> true ;;


(* Fonction qui détermine l'ensemble des préfixes de longueur 1 P *)

let rec prefixe exp  = match exp with
  | Lettre(a) -> failwith "expression non linéarisée"
  | Lettre_n(a,i) -> [a,i]
  | Somme(e1, e2) -> (prefixe e1)@(prefixe e2)
  | Concat(e1, e2) when motvide e1 -> (prefixe e1)@(prefixe e2)
  | Concat(e1, e2) -> prefixe e1
  | Etoile(e) -> prefixe e ;; 

(* Fonction qui détermine l'ensemble des suffixes de longueur 1 S *)

let rec suffixe exp = match exp with
  | Lettre(a) -> failwith "expression non linéarisée"
  | Lettre_n(a,i) -> [a,i]
  | Somme(e1, e2) -> (suffixe e1)@(suffixe e2)
  | Concat(e1, e2) when motvide e2 -> (suffixe e1)@(suffixe e2)
  | Concat(e1, e2) -> suffixe e2
  | Etoile(e) -> suffixe e;;

(* Fonction qui réalise le produit cartésien de deux listes de chaînes de caractères *)

let rec produit l1 l2 =
  let rec distribue x l = 		(* distribue un élément x sur une liste l *)
    match l with
      |[] -> []
      |a::q -> (x,a)::(distribue x q)
  in match (l1, l2) with
  | _, [] -> []
  | [], _ -> []
  | x::q, l2 ->(distribue x l2)@(produit q l2) ;; 

(* Fonction qui détermine l'ensemble des facteurs de longueur 2 F *)

let rec facteur exp = match exp with
  | Lettre(a) -> failwith "expression non linéarisée"
  | Lettre_n(a,i) -> []
  | Somme(e1, e2) -> (facteur e1)@(facteur e2)
  | Concat(e1, e2) -> let l = (facteur e1)@(facteur e2) 
		      in l@(produit (suffixe e1) (prefixe e2))
  | Etoile(e) -> (facteur e)@(produit (suffixe e) (prefixe e)) ;;


prefixe (linearise e);;

suffixe (linearise e);;

facteur (linearise e);;

(* Construction de l'automate de Glushkov *)
(* Les états sont numérotés par les lettres de la linéarisation, l'état initial est 0 *)

(* Fonction qui détermine les états terminaux à partir de la liste S *)

let rec terminaux s = match s with
  |[] -> []
  |(c,n)::q -> n::terminaux q;;

(* Fonction qui détermine les transitions initiales à partir de la liste P *)

let rec transitions_initiales p = match p with
  |[] -> []
  |(c,n)::q -> (0,c,n)::transitions_initiales q;;

(* Fonction qui détermine les autres transitions à partir de la liste F *)

let rec transitions f = match f with
  |[] -> []
  |((c1,n1),(c2,n2))::q -> (n1,c2,n2)::transitions q;;

(* Fonction qui réalise l'automate de Glushkov *)

let glushkov e =
  let el, c = (lineariser e 1) in
  let p = prefixe el and s = suffixe el and f = facteur el in
  let term = if (motvide el) then 0::terminaux s else terminaux s in
  let trans = (transitions_initiales p)@(transitions f) in
  {n = c ; i = 0 ; t = term ; gamma = trans};;

glushkov e ;;

let e2 = Concat(Concat(Etoile(Somme(Concat(Lettre('a'),Lettre('b')),Lettre('b'))),Lettre('b')),Lettre('a'));;

glushkov e2 ;;
