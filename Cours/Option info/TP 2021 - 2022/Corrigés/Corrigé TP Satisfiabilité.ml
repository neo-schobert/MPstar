(* TP Satisfiabilité *)

(* Formules logiques *)

(* Types connecteurs *)

type connecteur1 = NON	;;	(* connecteur unaire *)

type connecteur2 = ET | OU | ALORS ;;  (* connecteurs binaires *)

(* Type Expression booléenne *)

type expression = Vrai | Faux 
		  | Variable of int
		  | Noeud1 of connecteur1 * expression
		  | Noeud2 of connecteur2 * expression * expression ;; 

(* Exemples *)

let f1 = Noeud2(ALORS, Variable(0), Variable(1));; (* F1 = Si V0 Alors V1 *)

let f2 = Noeud2(ALORS, Noeud2(ET, Variable(0), Variable(1)), Variable(2));; (* F2 = Si (V0 Et V1) Alors V2 *)

let f3 = Noeud2(ALORS, Variable(0), Noeud1(NON,Variable(1)));; (* F3 = Si V0 Alors (Non V1) *)

let f4 = Noeud2(ET, Variable(0), Noeud1(NON,Variable(0)));; (* F4 = V0 Et (Non V0) *)


(* Evaluation d'une instanciation *)

let rec eval inst f = match f with
  | Vrai -> true
  | Faux -> false
  | Variable(i) -> inst.(i)
  | Noeud1(NON, e) -> not (eval inst e)
  | Noeud2(ET, e1, e2) -> (eval inst e1) && (eval inst e2)
  | Noeud2(OU, e1, e2) -> (eval inst e1) || (eval inst e2)
  | Noeud2(ALORS, e1, e2) -> (not (eval inst e1)) || (eval inst e2) ;;

let inst = [| true ; false ; false |];;

eval inst f1;;

eval inst f2;;

eval inst f3;;

eval inst f4;;

(* Satisfiabilité *)

(* Numéro de variable maximal *)

let rec variable_max f = match f with		(* s'il n'y a pas de variable, on renvoie -1 *)
  | Vrai -> -1
  | Faux -> -1
  | Variable(i) -> i
  | Noeud1(NON,e) -> variable_max e
  | Noeud2(_, e1, e2) -> max (variable_max e1) (variable_max e2);;

variable_max f1;;

variable_max f2;;

variable_max f3;;

variable_max f4;;

(* Création d'une instanciation à partir de l'écriture binaire *)

let instanciation i m =
  let tab = Array.make m true and q = ref i in
  for i = 0 to m-1 do
    tab.(m-1-i) <- (!q) mod 2 = 1 ; 		(* true correspond à un 1 dans l'écriture binaire *)
    q := (!q) / 2;
  done;
  tab ;;

instanciation 13 4 ;;
instanciation 4 4 ;; 

(* Fonction qui teste la satisfiabilité  *)

let satisfiable e =
  let n = 1 + variable_max e and i = ref 0 in
  while !i < (1 lsl n) && not (eval (instanciation !i n) e) do (* on teste toutes les instanciations associées à des entiers < 2^n et on s'arrête si on en trouve une qui marche *)
    incr(i);
  done;
  (!i < (1 lsl n), instanciation !i n);;
  
satisfiable f1;;

satisfiable f2;;

satisfiable f3;;

satisfiable f4;;

(* Forme normale disjonctive *)

(* Etape 1 : élmination des ALORS *)

let rec elim_alors f = match f with
  | Vrai -> Vrai
  | Faux -> Faux
  | Variable i -> Variable i
  | Noeud1(NON, e) -> Noeud1(NON, elim_alors e)
  | Noeud2(ET, e1 ,e2) -> Noeud2(ET, elim_alors e1, elim_alors e2)
  | Noeud2(OU, e1, e2) -> Noeud2(OU, elim_alors e1, elim_alors e2)
  | Noeud2(ALORS, e1, e2) -> Noeud2(OU, Noeud1(NON, elim_alors e1), elim_alors e2);;

elim_alors f1 ;; 

elim_alors f2 ;;

elim_alors f3 ;;

elim_alors f4 ;; 

(* Etape 2 : règles de De Morgan *)

let rec de_morgan f  = match f with
  | Vrai -> Vrai
  | Faux -> Faux
  | Variable i -> Variable i
  | Noeud1(NON, Vrai) -> Faux
  | Noeud1(NON, Faux) -> Vrai
  | Noeud1(NON, Variable i) -> Noeud1(NON, Variable i)
  | Noeud1(NON, Noeud1(NON, e)) -> de_morgan e
  | Noeud1(NON, Noeud2(OU, e1, e2)) -> Noeud2(ET, de_morgan (Noeud1(NON, e1)), de_morgan(Noeud1(NON,e2)))
  | Noeud1(NON, Noeud2(ET, e1, e2)) -> Noeud2(OU, de_morgan (Noeud1(NON, e1)), de_morgan(Noeud1(NON,e2)))
  | Noeud2(op, e1, e2) when op = ET || op = OU -> Noeud2(op, de_morgan e1, de_morgan e2)
  | e -> de_morgan (elim_alors e);;

de_morgan(f2);;

(* Etape 3 : distribuer les ET sur les OU *)

(* On renvoie le résultat sous forme de liste de facteurs, chaque facteur étant un couple de listes (variables positives, variables négatives) représentant une clause duale*)
(* On suppose ici qu'il n'y a pas de Vrai ou de Faux dans l'expression *)

(* Fonction qui réalise l'union de deux listes en évitant les répétitions *)

let rec union l1 l2 =
  match l1 with
    | [] -> l2
    | x::q when List.mem x l2 -> union q l2
    | x::q -> x::(union q l2) ;;

union [1;2;3] [3;4;5];;

(* Fonction qui distribue un facteur sur une liste de clauses *)

let rec distri1 (f1, f2) l =
  match l with
    | [] -> []
    | (s1, s2)::q -> ((union f1 s1), (union f2 s2)) :: distri1 (f1, f2) q;;

(* Fonction qui distribue une liste de clauses sur une autre  *)

let rec distribue s1 s2 =
  match s1 with
    | [] -> []
    | f::q -> union (distri1 f s2) (distribue q s2) ;;


(*  Fonction qui développe l'expression *)

let rec developpe f = match f with
  | Variable i -> [[i],[]]
  | Noeud1(NON, Variable i) -> [[],[i]]
  | Noeud2(OU, e1, e2) -> union (developpe e1) (developpe e2)
  | Noeud2(ET, e1, e2) -> distribue (developpe e1) (developpe e2)
  | _ -> failwith "expression non conforme" ;;

developpe (de_morgan (elim_alors f1));;

developpe (de_morgan (elim_alors f2));;

developpe (de_morgan (elim_alors f3));;

developpe (de_morgan (elim_alors f4));;

(* Etape 4 : éliminer les contradictions *)

(* Fonction qui détecte une contradiction *)

let rec contradiction pos l  = match l with
  | [] -> false
  | i::q ->  List.mem i pos || contradiction pos q ;;  

(* Fonction qui élimine les contradictions *)

let rec elimine_contradictions l = match l with
  | [] -> []
  | (p,n)::q when contradiction p n -> elimine_contradictions q
  | (p,n)::q -> (p,n)::elimine_contradictions q;;

elimine_contradictions(developpe(de_morgan(elim_alors f4)));;
