(* A *)


(* B *)
(* 1 *)

let l = [[3];[3;4];[4;7];[5;6;7];[6];[];[];[]];;


(* 2 *)
type vertex_state = To_explore | Exploring | Explored;;




(* 3 *)
let topo_sort graphe = 
  let n = List.length graphe in
  let pile = [] in
  let etat =  Array.make n To_explore in
  let rec topo_dfs graphe pile (h,index) etat =
    etat.(index) <- Exploring;
    List.iter (fun x -> if etat.(x) = To_explore then topo_dfs graphe pile (List.nth graphe x,x) etat) h;
    etat.(index) <- Explored in  
  let rec aux index acc = function
  | [] -> acc;
  | h::q -> if etat.(index) = To_explore then topo_dfs graphe acc (h,index) etat;
            aux (index+1) (index::acc) q;
in aux 0 pile graphe;;



(* 4 *)
let topo_sort2 graphe = 
  let n = List.length graphe in
  let pile = [] in
  let etat =  Array.make n To_explore in
  let rec topo_dfs graphe pile (h,index) etat =
    etat.(index) <- Exploring;
    List.iter (fun x -> if etat.(x) = To_explore then topo_dfs graphe pile (List.nth graphe x,x) etat) h;
    etat.(index) <- Explored in  
  let rec aux index acc = function
  | [] -> acc;
  | h::q -> if etat.(index) = Exploring then failwith "Erreur" else if etat.(index) = To_explore then topo_dfs graphe acc (h,index) etat;
            aux (index+1) (index::acc) q;
in aux 0 pile graphe;;

let gc = [ [3] ; [3;4] ; [4;7] ; [5;6;7] ; [6] ; [] ; [0] ; [] ] ;;


(* 5 *)
let topo_sort3 graphe = 
  let n = List.length graphe in
  let pile = [] in
  let etat =  Array.make n To_explore in
  let date = ref 0 in
  let date_sommet = Array.make n (0,0) in
  let rec topo_dfs graphe pile (h,index) etat date =
    etat.(index) <- Exploring;
    date_sommet.(index) <- (!date,0);
    List.iter (fun x -> date :=  (!date + 1);
                        if etat.(x) = To_explore then topo_dfs graphe pile (List.nth graphe x,x) etat date) h;
    etat.(index) <- Explored in
    date_sommet.(index) <- (fst (date_sommet.(index)),!date); 
  let rec aux index acc = function
  | [] -> acc;
  | h::q -> if etat.(index) = Exploring then failwith "Erreur" else if etat.(index) = To_explore then topo_dfs graphe acc (h,index) etat date;
            aux (index+1) (index::acc) q;
in aux 0 pile graphe;;


(* 6 *)
let big = [| [3] ; [3;4] ; [3;4] ; [6] ; [3;7;9] ; [6] ; [8;9;10] ; [9] ;
[10;11]; [11]; [] ;[]|] ;;


(* 7 *)


