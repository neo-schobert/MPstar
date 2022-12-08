(* A *)

(* A1 *)

type 'a qdata = {value: 'a; priority: int};;
type 'a priority_queue = {mutable first_free: int; heap: 'a qdata array};;

(* A2 *)

let heap_test = Array.init 10 (fun i -> i+1);;


(* C'est un tas-min *)

(* A3 *)

let swap tab a b =
  let c = tab.(a) in
  tab.(a) <- tab.(b);
  tab.(b) <- c;;


(* A4 *)

let rec up tab i = match i with
  | 0 -> ()
  | x when tab.(x/2).priority > tab.(x).priority -> swap tab x (x/2);
  up tab (x/2)
  | _ -> ();;



(* A5 *)

let rec down tab n = function 
| 0 -> ()
| x when 2*x >= n -> () 
| x when tab.(2*x).priority < tab.(x).priority -> swap tab (2*x) x;
down tab n (2*x);
| x when 2*x + 1 >= n -> ()
| x when tab.(2*x + 1).priority < tab.(x).priority -> swap tab (2*x + 1) x;
down tab n (2*x + 1)
| _ -> ();;





(* A6 *)

let make_priority_queue n elt = let fst_free = 0 in
let hp = Array.init n (fun i -> {value = elt.value; priority = elt.priority + i}) in 
{first_free = fst_free; heap = hp};;



(* A7 *)

let insert pq elt = 
  let n = Array.length pq.heap in  
  match pq.first_free with
  | x when x >= n -> failwith "FULL_PRIORITY_QUEUE"
  | _ -> pq.heap.(pq.first_free) <- elt;
  up pq.heap pq.first_free;
  pq.first_free <- (pq.first_free + 1);;



(* A8 *)

let get_min pq = 
  let n = Array.length pq.heap in
  let elt_min = ref pq.heap.(0) in
  let ind_min = ref 0 in
  for k = 0 to (n-1) do 
    if pq.heap.(k).priority < !elt_min.priority then (elt_min := pq.heap.(k);
  ind_min := k);
  done;
  for i = !ind_min to (n-2) do
    pq.heap.(i) <- pq.heap.(i+1);
  done;
  pq.first_free <- pq.first_free - 1;
  !elt_min.value;;




(* B5 *)

let dijkstra g start stop =
  let delta = make_priority_queue 0 {value = start; priority = 0} in
  let dist = Array.init g.size (fun i -> if i = start then 0 else max_int) in
  let pere = Array.init g.taille (fun i -> i) in
  for i=0 to g.size - 1 do (* initialisation de la file *)
    insert delta i;
  done;
  while not (est_vide q) do
      let x = extraire_min q in
      (* on regarde les adjacents de x *)
      List.iter (fun (c,y) ->
          if dist.(y) > dist.(x) + c
          then begin
              pere.(y) <- x;
              dist.(y) <- dist.(x) + c;
              diminuer_clef y dist.(y) q
      end) g.adj.(x)
  done;
  dist, pere
;;
