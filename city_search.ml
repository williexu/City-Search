open Parser
open Quadtree

let load_city_data (s:string) : string quadtree = 
  let all_cities : city list = parse s in
    List.fold_left (fun acc x -> 
      match x with 
      | (f1,f2,s) -> insert acc (f1,f2) s
    ) (new_tree ((-90.,-180.),(90.,180.))) all_cities

let city_search (q: string quadtree) (r : region) : string list = 
	fold_region (fun acc (cor,st) -> st :: acc) [] q r
