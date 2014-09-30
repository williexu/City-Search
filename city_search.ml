open Parser
open Quadtree

let load_city_data (s:string) : string quadtree =
  let (cl : city list) = parse (s) in
    let n1 = new_tree ((-90.,-180.), (90.,180.)) in
     let rec traverseCities (cl1:city list) (n2:string quadtree) : string quadtree =
      match cl1 with
      [] -> n2
      | x::xs -> match x with
               (lat,long,name) -> traverseCities (xs) (insert (n2) (lat,long) (name))
      in traverseCities cl n1


let city_search (q: string quadtree) (r : region) : string list =
	let f (a:'a list) ((c:coord), (b:'b)) : 'a list = ((city_to_string (fst c, snd c, b))::[]) @ a
   in
   fold_region f [] q r