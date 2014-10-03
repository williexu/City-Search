open Parser
open Quadtree

(** This function loads all the cities from the file named
  * by the string argument passed to it, and stores them in a string quadtree.
  * It returns that quadtree.

  * Arguments: A string s, which represents the name of the file whose
  * contents are to be loaded. The file format is CSV. Each line in the 
  * file is a city. The format of a line is Latitude , Longitude , Name.
  
  * Pre-condition: s must be of type string
  *  City coordinates are given in terms of latitude and longitude.
  *  Latitude ranges from -90.0 to 90.0.
  *  Longitude ranges from -180.0 to 180.0
  
  * Post-condition: The function returns a string quadtree, containing
  * the name of each city in s inserted into the string quadtree at the
  * appropriate latitude-longitude location in the quadtree. 
  * If there are no cities in s, it returns an empty string quadtree with
  * region ((-90.,-180.), (90.,180.)).
  *)
let load_city_data (s:string) : string quadtree = 
  let all_cities : city list = parse s in
    List.fold_left (fun acc x -> 
      match x with 
      | (f1,f2,s) -> insert acc (f1,f2) s
    ) (new_tree ((-90.,-180.),(90.,180.))) all_cities

(** This function returns a string list of all the cities in a string quadtree 
  * within a given region, specified by latitude and longitude.

  * Arguments: q of type string quadtree and a region r

  * Pre-condition: q must be of type string quadtree
                    r must be of type region, where region is of type
                    (coord*coord) and coord is of type (float*float)

  * Post-condition: The function returns a string list of all the cities in q
  * within the given region r. If there are no cities within r, it returns an
  * empty list.
  *)
let city_search (q: string quadtree) (r : region) : string list = 
	fold_region (fun acc (cor,st) -> st :: acc) [] q r
