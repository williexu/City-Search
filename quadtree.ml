type coord = float * float
type region = coord * coord
(** Quadtrees represent a square area of 2D space. This is reflected in its
  * region: two coordinates that indicate the lower left and upper right 
  * coordinates of that square. 
  * Nodes of a quadtree are tuples of its region and the four quadtrees that
  * quadtree. The four quadtrees of a node partition the region into four 
  * equally sized squares: NE, NW, SW, SE.
  * A quadtree leaf, like a node, is also a tuple; however, it is a tuple of
  * just its region and a list of (coord * 'a). 'a depends on what the quadtree
  * is used for. Most leaves, as long as its diagonal length is greater or equal
  * to than a constant min_diagonal, will only have one element in its list. If
  * an onject is added to that list, the leaf will become a Node and the objects 
  * will be distributed into it. Leaves with diagonal lengths smaller than 
  * min_diagonal can have more than one object in its list.  
  *)
type 'a quadtree =
  Node of region * 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree
  | Leaf of region * ((coord * 'a) list)
		       
let min_diagonal = 0.0000001

exception OutOfBounds

(** This is a constructor for quadtree that takes in a region. When called, it
  * evaluates to a Leaf tuple of the region input and a empty list.
  * Arguments: a region r 
  * Precondtion: r is a region that is a tuple of two coord
  * Postcondition: returns a 'a quadtree that is a leaf of a tuple of region
  * and an empty list
  *)
let new_tree (r:region) : 'a quadtree =
  Leaf (r, [])
        
let cor_in_region (c : coord) (r : region) =
  match r with
  | (r1,r2) -> (fst c >= fst r1) && (fst c <= fst r2) && (snd c >= snd r1)
    && (snd c <= snd r2)

let rec insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree =
  match q with 
  | Node (r,q1,q2,q3,q4) -> 
    if (cor_in_region c r) then
      let mid_x : float= (fst(fst r) +. fst(snd r)) /. 2.0 in
      let mid_y : float= (snd(fst r) +. snd(snd r)) /. 2.0 in
      if snd c >= mid_y then if fst c >= mid_x then Node(r,insert q1 c s,q2,q3,q4)
                             else Node(r,q1,insert q2 c s,q3,q4)
      else if fst c >= mid_x then Node(r,q1,q2,q3,insert q4 c s)
           else Node(r,q1,q2,insert q3 c s,q4)
    else raise OutOfBounds
  | Leaf (reg,lst) -> 
    if (cor_in_region c reg) then 
      let diagonal_length : float = (((fst(snd reg)-. fst (fst reg)) ** 2.) +. 
        ((snd(snd reg)-. snd(fst reg)) ** 2.)) ** 0.5 in
      if List.length(lst) = 1 then 
      (*if length of lst > 1, assume diagonal_length < min_diagonal*) 
        if diagonal_length < min_diagonal then Leaf (reg,(c,s) :: lst)
        else 
          let mid_x : float = (fst(fst reg) +. fst(snd reg)) /. 2.0 in
          let mid_y : float = (snd(fst reg) +. snd(snd reg)) /. 2.0 in
          let new_Node : 'a quadtree =Node (reg,new_tree((mid_x,mid_y),snd reg),
            new_tree ((fst(fst reg),mid_y),(mid_x,snd(snd reg))),
            new_tree (fst reg,(mid_x,mid_y)),
            new_tree ((mid_x,snd(fst reg)),(fst(snd reg),mid_y))) in 
          insert (insert new_Node (fst(List.hd lst)) (snd(List.hd lst))) c s  
      else Leaf (reg,(c,s) :: lst)
    else raise OutOfBounds 

							      
let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a)
		  (a: 'a) (t: 'b quadtree): 'a 
  = match t with
    |Node (r,q1,q2,q3,q4) -> 
      fold_quad f(fold_quad f(fold_quad f(fold_quad f a q1) q2) q3) q4 
    |Leaf (reg, lst) -> 
      match lst with 
      | [] -> a
      | x::xs -> fold_quad f (f a x) (Leaf(reg,xs))
	   
let rec fold_region (f: 'a -> coord * 'b -> 'a) (a : 'a) (t : 'b quadtree) 
  (r : region) : 'a
  = match t with
    |Node (re,q1,q2,q3,q4) -> 
      fold_region f(fold_region f(fold_region f(fold_region f a q1 r) q2 r) 
      q3 r) q4 r 
    |Leaf (reg, lst) -> 
      match lst with 
      | [] -> a
      | x::xs -> if cor_in_region (fst x) r then 
                   fold_region f (f a x) (Leaf(reg,xs)) r
                 else fold_region f a (Leaf(reg,xs)) r


