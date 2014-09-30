type coord = float * float
type region = coord * coord

(*  'a quadtree is an abstract datatype to represent quadtrees.
  * It has two contructors-Node and Leaf.
  * 
    * The Node represents the region of space between two x coordinates, and between 2 y coordinates. 
    * The region is partitioned recursively into four equally-sized quadrants.
    * Each of those quadrants is a subtree.
    
    * The Node type is represented as a tuple of the region and each of the four subtrees or quadrants.
    * The first quadtree in a Node is the north-eastern quadrant (I) of the region,
    * the second is the north-western quadrant (II), the third is the south-western quadrant (III),
    * and the fourth is the south-eastern quadrant (IV)

    * The Leaf constructor represents a (possibly empty) set of objects.
    * It is represented as a tuple of the region and a list of tuples of the coordinate location of the 'a type contained in the
    * Leaf and the a' type.

    * Generally, the Leaf should only contain one object, and if a new object being inserted into the Leaf causes its list to have more
    * than one object, the Leaf becomes a Node and the Leaf's objects are distributed into the Node. What this means is that each object in
    * the list now has a more specific location, i.e., the quadtree is more divided. To avoid the quadtree becoming too divided, a Leaf
    * is never separated if doing so results in the size of the region becoming too small. If the region already has a
    * diagonal length (from its lower-left coordinate to the upper-right) that is less than a constant min_diagonal = o.0000001, then
    * the Leaf remains a leaf, and each new object is added to that Leaf's list, i.e., length of the list increases.
    * The order of objects in the list is unspecified.
*)
type 'a quadtree =
  Node of region * 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree
  | Leaf of region * ((coord * 'a) list)
		       
let min_diagonal = 0.0000001
		     
exception OutOfBounds

(* This function initializes a new quadtree that will contain points within the given region.
    * Initially, there are no points in the quadtree so its Leaf constructor is simply
    * initialized with the region r and an empty list.
    
    * Arguments: It takes in a region r, which is a tuple of two coords.

    * Pre-condition: 'r' must be a region.
    * Post-condition: The function returns an 'a quadtree with its Leaf constructor
    * initialized with the region and an empty list.
*)
let new_tree (r:region) : 'a quadtree =
    Leaf (r,[])

        
let rec insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree =
  match q with
  | Node (r,x1,x2,x3,x4) -> 
     (if (fst c >= fst (fst r) && fst c <= fst (snd r) && snd c >= snd (fst r) && snd c <= snd (snd r)) then
           let diagCentre r : (float*float) = ((fst (fst r) +. fst (snd r)) /. 2.), ((snd (fst r) +. snd (snd r)) /.2.) in
        
           if (fst c >= fst (fst r) && fst c < fst (diagCentre r) && snd c < snd (diagCentre r)) 
              then Node(r, x1, x2, insert x3 c s, x4)
              else if (fst c >= fst (fst r) && fst c < fst (diagCentre r) && snd c >= snd (diagCentre r)) 
                   then Node(r, x1, insert x2 c s, x3, x4)
                   else if (fst c >= fst (diagCentre r) && fst c <= fst (snd r) && snd c < snd (diagCentre r))
                        then Node(r, x1, x2, x3, insert x4 c s)
                        else Node(r, insert x1 c s, x2, x3, x4)

      else raise OutOfBounds)

  | Leaf (r, l) -> 
     (if (fst c >= fst (fst r) && fst c <= fst (snd r) && snd c >= snd (fst r) && snd c <= snd (snd r)) then
            
            let diagLength dl : float = (((fst (snd r) -. fst (fst r)) ** 2.) +. ((snd (snd r) -. snd (fst r)) ** 2.)) ** 0.5 in
            
             let diagCentre r : (float*float) = ((fst (fst r) +. fst (snd r)) /. 2.), ((snd (fst r) +. snd (snd r)) /.2.) in

                 if ((List.length(l) > 0) && (diagLength r > min_diagonal)) 
                
                 then let newNode : 'a quadtree = insert (Node(r, 
                 
                 new_tree (  (fst (diagCentre r), snd (diagCentre r)), (fst (snd r), snd (snd r))     ),
                 new_tree (  (fst (fst r), snd (diagCentre r) ) , (fst (diagCentre r), snd (snd r))  ),
                 new_tree (  (fst (fst r), snd (fst r)), (fst (diagCentre r), snd (diagCentre r))     ),
                 new_tree (  (fst (diagCentre r), snd (fst r)), (fst (snd r), snd (diagCentre r))      )
                               )) c s 
                      
                      in
                          let rec travList (ls) (newNode1) : 'a quadtree =
                           (match ls with
                               [] -> newNode1
                              | h::t-> travList (t) (insert (newNode1) (fst h) (snd h)))
                          in travList l newNode
                                
                 else Leaf (r, (c,s)::l) 

     else raise OutOfBounds)



							      
let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a)
		  (a: 'a) (t: 'b quadtree): 'a 
  = 
  match t with
  | Node (r,x1,x2,x3,x4) -> fold_quad f (fold_quad f (fold_quad f (fold_quad f a x1) x2) x3) x4
  | Leaf (r,l) -> match l with
                  [] -> a
                  | x::xs -> fold_quad f (f a x) (Leaf (r,xs))
                  

	   
let rec fold_region (f: 'a -> coord * 'b -> 'a) (a : 'a) (t : 'b quadtree) 
  (r : region) : 'a
= 
 match t with
 | Node (r1,x1,x2,x3,x4) -> fold_region f (fold_region f (fold_region f (fold_region f a x1 r) x2 r) x3 r) x4 r
 | Leaf (r1,l) -> match l with
                 [] -> a
                 | x::xs -> if (fst (fst x) >= fst (fst r) && fst (fst x) <= fst (snd r) && 
                             snd (fst x) >= snd (fst r) && snd (fst x) <= snd (snd r))
                           then (fold_region f (f a x) (Leaf (r1,xs)) r)
                           else (fold_region f a (Leaf (r1,xs)) r)

                       

  

