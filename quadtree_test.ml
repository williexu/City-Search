open Quadtree
open Assertions

(* Problem 2: Quadtrees- Excercise 1b*)
TEST_UNIT "new_tree_test1" = assert_true(new_tree ((1.,1.),(2.,2.)) = 
	Leaf (((1.,1.),(2.,2.)),[]))

(* Problem 2: Quadtrees- Excercise 1c*)
TEST_UNIT "insert_test1" = assert_true(insert (Leaf(((1.,1.),(3.,3.)),[])) 
	(2.5,2.5) 1 = Leaf (((1., 1.), (3., 3.)), [((2.5, 2.5), 1)]))
TEST_UNIT "insert_test2" = assert_true(insert (Leaf (((1., 1.), (3., 3.)), 
	[((2.5, 2.5), 1)])) (1.1,1.1) 2 = 
	Node (((1., 1.), (3., 3.)), Leaf (((2., 2.), (3., 3.)), [((2.5, 2.5), 1)]),
		                        Leaf (((1., 2.), (2., 3.)), []),
		                        Leaf (((1., 1.), (2., 2.)), [((1.1, 1.1), 2)]),
		                        Leaf (((2., 1.), (3., 2.)), [])))
TEST_UNIT "insert_test3" = assert_true(insert (Leaf (((1., 1.), (3., 3.)),
	[((2.1, 2.1), 1)])) (2.9,2.9) 2 = 
    Node (((1., 1.), (3., 3.)), 
    	Node(((2., 2.), (3., 3.)),Leaf(((2.5, 2.5),(3., 3.)), [((2.9, 2.9), 2)]),
    	                          Leaf(((2., 2.5),(2.5, 3.)), []),
    	                          Leaf(((2., 2.),(2.5, 2.5)), [((2.1, 2.1), 1)]), 
    	                          Leaf(((2.5, 2.),(3., 2.5)), [])), 
    	Leaf (((1., 2.), (2., 3.)), []), 
    	Leaf (((1., 1.), (2., 2.)), []),
    	Leaf (((2., 1.), (3., 2.)), [])))
TEST_UNIT "insert_test4" = assert_true(insert (Leaf(((0.,0.),
    (0.00000003,0.00000003)),[((0.00000001,0.00000001),1)])) 
    (0.00000002,0.00000002) 2 = 
    Leaf (((0., 0.), (3e-08, 3e-08)), [((2e-08, 2e-08), 2); ((1e-08, 1e-08), 1)]))
TEST_UNIT "insert_test5" = assert_raises (Some (OutOfBounds)) ((insert 
    (Leaf(((1.,1.),(3.,3.)),[]))) (4.,4.)) 1

(* Problem 2: Quadtrees- Excercise 1d*)
let func (acc: int) ((point: coord), (b: int)) : int = acc + b

TEST_UNIT "fold_quad_test1" = assert_true(fold_quad func 0 
	(Leaf(((1.,1.),(3.,3.)),[])) = 0)
TEST_UNIT "fold_quad_test2" = assert_true(fold_quad func 0 
	(Leaf (((1., 1.), (3., 3.)), [((2.5, 2.5), 1)])) = 1)
TEST_UNIT "fold_quad_test3" = assert_true(fold_quad func 0 
	(insert (Leaf (((1., 1.), (3., 3.)), [((2.5, 2.5), 1)])) (1.1,1.1) 2) = 3)
TEST_UNIT "fold_quad_test4" = assert_true(fold_quad func 0 
	(insert (insert (Leaf (((1.,1.), (3.,3.)),[((2.5, 2.5), 1)])) (1.1,1.1) 2)
	(2.9,2.9) 3) = 6)
TEST_UNIT "fold_quad_test5" = assert_true(fold_quad func 0 
	(Leaf (((0.,0.),(3e-08, 3e-08)), [((2e-08, 2e-08), 2); ((1e-08, 1e-08), 1)]))
	 = 3)

(* Problem 2: Quadtrees- Excercise 1e*)

TEST_UNIT "fold_region_test1" = assert_true(fold_region func 0 
	(Leaf(((1.,1.),(3.,3.)),[])) ((1.,1.),(2.,2.))= 0)
TEST_UNIT "fold_region_test2" = assert_true(fold_region func 0 
	(Leaf (((1., 1.), (3., 3.)), [((2.5, 2.5), 1)])) ((1.,1.),(2.,2.))= 0)
TEST_UNIT "fold_region_test3" = assert_true(fold_region func 0 
	(Leaf (((1., 1.), (3., 3.)), [((2.5, 2.5), 1)])) ((1.,1.),(3.,3.))= 1)
TEST_UNIT "fold_region_test4" = assert_true(fold_region func 0 
	(insert (Leaf (((1., 1.), (3., 3.)), [((2.5, 2.5), 1)])) (1.1,1.1) 2)
	((0.,0.),(1.5,1.5)) = 2)
TEST_UNIT "fold_region_test4" = assert_true(fold_region func 0 
	(insert (insert (Leaf (((1.,1.), (3.,3.)),[((2.5, 2.5), 1)])) (1.1,1.1) 2)
	(2.9,2.9) 3) ((2.,2.),(3.,3.))= 4)



let () = Pa_ounit_lib.Runtime.summarize ()