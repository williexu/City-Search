open Assertions
open Parser
open Quadtree


TEST_UNIT "city_search_test1" = assert_true(city_search (Leaf(((1.,1.),(3.,3.)),[((0.8,0.3),"Bangalore")]))
       ((1., 1.), (2., 2.)) = ["lat=1.3, long=1.8, name=Bangalore\n"]) 

TEST_UNIT "city_search_test2" = assert_true(city_search (Node (((1., 1.), (3., 3.)), Leaf (((2., 2.), (3., 3.)), [((2.5, 2.5), "Ithaca")]),
                            Leaf (((1., 2.), (2., 3.)), [((2.0,1.0),"Mumbai")]),
                            Leaf (((1., 1.), (2., 2.)), [((1.1, 1.1), "Delhi")]),
                            Leaf (((2., 1.), (3., 2.)), []))) ((0.0,0.0),(3.0,3.0)) = 
              ["lat=1.1, long=1.1, name=Delhi\n"; "lat=2., long=1., name=Mumbai\n";"lat=2.5, long=2.5, name=Ithaca\n"] )

TEST_UNIT "city_search_test3" = assert_true(city_search (Node (((1., 1.), (3., 3.)), Leaf (((2., 2.), (3., 3.)), [((2.5, 2.5), "Ithaca")]),
                            Leaf (((1., 2.), (2., 3.)), [((2.0,1.0),"Mumbai")]),
                            Leaf (((1., 1.), (2., 2.)), [((1.1, 1.1), "Delhi")]),
                            Leaf (((2., 1.), (3., 2.)), []))) ((0.0,0.0),(0.0,0.0)) = [] )
 
TEST_UNIT "city_search_test4" = assert_true(city_search (Leaf(((1.,1.),(3.,3.)),[]))
        ((1., 1.),(2.,2.)) = []) 

TEST_UNIT "city_search_test5" = assert_true(city_search (Node (((1., 1.), (3., 3.)), Leaf (((2., 2.), (3., 3.)), [((6.5, 6.5), "Ithaca")]),
                            Leaf (((1., 2.), (2., 3.)), [((2.0,1.0),"Mumbai")]),
                            Leaf (((1., 1.), (2., 2.)), [((1.1, 1.1), "Delhi")]),
                            Leaf (((2., 1.), (3., 2.)), []))) ((0.0,0.0),(3.0,3.0)) = [] )

let () = Pa_ounit_lib.Runtime.summarize ()