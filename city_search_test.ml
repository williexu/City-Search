open Assertions
open Parser
open Quadtree
open City_search

(* load_city_data *)

let s = "ithaca.csv"

TEST_UNIT "load_city_data_test1" = assert_true( (city_search (load_city_data s) ((-90.,-180.), (42.44,76.5))) = 
    ["lat=42.4339618, long=-76.4924404, name=South Hill School\n"; 
    "lat=42.4367395, long=-76.4766064, name=Belle Sherman School\n"])

TEST_UNIT "load_city_data_test2" = assert_true(city_search (load_city_data s) ((-90.,-180.), (90.,180.)) = 
    ["lat=42.4417395, long=-76.5118856, name=Oldport Harbour\n";
     "lat=42.4422222, long=-76.5022222, name=Greater Ithaca Activities Center\n";   
     "lat=42.4423826, long=-76.4987232, name=First Presbyterian Church\n";
     "lat=42.4438889, long=-76.5016667, name=Calvary Baptist Church\n";
     "lat=42.450906, long=-76.4954959, name=Fall Creek School\n";
     "lat=42.4339618, long=-76.4924404, name=South Hill School\n";
     "lat=42.4367395, long=-76.4766064, name=Belle Sherman School\n"])




(* city_search *)
TEST_UNIT "city_search_test1" = assert_true(city_search (Leaf(((1.,1.),(3.,3.)),[((1.8,1.3),"Bangalore")]))
       ((1., 1.), (2., 2.)) = ["lat=1.8, long=1.3, name=Bangalore\n"]) 

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
                            Leaf (((2., 1.), (3., 2.)), []))) ((0.0,0.0),(3.0,3.0)) = 
                           ["lat=1.1, long=1.1, name=Delhi\n"; "lat=2., long=1., name=Mumbai\n"] )

let () = Pa_ounit_lib.Runtime.summarize ()