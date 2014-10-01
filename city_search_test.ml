open City_search
open Quadtree
open Assertions

(*Problem 2: City Search- Excercise 2a *)
TEST_UNIT "load_city_data_test1" = assert_true(city_search 
	(load_city_data "ithaca.csv") ((42.,-77.),(43.,-76.)) = ["Oldport Harbour";
	"Greater Ithaca Activities Center";"First Presbyterian Church"; 
	"Calvary Baptist Church"; "Fall Creek School";"South Hill School"; 
	"Belle Sherman School"])

TEST_UNIT "city_search_test1" = assert_true(city_search 
	(Leaf(((1.,1.),(3.,3.)),[])) ((0.,0.),(4.,4.)) = [])
TEST_UNIT "city_search_test2" = assert_true(city_search 
    (insert (insert (Leaf (((1.,1.),(3.,3.)),[((2.5, 2.5),"1")]))(1.1,1.1) "2")
	(2.9,2.9) "3") ((0.,0.),(1.,1.)) = [])
TEST_UNIT "city_search_test3" = assert_true(city_search 
    (insert (insert (Leaf (((1.,1.),(3.,3.)),[((2.5, 2.5),"1")]))(1.1,1.1) "2")
	(2.9,2.9) "3") ((2.,2.),(3.,3.)) = ["1";"3"])
TEST_UNIT "city_search_test4" = assert_true(city_search 
    (insert (insert (Leaf (((1.,1.),(3.,3.)),[((2.5, 2.5),"1")]))(1.1,1.1) "2")
	(2.9,2.9) "3") ((0.,0.),(3.,3.)) = ["2";"1";"3"])

let () = Pa_ounit_lib.Runtime.summarize ()