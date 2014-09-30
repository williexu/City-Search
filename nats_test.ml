open Assertions
open Nats

(* IntNat *)
(* 
module type NATN = sig                                                                               
type t                                                                      
    val zero : t
    val one : t
    val ( + ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( < ) : t -> t -> bool
    exception Unrepresentable
    val int_of_nat : t -> int
    val nat_of_int : int -> t
  end
*)
let veryBig = max_int+2
(*let tooBig = IntNat.( + ) (IntNat.one) (IntNat.nat_of_int(max_int))*)

(* test ( +) *)

TEST_UNIT "IntNat.( + )_test1" = assert_true ( (IntNat.( + )) (IntNat.nat_of_int(1)) (IntNat.nat_of_int(2)) = (IntNat.nat_of_int(3)))
TEST_UNIT "IntNat.( + )_test2" = assert_true ( IntNat.( + ) (IntNat.nat_of_int(0)) (IntNat.nat_of_int(2)) = IntNat.nat_of_int(2))
TEST_UNIT "IntNat.( + )_test3" = assert_true ( IntNat.( + ) (IntNat.zero) (IntNat.one) = IntNat.one)
TEST_UNIT "IntNat.( + )_test4" = assert_true ( IntNat.( + ) (IntNat.zero) (IntNat.( + ) (IntNat.one) (IntNat.one)) = IntNat.( + ) (IntNat.one) (IntNat.one))
TEST_UNIT "IntNat.( + )_test5" = assert_raises (Some (IntNat.Unrepresentable)) (IntNat.( + ) (IntNat.nat_of_int(max_int))) IntNat.one
TEST_UNIT "IntNat.( + )_test6" = assert_true ( IntNat.( + ) (IntNat.nat_of_int(max_int)) IntNat.zero = (IntNat.nat_of_int(max_int)))
TEST_UNIT "IntNat.( + )_test7" = assert_raises (Some (IntNat.Unrepresentable)) (IntNat.( + ) (IntNat.nat_of_int(max_int))) (IntNat.nat_of_int(max_int))

(* test ( * ) *)

TEST_UNIT "IntNat.( * )_test1" = assert_true ( (IntNat.( * )) (IntNat.nat_of_int(1)) (IntNat.nat_of_int(2)) = (IntNat.nat_of_int(2)))
TEST_UNIT "IntNat.( * )_test2" = assert_true ( (IntNat.( * )) (IntNat.zero) (IntNat.nat_of_int(2)) = (IntNat.zero))
TEST_UNIT "IntNat.( * )_test3" = assert_true ( IntNat.( * ) (IntNat.zero) (IntNat.one) = IntNat.zero)
TEST_UNIT "IntNat.( * )_test4" = assert_true ( IntNat.( * ) (IntNat.one) (IntNat.( * ) (IntNat.one) (IntNat.one)) = IntNat.one)
TEST_UNIT "IntNat.( * )_test5" = assert_true ( IntNat.( * ) (IntNat.one) (IntNat.( + ) (IntNat.one) (IntNat.one)) = IntNat.( + ) (IntNat.one) (IntNat.one))
TEST_UNIT "IntNat.( + )_test6" = assert_raises (Some (IntNat.Unrepresentable)) (IntNat.( * ) (IntNat.nat_of_int(max_int))) (IntNat.nat_of_int(max_int))

(* test ( < ) *)

TEST_UNIT "IntNat.( < )_test1" = assert_true ( (IntNat.( < )) (IntNat.nat_of_int(2)) (IntNat.nat_of_int(4)))
TEST_UNIT "IntNat.( < )_test2" = assert_false ( (IntNat.( < )) (IntNat.nat_of_int(3)) (IntNat.nat_of_int(2)))
TEST_UNIT "IntNat.( < )_test3" = assert_true ( (IntNat.( < )) (IntNat.zero) (IntNat.one))
TEST_UNIT "IntNat.( < )_test4" = assert_false ( (IntNat.( < )) (IntNat.one) (IntNat.zero))

(* test (===) *)

TEST_UNIT "IntNat.( === )_test1" = assert_true ( (IntNat.( === )) (IntNat.nat_of_int(2)) (IntNat.nat_of_int(2)))
TEST_UNIT "IntNat.( === )_test2" = assert_false ( (IntNat.( === )) (IntNat.nat_of_int(3)) (IntNat.nat_of_int(2)))
TEST_UNIT "IntNat.( === )_test3" = assert_true ( (IntNat.( === )) (IntNat.zero) (IntNat.zero))
TEST_UNIT "IntNat.( === )_test4" = assert_false ( (IntNat.( === )) (IntNat.one) (IntNat.zero))

(* test (int_of_nat) *)
(*DOOOOOOOOOOOOOOOOOOOOOOoooo!!!!!!!!!!!!!!!*)
TEST_UNIT "IntNat.int_of_nat_test1" = assert_true ( (IntNat.int_of_nat((IntNat.nat_of_int(2)))) = 2)
TEST_UNIT "IntNat.int_of_nat_test2" = assert_true ( (IntNat.int_of_nat((IntNat.nat_of_int(45)))) = 45)
TEST_UNIT "IntNat.int_of_nat_test3" = assert_true ( (IntNat.int_of_nat(IntNat.zero)) = 0)
(*TEST_UNIT "IntNat.int_of_nat_test4" = assert_raises (Some (IntNat.Unrepresentable)) IntNat.int_of_nat((IntNat.nat_of_int(veryBig)))*)

(* test (nat_of_int) *)
TEST_UNIT "IntNat.nat_of_int_test1" = assert_true ( (IntNat.nat_of_int(2)) = (IntNat.nat_of_int(2)))
TEST_UNIT "IntNat.nat_of_int_test2" = assert_true ( (IntNat.nat_of_int(0)) = IntNat.zero)
TEST_UNIT "IntNat.nat_of_int_test3" = assert_raises (Some (IntNat.Unrepresentable)) IntNat.nat_of_int(-1)



(* ListNat *)

(* test ( + ) *)

TEST_UNIT "ListNat.( + )_test1" = assert_true ( ListNat.( + ) (ListNat.zero) (ListNat.one) = (ListNat.one)  )
TEST_UNIT "ListNat.( + )_test2" = assert_true ( ListNat.( + ) (ListNat.nat_of_int(0)) (ListNat.nat_of_int(2)) = ListNat.nat_of_int(2))
TEST_UNIT "ListNat.( + )_test3" = assert_true ( ListNat.( + ) (ListNat.zero) (ListNat.( + ) (ListNat.one) (ListNat.one)) = ListNat.( + ) (ListNat.one) (ListNat.one))
TEST_UNIT "ListNat.( + )_test4" = assert_true ( ListNat.( + ) (ListNat.nat_of_int(6)) (ListNat.nat_of_int(8)) = ListNat.nat_of_int(14))

(* test ( * ) *)
TEST_UNIT "ListNat.( * )_test1" = assert_true ( (ListNat.( * )) (ListNat.nat_of_int(1)) (ListNat.nat_of_int(2)) = (ListNat.nat_of_int(2)))
TEST_UNIT "ListNat.( * )_test1" = assert_true ( (ListNat.( * )) (ListNat.nat_of_int(2)) (ListNat.nat_of_int(1)) = (ListNat.nat_of_int(2)))
TEST_UNIT "ListNat.( * )_test2" = assert_true ( (ListNat.( * )) (ListNat.zero) (ListNat.nat_of_int(2)) = (ListNat.zero))
TEST_UNIT "ListNat.( * )_test3" = assert_true ( (ListNat.( * )) (ListNat.one) (ListNat.one) = ListNat.one)
TEST_UNIT "ListNat.( * )_test4" = assert_true ( ListNat.( * ) (ListNat.one) (ListNat.( * ) (ListNat.one) (ListNat.one)) = (ListNat.one))
TEST_UNIT "ListNat.( * )_test5" = assert_true ( ListNat.( * ) (ListNat.one) (ListNat.( + ) (ListNat.one) (ListNat.one)) = ListNat.( + ) (ListNat.one) (ListNat.one))

(* test ( < ) *)

TEST_UNIT "ListNat.( < )_test1" = assert_true ( (ListNat.( < )) (ListNat.nat_of_int(2)) (ListNat.nat_of_int(4)))
TEST_UNIT "ListNat.( < )_test2" = assert_false ( (ListNat.( < )) (ListNat.nat_of_int(3)) (ListNat.nat_of_int(2)))
TEST_UNIT "ListNat.( < )_test3" = assert_true ( (ListNat.( < )) (ListNat.zero) (ListNat.one))
TEST_UNIT "ListNat.( < )_test4" = assert_false ( (ListNat.( < )) (ListNat.one) (ListNat.zero))

(* test (===) *)

TEST_UNIT "ListNat.( === )_test1" = assert_true ( (ListNat.( === )) (ListNat.nat_of_int(2)) (ListNat.nat_of_int(2)))
TEST_UNIT "ListNat.( === )_test2" = assert_false ( (ListNat.( === )) (ListNat.nat_of_int(3)) (ListNat.nat_of_int(2)))
TEST_UNIT "ListNat.( === )_test3" = assert_true ( (ListNat.( === )) (ListNat.zero) (ListNat.zero))
TEST_UNIT "ListNat.( === )_test4" = assert_false ( (ListNat.( === )) (ListNat.one) (ListNat.zero))

(* test (int_of_nat) *)

TEST_UNIT "IntNat.int_of_nat_test1" = assert_true ( (IntNat.int_of_nat((IntNat.nat_of_int(2)))) = 2)
TEST_UNIT "IntNat.int_of_nat_test2" = assert_true ( (IntNat.int_of_nat((IntNat.nat_of_int(45)))) = 45)
TEST_UNIT "IntNat.int_of_nat_test3" = assert_true ( (IntNat.int_of_nat(IntNat.zero)) = 0)

(* test (nat_of_int) *)

TEST_UNIT "IntNat.nat_of_int_test1" = assert_true ( (IntNat.nat_of_int(2)) = (IntNat.nat_of_int(2)))
TEST_UNIT "IntNat.nat_of_int_test2" = assert_true ( (IntNat.nat_of_int(0)) = IntNat.zero)
TEST_UNIT "IntNat.nat_of_int_test3" = assert_raises (Some (IntNat.Unrepresentable)) IntNat.nat_of_int(-1)




let () = Pa_ounit_lib.Runtime.summarize ()