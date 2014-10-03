open Assertions
open Nats

(* IntNat *)

(* test ( +) *)

TEST_UNIT "IntNat.( + )_test1" = assert_true (IntNat.( + ) 
	(IntNat.nat_of_int(15)) (IntNat.nat_of_int(20)) = (IntNat.nat_of_int(35)))
TEST_UNIT "IntNat.( + )_test2" = assert_true ( IntNat.( + ) 
	(IntNat.nat_of_int(0)) (IntNat.nat_of_int(2)) = IntNat.nat_of_int(2))
TEST_UNIT "IntNat.( + )_test3" = assert_true ( IntNat.( + ) 
	(IntNat.zero) (IntNat.one) = IntNat.one)
TEST_UNIT "IntNat.( + )_test4" = assert_true ( IntNat.( + ) 
	(IntNat.zero) (IntNat.( + ) (IntNat.one) (IntNat.one)) =
	IntNat.( + ) (IntNat.one) (IntNat.one))
TEST_UNIT "IntNat.( + )_test5" = assert_raises (Some (IntNat.Unrepresentable))
    (IntNat.( + ) (IntNat.nat_of_int(max_int))) IntNat.one
TEST_UNIT "IntNat.( + )_test6" = assert_true ( IntNat.( + ) 
	(IntNat.nat_of_int(max_int)) IntNat.zero = (IntNat.nat_of_int(max_int)))
TEST_UNIT "IntNat.( + )_test7" = assert_raises (Some (IntNat.Unrepresentable))
    (IntNat.( + ) (IntNat.nat_of_int(max_int))) (IntNat.nat_of_int(max_int))

(* test ( * ) *)

TEST_UNIT "IntNat.( * )_test1" = assert_true ( (IntNat.( * )) 
	(IntNat.nat_of_int(15)) (IntNat.nat_of_int(20)) = (IntNat.nat_of_int(300)))
TEST_UNIT "IntNat.( * )_test2" = assert_true ( (IntNat.( * )) 
	(IntNat.nat_of_int(1)) (IntNat.nat_of_int(2)) = (IntNat.nat_of_int(2)))
TEST_UNIT "IntNat.( * )_test3" = assert_true ( (IntNat.( * )) (IntNat.zero)
    (IntNat.nat_of_int(2)) = (IntNat.zero))
TEST_UNIT "IntNat.( * )_test4" = assert_true ( IntNat.( * ) (IntNat.zero) 
	(IntNat.one) = IntNat.zero)
TEST_UNIT "IntNat.( * )_test5" = assert_true ( IntNat.( * ) (IntNat.one) 
	(IntNat.( * ) (IntNat.one) (IntNat.one)) = IntNat.one)
TEST_UNIT "IntNat.( * )_test6" = assert_true ( IntNat.( * ) (IntNat.one) 
	(IntNat.( + ) (IntNat.one) (IntNat.one)) = IntNat.( + ) (IntNat.one) 
	(IntNat.one))
TEST_UNIT "IntNat.( + )_test7" = assert_raises (Some (IntNat.Unrepresentable))
    (IntNat.( * ) (IntNat.nat_of_int(max_int))) (IntNat.nat_of_int(max_int))

(* test ( < ) *)

TEST_UNIT "IntNat.( < )_test1" = assert_true ( (IntNat.( < )) 
	(IntNat.nat_of_int(2)) (IntNat.nat_of_int(4)))
TEST_UNIT "IntNat.( < )_test2" = assert_false ( (IntNat.( < )) 
	(IntNat.nat_of_int(3)) (IntNat.nat_of_int(2)))
TEST_UNIT "IntNat.( < )_test3" = assert_true ( (IntNat.( < )) 
	(IntNat.zero) (IntNat.one))
TEST_UNIT "IntNat.( < )_test4" = assert_false ( (IntNat.( < )) 
	(IntNat.one) (IntNat.zero))

(* test (===) *)

TEST_UNIT "IntNat.( === )_test1" = assert_true ( (IntNat.( === )) 
	(IntNat.nat_of_int(2)) (IntNat.nat_of_int(2)))
TEST_UNIT "IntNat.( === )_test2" = assert_false (IntNat.( === ) 
	(IntNat.nat_of_int(3)) (IntNat.nat_of_int(2)))
TEST_UNIT "IntNat.( === )_test3" = assert_true (IntNat.( === )
	(IntNat.zero) (IntNat.zero))
TEST_UNIT "IntNat.( === )_test4" = assert_false (IntNat.( === ) 
	(IntNat.one) (IntNat.zero))

(* test (int_of_nat) *)

TEST_UNIT "IntNat.int_of_nat_test1" = assert_true (IntNat.int_of_nat
	(IntNat.nat_of_int(2)) = 2)
TEST_UNIT "IntNat.int_of_nat_test2" = assert_true (IntNat.int_of_nat
	(IntNat.nat_of_int(45)) = 45)
TEST_UNIT "IntNat.int_of_nat_test3" = assert_true (IntNat.int_of_nat
	(IntNat.zero) = 0)

(* test (nat_of_int) *)

TEST_UNIT "IntNat.nat_of_int_test1" = assert_true ((IntNat.nat_of_int(0)) = 
    IntNat.zero)
TEST_UNIT "IntNat.nat_of_int_test2" = assert_raises (Some 
	(IntNat.Unrepresentable)) IntNat.nat_of_int(-1)


(* ListNat *)

(* test ( + ) *)

TEST_UNIT "ListNat.( + )_test1" = assert_true ( ListNat.( + ) 
	(ListNat.zero) (ListNat.one) = (ListNat.one)  )
TEST_UNIT "ListNat.( + )_test2" = assert_true ( ListNat.( + ) 
	(ListNat.nat_of_int(0)) (ListNat.nat_of_int(2)) = ListNat.nat_of_int(2))
TEST_UNIT "ListNat.( + )_test3" = assert_true ( ListNat.( + ) 
	(ListNat.zero) (ListNat.( + ) (ListNat.one) (ListNat.one)) = ListNat.( + ) 
	(ListNat.one) (ListNat.one))
TEST_UNIT "ListNat.( + )_test4" = assert_true ( ListNat.( + ) 
	(ListNat.nat_of_int(6)) (ListNat.nat_of_int(8)) = ListNat.nat_of_int(14))

(* test ( * ) *)

TEST_UNIT "ListNat.( * )_test1" = assert_true (ListNat.( * ) 
	(ListNat.nat_of_int(1)) (ListNat.nat_of_int(2)) = (ListNat.nat_of_int(2)))
TEST_UNIT "ListNat.( * )_test1" = assert_true ( ListNat.( * ) 
	(ListNat.nat_of_int(2)) (ListNat.nat_of_int(1)) = (ListNat.nat_of_int(2)))
TEST_UNIT "ListNat.( * )_test2" = assert_true ( ListNat.( * ) 
	(ListNat.zero) (ListNat.nat_of_int(2)) = (ListNat.zero))
TEST_UNIT "ListNat.( * )_test3" = assert_true ( ListNat.( * ) 
	(ListNat.one) (ListNat.one) = ListNat.one)
TEST_UNIT "ListNat.( * )_test4" = assert_true ( ListNat.( * ) 
	(ListNat.one) (ListNat.( * ) (ListNat.one) (ListNat.one)) = (ListNat.one))
TEST_UNIT "ListNat.( * )_test5" = assert_true ( ListNat.( * ) 
	(ListNat.one) (ListNat.( + ) (ListNat.one) (ListNat.one)) = 
	ListNat.( + ) (ListNat.one) (ListNat.one))

(* test ( < ) *)

TEST_UNIT "ListNat.( < )_test1" = assert_true (ListNat.( < ) 
	(ListNat.nat_of_int(2)) (ListNat.nat_of_int(4)))
TEST_UNIT "ListNat.( < )_test2" = assert_false (ListNat.( < ) 
	(ListNat.nat_of_int(3)) (ListNat.nat_of_int(2)))
TEST_UNIT "ListNat.( < )_test3" = assert_true (ListNat.( < ) 
	(ListNat.zero) (ListNat.one))
TEST_UNIT "ListNat.( < )_test4" = assert_false (ListNat.( < ) 
	(ListNat.one) (ListNat.zero))

(* test (===) *)

TEST_UNIT "ListNat.( === )_test1" = assert_true (ListNat.( === ) 
	(ListNat.nat_of_int(2)) (ListNat.nat_of_int(2)))
TEST_UNIT "ListNat.( === )_test2" = assert_false (ListNat.( === ) 
	(ListNat.nat_of_int(3)) (ListNat.nat_of_int(2)))
TEST_UNIT "ListNat.( === )_test3" = assert_true (ListNat.( === ) 
	(ListNat.zero) (ListNat.zero))
TEST_UNIT "ListNat.( === )_test4" = assert_false (ListNat.( === ) 
	(ListNat.one) (ListNat.zero))

(* test (int_of_nat) *)

TEST_UNIT "ListNat.int_of_nat_test1" = assert_true 
    (ListNat.int_of_nat(ListNat.nat_of_int(2)) = 2)
TEST_UNIT "ListNat.int_of_nat_test2" = assert_true 
    (ListNat.int_of_nat(ListNat.nat_of_int(45)) = 45)
TEST_UNIT "ListNat.int_of_nat_test3" = assert_true 
    (ListNat.int_of_nat(ListNat.zero) = 0)

(* test (nat_of_int) *)

TEST_UNIT "ListNat.nat_of_int_test1" = assert_true (ListNat.nat_of_int(2) = 
	ListNat.( + ) ListNat.one ListNat.one)
TEST_UNIT "ListNat.nat_of_int_test2" = assert_true (ListNat.nat_of_int(0) = 
    ListNat.zero)
TEST_UNIT "ListNat.nat_of_int_test3" = assert_raises (Some 
	(ListNat.Unrepresentable)) ListNat.nat_of_int(-1)

(* NatConvertFn *)

module IntConvert = NatConvertFn(IntNat)

(* int_of_nat *)

TEST_UNIT "NatConvertFn.int_of_nat_test1" = assert_true 
    ((IntConvert.int_of_nat(IntNat.nat_of_int(8))) = 8)
TEST_UNIT "NatConvertFn.int_of_nat_test2" = assert_true 
    ((IntConvert.int_of_nat(IntNat.nat_of_int(0))) = 0)

(* nat_of_int *)

TEST_UNIT "NatConvertFn.nat_of_int_test1" = assert_raises 
    (Some (IntNat.Unrepresentable)) IntConvert.nat_of_int(-1)
TEST_UNIT "NatConvertFn.nat_of_int_test2" = assert_true 
    (IntConvert.nat_of_int(8) = IntNat.nat_of_int(8))

(* AlienNatFn *)

module IntAlien : AlienMapping = struct
  type aliensym = int

  let one = 1
  let zero = 0
  let int_of_aliensym (x:aliensym): int = x
end

module AlienNat = AlienNatFn(IntAlien)

(* test ( + ) *)

TEST_UNIT "AlienNat.( + )_test1" = assert_true (AlienNat.( + ) 
	(AlienNat.zero) (AlienNat.one) = AlienNat.nat_of_int(1))
TEST_UNIT "AlienNat.( + )_test2" = assert_true ( AlienNat.( + ) 
	(AlienNat.nat_of_int(0)) (AlienNat.nat_of_int(2)) = AlienNat.nat_of_int(2))
TEST_UNIT "AlienNat.( + )_test3" = assert_true ( AlienNat.int_of_nat
	(AlienNat.( + ) (AlienNat.zero) (AlienNat.( + ) (AlienNat.one) 
	(AlienNat.one))) = AlienNat.int_of_nat(AlienNat.nat_of_int(2)))
TEST_UNIT "AlienNat.( + )_test4" = assert_true ( AlienNat.( + ) 
	(AlienNat.nat_of_int(6)) (AlienNat.nat_of_int(8)) =AlienNat.nat_of_int(14))

(* test ( * ) *)

TEST_UNIT "AlienNat.( * )_test1" = assert_true (AlienNat.( * ) 
	(AlienNat.nat_of_int(1)) (AlienNat.nat_of_int(2)) = 
	(AlienNat.nat_of_int(2)))
TEST_UNIT "AlienNat.( * )_test1" = assert_true (AlienNat.( * ) 
	(AlienNat.nat_of_int(2)) (AlienNat.nat_of_int(1))=(AlienNat.nat_of_int(2)))
TEST_UNIT "AlienNat.( * )_test2" = assert_true (AlienNat.( * ) 
	(AlienNat.zero) (AlienNat.nat_of_int(2)) = (AlienNat.zero))
TEST_UNIT "AlienNat.( * )_test3" = assert_true (AlienNat.( * ) 
	(AlienNat.one) (AlienNat.one) = AlienNat.one)
TEST_UNIT "AlienNat.( * )_test4" = assert_true (AlienNat.( * ) 
	(AlienNat.one) (AlienNat.( * ) (AlienNat.one) (AlienNat.one)) = 
	(AlienNat.one))
TEST_UNIT "AlienNat.( * )_test5" = assert_true (AlienNat.( * ) 
	(AlienNat.one) (AlienNat.( + ) (AlienNat.one) (AlienNat.one)) = 
	AlienNat.( + ) (AlienNat.one) (AlienNat.one))

(* test ( < ) *)

TEST_UNIT "AlienNat.( < )_test1" = assert_true ( (AlienNat.( < )) 
	(AlienNat.nat_of_int(2)) (AlienNat.nat_of_int(4)))
TEST_UNIT "AlienNat.( < )_test2" = assert_false ( (AlienNat.( < )) 
	(AlienNat.nat_of_int(3)) (AlienNat.nat_of_int(2)))
TEST_UNIT "AlienNat.( < )_test3" = assert_true ( (AlienNat.( < )) 
	(AlienNat.zero) (AlienNat.one))
TEST_UNIT "AlienNat.( < )_test4" = assert_false ( (AlienNat.( < )) 
	(AlienNat.one) (AlienNat.zero))

(* test (===) *)

TEST_UNIT "AlienNat.( === )_test1" = assert_true ( AlienNat.( === ) 
	(AlienNat.nat_of_int(2)) (AlienNat.nat_of_int(2)))
TEST_UNIT "AlienNat.( === )_test2" = assert_false (AlienNat.( === ) 
	(AlienNat.nat_of_int(3)) (AlienNat.nat_of_int(2)))
TEST_UNIT "AlienNat.( === )_test3" = assert_true (AlienNat.( === ) 
	(AlienNat.zero) (AlienNat.zero))
TEST_UNIT "AlienNat.( === )_test4" = assert_false (AlienNat.( === ) 
	(AlienNat.one) (AlienNat.zero))

(* test (int_of_nat) *)

TEST_UNIT "AlienNat.int_of_nat_test1" = assert_true 
    ((AlienNat.int_of_nat(AlienNat.nat_of_int(2))) = 2)
TEST_UNIT "AlienNat.int_of_nat_test2" = assert_true 
    ((AlienNat.int_of_nat(AlienNat.nat_of_int(45))) = 45)
TEST_UNIT "AlienNat.int_of_nat_test3" = assert_true 
    ((AlienNat.int_of_nat(AlienNat.zero)) = 0)

(* test (nat_of_int) *)

TEST_UNIT "AlienNat.nat_of_int_test1" = assert_true 
    (AlienNat.nat_of_int(2) = (AlienNat.( + ) (AlienNat.one) (AlienNat.one)))
TEST_UNIT "AlienNat.nat_of_int_test2" = assert_true 
    (AlienNat.nat_of_int(0) = AlienNat.zero)
TEST_UNIT "AlienNat.nat_of_int_test3" = assert_raises 
    (Some (AlienNat.Unrepresentable)) AlienNat.nat_of_int(-1)


let () = Pa_ounit_lib.Runtime.summarize ()