(* BEGIN: DO NOT CHANGE THIS CODE, except for adding comments
   to NATN as required in exercise 1. *)
module type NATN = sig
  type t
  
  (* val zero:t is a value declaration in the NATN signature.
    * The name of the value declaration is zero and its type is t.
    * Pre-condition : Must be of type t
    * Post-condition : Must be of type t

    * zero is an identity element for addition, i.e, zero added to
    * any other value of type t returns the value.

    * zero multiplied by any other value of type t
    * returns zero.
  *)
  val zero : t
  
  (* val one:t is a value declaration in the NATN signature.
    * The name of the value declaration is one and its type is t.
    * Pre-condition : Must be of type t
    * Post-condition : Must be of type t

    * one is an identity element for multiplication, i.e, one multiplied by
    * any other value of type t returns the value.
  *)
  val one : t

  (* val ( + ) behaves like the operator '+'. It returns the sum of two values of type t.
    * Arguments : It takes in two arguments of type t.
    * Pre-condition : Both arguments must be of type t.
    * Post-condition : It returns the sum of its arguments, and the sum is of type t.

    * ( + ) is assosciative, i.e., (a+b)+c === a+(b+c) (the order of evaluation of
    * arguments does not matter.)
  *)
  val ( + ) : t -> t -> t

  (* val ( * ) behaves like the operator '*'. It returns the product of two values of type t.
    * Arguments : It takes in two arguments of type t.
    * Pre-condition : Both arguments must be of type t.
    * Post-condition : It returns the product of its arguments, and the product is of type t.
    * If the sum exceeds the maximum possible value of t, it returns the Unrepresentable exception.

    * ( * ) is assosciative, i.e., (a*b)*c === a*(b*c) (the order of evaluation of
    * arguments does not matter.)

    * ( * ) is distributive over ( + ), i.e., a*(b+c) = a*b + a+c
  *)
  val ( * ) : t -> t -> t 

  (* val ( < ) behaves like the operator '<'. It returns a boolean value depending on which argument
    * passed to it is smaller.
    * Arguments : It takes in two arguments of type t.
    * Pre-condition : Both arguments must be of type t.
    * Post-condition : It returns a bool - true if first argument is smaller than the second
    * and false otherwise.
    * If the product exceeds the maximum possible value of t, it returns the Unrepresentable exception.
  *)
  val ( < ) : t -> t -> bool

  (* val ( === ) behaves like the operator '='. It returns a boolean value depending on whether the
    * arguments passed to it are equal or not.
    * Arguments : It takes in two arguments of type t.
    * Pre-condition : Both arguments must be of type t.
    * Post-condition : It returns a bool - true if the arguments are equal and false otherwise.
  *)
  val ( === ) : t -> t -> bool
			    
  exception Unrepresentable
  
  (* This function is used to convert from a natural number to the int type.
    * Arguments : It takes in a value of type t (a natural number.)
    * Pre-condition : Argument must be of type t.
    * Post-condition : The functions returns the int representation of t if possible.
    * Not all natural numbers are representable with int (like 2 128 ),
    * so this function raises the Unrepresantable exception in such scenarios. *)
  val int_of_nat: t -> int

  (* This function is used to convert from the int type number to a natural number.
    * Arguments : It takes in a value of type int.
    * Pre-condition : Argument must be of type int.
    * Post-condition : The functions returns the natural number representation of int if possible.
    * not all values of type int are representable as natural numbers (like -1),
    * so this function raises the Unrepresantable exception in such scenarios. *)
  val nat_of_int: int -> t
end

module type AlienMapping = sig
  type aliensym

  val int_of_aliensym: aliensym -> int
  val one: aliensym
  val zero: aliensym
end

type sign = Positive | Negative
let sign_int (n:int) : sign = 
  if n >= 0 then Positive else Negative
let sum_overflows (i1:int) (i2:int) : bool = 
  sign_int i1 = sign_int i2 && sign_int(i1 + i2) <> sign_int i1
(* END: DO NOT CHANGE THIS CODE *)

(* Add your solution here for IntNat, ListNat, NatConvertFn, 
   and AlienNatFn, being careful to use the declarations and
   types specified in the problem set. *)


(* IntNat *)
module IntNat : NATN = struct
type t = int

let zero = 0
let one = 1
exception Unrepresentable

let ( + ) (x:int) (y:int) : int = 
     if ((x+y)<0) then raise Unrepresentable else (x+y)

let ( * ) (x:int) (y:int) : int = 
       if (((max_int/x)/y)=0) then raise Unrepresentable else (x*y)

let ( < ) (x:int) (y:int) : bool = (x<y)

let ( === ) (x:int) (y:int) : bool = (x=y)



let int_of_nat (x:t) : int =
    if (x>max_int) then raise Unrepresentable else x

let nat_of_int (y:int) : t =
    if (y<0) then raise Unrepresentable else y

end

(* ListNat *)
module ListNat : NATN = struct
(* The list [ a1 ; ...; an ] represents the
* natural number n . That is , the list lst represents
* length ( lst ). The empty list represents 0. The values of
* the list elements are irrelevant . *)
type t = int list

let zero = []
let one = [1]

exception Unrepresentable

let ( + ) (l1:int list) (l2:int list) : int list =
    List.fold_left (fun acc x -> x::acc) l2 l1

(*check*)
let ( * ) (l1:int list) (l2:int list) : int list = failwith "hi"
    (*match l2 with
    [] -> []
    |h::t ->
    let rec make_product_list (l1:int list) (l2:int list)
    List.fold_left (fun acc x -> List.fold_left (fun acc1 y -> y::acc1) [] l1) [] l2*)

let ( < ) (l1:int list) (l2:int list) : bool =
    (List.length(l1)<List.length(l2))

let ( === ) (l1:int list) (l2:int list) : bool =
    (List.length(l1)=List.length(l2))

let int_of_nat (l1:int list) : int =
    List.length(l1)

let nat_of_int (y:int) : int list =
      let rec make_nat_list (l:int list)(x:int) : int list =
         if (x=0) then [] else make_nat_list (0::l) (x-1) 
      in
         if (y>=0) 
         then make_nat_list [] y
         else raise Unrepresentable


end


