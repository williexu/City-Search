(* BEGIN: DO NOT CHANGE THIS CODE, except for adding comments
   to NATN as required in exercise 1. *)
module type NATN = sig
  type t
  
  (** val zero:t is a value declaration in the NATN signature.
    * The name of the value declaration is zero and its type is t.
    * Pre-condition : Must be of type t
    * Post-condition : Must be of type t

    * zero is an identity element for addition, i.e, zero added to
    * any other value of type t returns a value that represents the same 
    * natural number.

    * zero multiplied by any other value of type t
    * returns zero.
    *)
  val zero : t
  
  (** val one:t is a value declaration in the NATN signature.
    * The name of the value declaration is one and its type is t.
    * Pre-condition : Must be of type t
    * Post-condition : Must be of type t

    * one is an identity element for multiplication, i.e, one multiplied by
    * any other value of type t returns a value representing that same natural
    * number.
    *)
  val one : t

  (** val ( + ) behaves like the operator '+'. It returns the sum of two values of type t.
    * Arguments : It takes in two arguments of type t.
    * Pre-condition : Both arguments must be of type t.
    * Post-condition : It returns the sum of its arguments, and the sum is of type t.

    * ( + ) is assosciative, i.e., (a+b)+c === a+(b+c) (the order of evaluation of
    * arguments does not matter.)
    *)
  val ( + ) : t -> t -> t

  (** val ( * ) behaves like the operator '*'. It returns the product of two values of type t.
    * Arguments : It takes in two arguments of type t.
    * Pre-condition : Both arguments must be of type t.
    * Post-condition : It returns the product of its arguments, and the product is of type t.

    * ( * ) is assosciative, i.e., (a*b)*c === a*(b*c) (the order of evaluation of
    * arguments does not matter.)

    * ( * ) is distributive over ( + ), i.e., a*(b+c) = a*b + a+c

    * if the output exceeds the max value of t, then raise Unrepresentable exception
    *)
  val ( * ) : t -> t -> t 

  (** val ( < ) behaves like the operator '<'. It returns a boolean value depending on which argument
    * passed to it is smaller.
    * Arguments : It takes in two arguments of type t.
    * Pre-condition : Both arguments must be of type t.
    * Post-condition : It returns a bool - true if first argument is amller than the second
    * and false otherwise.

    * if the output exceeds the max value of t, then raise Unrepresentable exception
    *)
  val ( < ) : t -> t -> bool

  (** val ( === ) behaves like the operator '='. It returns a boolean value depending on whether the
    * arguments passed to it are equal or not.
    * Arguments : It takes in two arguments of type t.
    * Pre-condition : Both arguments must be of type t.
    * Post-condition : It returns a bool - true if the arguments are equal and false otherwise.
    *)
  val ( === ) : t -> t -> bool
			    
  exception Unrepresentable
  
  (** This function is used to convert from a natural number to the int type.
    * Arguments : It takes in a value of type t (a natural number.)
    * Pre-condition : Argument must be of type t.
    * Post-condition : The functions returns the int representation of t if possible.
    * Not all natural numbers are representable with int (like 2^128 ),
    * so this function raises the Unrepresantable exception in such scenarios. 
    *)
  val int_of_nat: t -> int

  (** This function is used to convert from the int type number to a natural number.
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


module IntNat: NATN = struct
(** Each int represents the corresponding natural number n . Obviously, 
  * negative numbers would be an invalid natural number and large natural
  * numbers greater than max_int cannot be represented.
  *)
  type t = int
  exception Unrepresentable

  let zero = 0
  let one = 1

(** This function is used to add two natural numbers of type t (int).
  * Arguments : It takes in two arguments x,y of type t.
  * Pre-condition : Arguments must be of type t (int).
  * Post-condition : The functions returns the natural number representation 
  * of x + y if possible.
  * not all values of type int are representable as natural numbers (like -1),
  * and having a sum greater than max_int is not possible, therefore
  * this function raises the Unrepresantable exception in such scenarios. 
  *)
  let ( + ) (x: int) (y:int): int = 
    let sum_overflows ( i1 : int ) ( i2 : int ) : bool =
      (x + y) < 0 in
    if (sum_overflows x y) || (x < 0) || y < 0 then raise Unrepresentable 
    else x + y

(** This function is used to add multiply natural numbers of type t (int).
  * Arguments : It takes in two arguments x,y of type t.
  * Pre-condition : Arguments must be of type t (int).
  * Post-condition : The functions returns the natural number representation 
  * of x * y if possible.
  * not all values of type int are representable as natural numbers (like -1),
  * and having a product greater than max_int is not possible, therefore
  * this function raises the Unrepresantable exception in such scenarios. 
  *)
  let ( * ) (x:int) (y: int): int = 
    let product_overflows ( i1 : int ) ( i2 : int ) : bool =
      (max_int/x)/y = 0 in
    if x = 0 || y = 0 then 0 else
      if (product_overflows x y) || (x < 0) || y < 0 then raise Unrepresentable 
      else x * y

(** This function is used to compare natural numbers of type t (int), returning
  * true if the first argument is less than the second, false otherwise
  * Arguments : It takes in two arguments x,y of type t.
  * Pre-condition : Arguments must be of type t (int).
  * Post-condition : The functions returns a bool representating x < y if 
  * possible.
  * not all values of type int are representable as natural numbers (like -1),
  * therefore this function raises the Unrepresantable exception in this case.
  *)
  let ( < ) (x:int) (y:int): bool =
    if  (x < 0) || y < 0 then raise Unrepresentable 
    else x < y

(** This function is used to compare natural numbers of type t (int), returning
  * if the two numbers are equal
  * Arguments : It takes in two arguments x,y of type t.
  * Pre-condition : Arguments must be of type t (int).
  * Post-condition : The functions returns a bool representing x = y if
  * possible
  * not all values of type int are representable as natural numbers (like -1),
  * therefore this function raises the Unrepresantable exception in this case.
  *)
  let ( === ) (x:int) (y:int): bool=
    if  (x < 0) || y < 0 then raise Unrepresentable 
    else x = y

(** This function is used to convert a natural number of type t (int) into 
  * an int.
  * Arguments : It takes in one argument x of type t.
  * Pre-condition : Argument must be of type t (int).
  * Post-condition : The functions returns an int representing the argument.
  * a natural number cannot be used as an argument,
  * therefore this function raises the Unrepresantable exception in this case.
  *)
  let int_of_nat (x:int): int =
    if  (x < 0) then raise Unrepresentable 
    else x

(** This function is used to convert a natural number of type int into 
  * a natural number of type t (int).
  * Arguments : It takes in one argument x of type int.
  * Pre-condition : Argument must be of type int.
  * Post-condition : The functions returns an int representing the argument.
  * not all values of type int are representable as natural numbers (like -1),
  * therefore this function raises the Unrepresantable exception in this case.
  *)
  let nat_of_int (x:int): int =
    if  (x < 0) then raise Unrepresentable 
    else x
end


module ListNat: NATN = struct
(** The list [ a1 ; ...; an ] represents the
  * natural number n . That is , the list lst represents
  * length ( lst ). The empty list represents 0. The values of
  * the list elements are irrelevant.
  *)
  type t = int list
  exception Unrepresentable

  let zero = []
  let one = [0]

let add_list (z:t): int = (List.fold_left (fun acc s -> 
    let (new_acc: int)= 1 + acc in
      if (new_acc < 0) then raise Unrepresentable else new_acc) 0 z)

(** This function is used to add two natural numbers of type t (int list).
  * Arguments : It takes in two arguments x,y of type t.
  * Pre-condition : Arguments must be of type t (int list).
  * Post-condition : The functions returns the natural number representation 
  * of x + y.
  *)
  let ( + ) (x: int list) (y:int list): int list= 
    List.fold_left (fun acc z -> z:: acc) y x

(** This function is used to multiply two natural numbers of type t (int list).
  * Arguments : It takes in two arguments x,y of type t.
  * Pre-condition : Arguments must be of type t (int list). x and y cannot 
  * represent numbers greater than max_int
  * Post-condition : The functions returns the natural number representation 
  * of x * y if possible.
  * Since having a product greater than max_int or having arguments greater 
  * than max_int is not possible using this method,
  * therefore this function raises the Unrepresantable exception in this 
  * scenario. 
  *)
  let ( * ) (x:int list) (y: int list): int list=
    let product_overflows ( i1 : int list) ( i2 : int list) : bool =
      (max_int/(add_list x))/(add_list y) = 0 in
    if x = zero || y = zero then zero else
    let rec make_product (a: int)(b: int list): int list=
      if a = 0 then b
      else make_product (a-1) (0:: b) in
    if product_overflows x y then raise Unrepresentable
    else make_product((add_list x) * (add_list y)) []

(** This function is used to compare two natural numbers of type t (int list).
  * It returns a bool representing x < y where x and y are natural numbers.
  * Arguments : It takes in two arguments x,y of type t.
  * Pre-condition : Arguments must be of type t (int list). x and y cannot 
  * represent numbers greater than max_int
  * Post-condition : The functions returns a bool representing 
  * x < y if possible.
  * Having arguments greater than max_int will raise Unrepresentable
  *)
  let ( < ) (x:int list) (y:int list): bool =
    (add_list x) < (add_list y)

(** This function is used to compare two natural numbers of type t (int list).
  * It returns a bool representing x = y where x and y are natural numbers.
  * Arguments : It takes in two arguments x,y of type t.
  * Pre-condition : Arguments must be of type t (int list). x and y cannot 
  * represent numbers greater than max_int
  * Post-condition : The functions returns a bool representing 
  * x = y if possible.
  * Having arguments greater than max_int will return Unrepresentable
  *)
  let ( === ) (x:int list) (y:int list): bool=
    (add_list x) = (add_list y)

(** This function is used to convert a natural number of type t (int list) into
  * an int
  * Arguments : It takes in one argument x of type t.
  * Pre-condition : Arguments must be of type t (int list). x cannot 
  * represent numbers greater than max_int
  * Post-condition : The functions returns an int representing 
  * x if possible.
  * Having an argument greater than max_int will return Unrepresentable
  *)
  let int_of_nat (x:int list): int =
    add_list x

(** This function is used to convert an int into a natural number of type
  * t (int list)
  * Arguments : It takes in one argument x of type int.
  * Pre-condition : Arguments must be of type int.
  * Post-condition: The functions returns a natural number of type t (int list)
  * representing x if possible.
  *)
  let nat_of_int (x: int) : int list =
    let rec make_product (a: int)(b: int list): int list=
      if a = 0 then b
      else make_product (a-1) (0:: b) in
    if (x >= 0) then (make_product x [])
    else raise Unrepresentable 
end

(** This function takes in a module of NATN and uses the conversion methods
  * in said module
  * Arguments : It takes in a Module of type NATN.
  * Pre-condition : Argument must be module of type NATN.
  * Post-condition : The functions returns a structure utilizing the conversion
  * methods in the argument module.
  *)
module NatConvertFn ( N : NATN ) = struct
  let int_of_nat ( n : N . t ): int = N.int_of_nat n
  let nat_of_int ( n : int ): N . t = N.nat_of_int n
end

(* helper to allow int addition in module *)
let add_int (a: int)(b: int): int = a + b

module AlienNatFn ( M : AlienMapping ): NATN = struct
(** The list [ M.aliensym1 ; ...; M.aliensymn ] represents the
  * natural number (M.aliensym1 + M.aliensym2 + ... M.aliensymn). 
  * ( * ) can only be used to make a product <= max_int
  *)
  type t = M.aliensym list
  exception Unrepresentable

(** helper that sums the alien syms in the inputted list and returns the int 
  * equivalent. If at any time while adding, the elements added becomes
  * greater than max_int, it raises Unrepresentable 
  * Precondition: z is type t (M.aliensym list)
  * Postcondition: returns type int that is non-negative
  *)
  let add_list (z:t): int = (List.fold_left (fun acc s -> 
      let (new_acc: int)= add_int(M.int_of_aliensym s) acc in
        if (new_acc < 0) then raise Unrepresentable else new_acc) 0 z)

  let zero = []
  let one = [M.one] 

(** This function is used to add two natural numbers of type t.
  * Arguments : It takes in two arguments x,y of type t.
  * Pre-condition : Arguments must be of type t (M.aliensym list).
  * Post-condition : The function returns the natural number representation 
  * of x + y.
  *)
  let ( + ) (x: t) (y: t): t = 
    List.fold_left (fun acc s -> s:: acc) x y

(** This function is used to multiply two natural numbers of type t.
  * Arguments : It takes in two arguments x,y of type t.
  * Pre-condition : Arguments must be of type t (M.aliensym list).
  * Post-condition : The function returns the natural number representation 
  * of x * y. This method cannot return a product greater than max_int or take
  * in arguments representing numbers greater than
  * max_int so it raises Unrepresentable in these cases.
  *)
  let ( * ) (x: t) (y: t): t = 
    let rec make_product (a: int)(b: t): t=
      if a = 0 then b
      else make_product (a-1) (M.one:: b) in
    let (xint: int) = add_list x in
    let (yint: int) = add_list y in
    let product_overflows ( i1 : int ) ( i2 : int ) : bool =
      (max_int/i1)/i2 = 0 in
    if xint = 0 || yint = 0 then zero else
      if (product_overflows xint yint) then raise Unrepresentable 
      else make_product(xint * yint) []

(** This function is used to compare two natural numbers of type t, returning
  * true if the first is less than the second and false otherwise
  * Arguments : It takes in two arguments x,y of type t.
  * Pre-condition : Arguments must be of type t (M.aliensym list).
  * Post-condition : The function returns a bool representing x < y. 
  * This method cannot take in arguments representing numbers greater than
  * max_int so it raises Unrepresentable in these cases.
  *)
  let ( < ) (x:t) (y:t): bool =
    (add_list x) < (add_list y)

(** This function is used to compare two natural numbers of type t, returning
  * true if the first is equal to the second and false otherwise
  * Arguments : It takes in two arguments x,y of type t.
  * Pre-condition : Arguments must be of type t (M.aliensym list).
  * Post-condition : The function returns a bool representing x = y. 
  * This method cannot take in arguments representing numbers greater than
  * max_int so it raises Unrepresentable in these cases.
  *)
  let ( === ) (x:t) (y:t): bool=
    (add_list x) = (add_list y)

(** This function converts a natural number of type t into an int
  * Arguments : It takes in one argument x of type t.
  * Pre-condition : Argument must be of type t (M.aliensym list).
  * Post-condition : The function returns an int representing the argument
  * if possible. 
  * This method cannot take in an argument representing a number greater than
  * max_int so it raises Unrepresentable in this case.
  *)
  let int_of_nat (x:t): int =
    add_list x

(** This function converts an int into a natural number of type t
  * Arguments : It takes in one argument x of type int.
  * Pre-condition : Argument must be of type int.
  * Post-condition : The function returns a natural number of type t
  * representing the argument if possible. 
  * This method cannot take in an argument that is negative so it raises
  * Unrepresentable in this case.
  *)
  let nat_of_int (x:int): t =
    let rec make_product (a: int)(b: t): t=
      if a = 0 then b
      else make_product (a-1) (M.one:: b) in
    if (x >= 0) then (make_product x [])
    else raise Unrepresentable 
end


