(* SML source file. Copyright (c) by 2222-42 2020.
* Q 3.7
*)

(* 1 *)
fun f x y z = x y z : int ;
(* 
expect: ('a -> 'b -> int) -> 'a -> 'b -> int
*)

(* 2 *)
fun f x y z = x (y z): int;
(* 
expect: ('a -> int) -> ('b -> 'a) -> 'b -> int
*)

(* 3 *)
fun f x y z = (x y z) : int ;
(* 
expect: ('a -> 'b -> int) -> 'a -> 'b -> int
*)

(* 4 *)
fun f x y z = x y (z : int) ;
(* 
expect: ('a -> int -> 'b) -> 'a -> int -> 'b
*)

(* 5 *)
fun f x y z = x (y z : int);
(* 
expect: (int -> 'a) -> ('b -> int) -> 'b -> 'a
*)
