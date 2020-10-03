(* SML source file. Copyright (c) by 2222-42 2020.
* Q 3.10
*)

(* 
- fun higherTwice f (x: 'a -> 'a) = f (f x)
val higherTwice = fn : (('a -> 'a) -> 'a -> 'a) -> ('a -> 'a) -> 'a -> 'a
*)

(* fun id x = (if true then x else exp_t); *)
fun K x y = x;

(* 
- fun nonDeclaredHigherTwice f x = f (f (fn x => x)); 
val nonDeclaredHigherTwice = fn : (('a -> 'a) -> 'a -> 'a) -> 'b -> 'a -> 'a
*)
(* 
fun nonDeclaredHigherTwice f x = f (f (fn y => K x y));
val nonDeclaredHigherTwice = fn : (('a -> 'b) -> 'a -> 'b) -> 'b -> 'a -> 'b 
*)

fun nonDeclaredHigherTwice f x = f (f (fn y => K x (if true then y else x)));
(* val nonDeclaredHigherTwice = fn : (('a -> 'a) -> 'a -> 'a) -> 'a -> 'a -> 'a *)

(* 筆者の回答: *)
fun id x = x
fun higherTwice f x = f (f (K x (fn y => (y x, y id))));
