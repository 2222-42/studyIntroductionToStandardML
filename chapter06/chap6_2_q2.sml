(* SML source file. Copyright (c) by 2222-42 2020.
* Q6.2
*)

(* fun L1 a = a :: a; *)
(* 
expect: operator and operand do not agree 
a :: aにおいて、後ろのLがaの型'aの'a listになっていないから。

a が nilであったらセーフかな？
- fun L1 nil = nil :: nil; 
stdIn:9.5-9.24 Warning: match nonexhaustive
          nil => ...

val L1 = fn : 'a list -> 'b list list
*)

fun L2 a = a :: [a];
(* 
expect: success
L2 : 'a -> ('a * 'a list)

result: 
val L2 = fn : 'a -> 'a list
*)

fun L3 (a, b) = a :: b;
(* 
expect: bが'a listになっていると型推論すると思うから成功する
val L3 = fn : 'a * 'a list -> 'a list
*)

(* fun L4 (a, b) = (a::b, b::a); *)
(* 
expect: operatorとoperand が一致しない。

a::bより'a listになっていると型推論し、aの型は'aとなる。
b::aより'b listになっていると型推論し、bの方は'bとなる。
これによって、相互循環に陥り、型が定まらない。

resul:
Error: operator and operand do not agree [circularity]
  operator domain: 'Z list * 'Z list list
  operand:         'Z list * 'Z
  in expression:
    b :: a

おおよそあっているが、a::bの型推論の結果をそのままb::aに適用して、一致しないことになった。
*)

fun L5 (a,b) = [a] :: b;
(* 
expect:
val L5 = fn : ('a list * 'a list list) -> 'a list list

result:
val L5 = fn : 'a * 'a list list -> 'a list list
*)

fun L6 (a, b) = a :: [b];
(* 
expect:
val L6 = fn : ('a * 'a) -> 'a list
*)

fun L7 (a, b) = (a, b)::[(a, b)];
(* 
expect:
val L7 = fn : ('a * 'b) -> ('a * 'b) list
*)