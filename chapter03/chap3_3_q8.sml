(* SML source file. Copyright (c) by 2222-42 2020.
* Q 3.8
*)

fun K x y = x;

fun f x = K x (fn y => x (x 1));
(* 
関数の動作
K x y = x
であるから、関数fの返り値の型はxのと同じになる。
よって、xの型をXとすると、fの型はX -> Xとなる。

型の説明
右の(fn y => x (x 1))より
- x: int -> int
よって、
- expect: (int -> int) -> (int -> int)
*)