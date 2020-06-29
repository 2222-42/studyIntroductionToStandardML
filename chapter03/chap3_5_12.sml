(* SML source file. Copyright (c) by 2222-42 2020.
* Q 3.12
*)

fun twice f x = f (f x);
val fourTimes = fn f => twice twice f;
fun addOne x = x + 1;
fourTimes addOne 1;   
(* val it = 5 : int *)

fn f => twice fourTimes f;
(* 
expect: 左結合だから、Warningが発生する。

result:
- fn f => twice fourTimes f;
val it = fn : ('a -> 'a) -> 'a -> 'a

ちゃんと、関数式にしているからエラーは起きないんですよ。
*)
val test1 = fn f => twice fourTimes f;
test1 addOne 0;
(* val it = 16 : int *)
(* 
筆者の回答：
 twice fourTimes f x
は，その定義から， 
 fourTimes (fourTimes f) x 
である．
さらにfourTimesの定義にしたがって展開すると 
 (fourTimes f) ((fourTimes f) ((fourTimes f) (fourTimes f x))) 
となる．このことから，
twice fourTimes f x はfをxに4^2回適用する関数となることがわかる
*)

fn f => fourTimes twice f;
(* 
expect: 8回適用する関数

result:
val it = fn : ('a -> 'a) -> 'a -> 'a

4回適用するを2回適用するから、与えられたf を16回適用する。
*)

val test2 = fn f => fourTimes twice f;
test2 addOne 0;
(* val it = 16 : int *)

