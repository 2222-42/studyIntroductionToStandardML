(* SML source file. Copyright (c) by 2222-42 2020.
* 
*)

(* 
組や組パターンを表す式や型、およびパターンは、
数字をラベルとするレコードの略記法
*)

val p = (2,3);
fun f (x, y) = x + y;
f p;

(* 
略記法であるから、組に対しても通常のれこー表記による操作を行うことができる
*)

#2 p;
#1 p;
(* #3 p; 
stdIn:1.2-1.6 Error: operator and operand do not agree [record labels]
  operator domain: {3:'Y; 'Z}
  operand:         int * int
  in expression:
    (fn {3=3,...} => 3) p
*)

val {1=y, 2=x} = p;