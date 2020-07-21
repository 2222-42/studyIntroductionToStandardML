(* SML source file. Copyright (c) by 2222-42 2020.
* Q 2.10
*)

fun F x = if x < 1000.0 then 0.01 
          else if x > 2000.0 then 0.02 
          else (0.00001 * (x - 1000.0)) + 0.01;

(*
- A(10.0, 1);
これでもう落ちるから、多分実装がダメ。
-> Iの計算で無限に循環していたのが原因
*)
fun I(x, n) = F(A(x, n - 1))
and A(x, n) = if n = 0 then x
              else A(x,n-1)*(1.0+I(x,n));

A(900.0, 10);
A(900.0, 20);
A(900.0, 25);
A(900.0, 30);