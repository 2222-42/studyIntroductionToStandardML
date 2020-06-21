fun F x = if x >= 1000.0 then 0.02 else (0.00001 * x) + 0.01;

(*
- A(10.0, 1);
これでもう落ちるから、多分実装がダメ。
*)
fun I(x, n) = F(A(x, n))
and A(x, n) = if n = 0 then x
              else A(x,n-1)*(1.0+I(x,n));