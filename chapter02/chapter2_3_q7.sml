(* SML source file. Copyright (c) by 2222-42 2020.
* Q 2.7
*)

fun fib n = if n < 2 then n else fib (n - 1) + fib (n - 2);

fun G(n, f, g) = if n = 0 then f else if n = 1 then g else G(n - 1, g, f + g);

fun fastFib n = G(n, fib 0, fib 1);

(* 
筆者の回答例:
   fun G(n,fk,fk1) = if n = 0 then fk
                     else if n = 1 then fk1
                     else G(n-1,fk1,fk+fk1);
   fun fastFib n = G(n,0,1);

確かに、私の回答の`G`の第二引数`fib 0`と第三引数`fib 1`はただ0と1を出力しているだけで、`fib`の計算を導入する意味はない。
*)