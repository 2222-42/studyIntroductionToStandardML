(* SML source file. Copyright (c) by 2222-42 2020.
* Q 2.5
*)

fun fib n = if n < 2 then n else fib (n - 1) + fib (n - 2);
(* 
筆者の回答例:
   fun fib n = if n = 0 then 0
               else if n = 1 then 1
               else fib (n - 1) + fib (n - 2)

本質的には一緒
*)