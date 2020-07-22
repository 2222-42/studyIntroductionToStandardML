(* SML source file. Copyright (c) by 2222-42 2020.
* Q 2.16
*)

fun summation' f n = if n <= 0 then 0.0
                     else f n + summation' f (n - 1);