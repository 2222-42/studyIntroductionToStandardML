(* SML source file. Copyright (c) by 2222-42 2020.
* Q 2.15
*)

fun summation f n = if n = 1 then f 1
                    else f n + summation f (n - 1);

fun power m n = if m = 0 then 1
                else n * power (m - 1) n;

fun sumUpTo n = summation (power 1) n;
(* 筆者の回答は以下。結果に違いはないが、こちらの方がシンプル。 *)
fun f1 n = summation (fn x => x) n;

fun correctSquare n = summation (power 2) n;

fun sumOfSumUpTo n = summation sumUpTo n;