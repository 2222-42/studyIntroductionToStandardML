(* SML source file. Copyright (c) by 2222-42 2020.
* 2.18
*)

(* 1 *)
fun accumulate h z f n = 
    if n = 0 then z
    else h (f (n), accumulate h z f (n-1))

(* 2 *)
(* fun summation f n = if n = 1 then f 1
                    else f n + summation f (n - 1); *)
fun summation f n = accumulate (op +) 0;

(* 3 *)
fun returnInput x = x;
fun sumUpTo n = accumulate (op +) 0 returnInput n;
fun timesUpTo n = accumulate (op *) 1 returnInput n;

fun power m n = if m = 0 then 1
                else n * power (m - 1) n;
fun heihou n x = n * (power n x);
fun f3 n x = accumulate (op +) 0 (heihou x) n;