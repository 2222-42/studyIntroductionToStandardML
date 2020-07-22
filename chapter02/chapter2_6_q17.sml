(* SML source file. Copyright (c) by 2222-42 2020.
* Q 2.17
*)

(* f x : real -> real *)
fun summation' f n = if n <= 0 then 0.0
                     else f n + summation' f (n - 1);

(* fun summationReal f n = if n <= 0 then 0.0
                     else f n + summationReal f (n - 1); *)

fun integral f n a b = 
    let
        val unit = (b - a)  / real n
        fun G x = f(a + real x * unit ) * unit
    in
        summation' G n
    end;

fun cube x:real = x * x * x;

val result1 = integral cube 1000 0.0 1.0;
val result2 = integral cube 10000 0.0 1.0;
val result3 = integral cube 100000 0.0 1.0;