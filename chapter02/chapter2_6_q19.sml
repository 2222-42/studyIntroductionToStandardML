(* SML source file. Copyright (c) by 2222-42 2020.
* q 19
*)

(* 
fun power m n = if m = 0 then 1
                else n * power (m - 1) n;

fun Power (m, n) = if m = 0 then 1
                    else n * Power(m-1, n); 
*)

(* 1 *)

fun power m n = if m = 0 then 1
                else n * power (m - 1) n;

fun Power(m, n) = power m n;

fun Power' (m, n) = if m = 0 then 1
                    else n * Power'(m-1, n); 

fun power' m n = Power'(m, n);

(* 2 *)

(* curry: 'a * 'b -> 'c into 'a -> 'b -> 'c *)
fun curry f x y = f (x, y)
(* uncurry: 'a -> 'b -> 'c into 'a * 'b -> 'c *)
fun uncurry f (x, y) = f x y

(* 3 *)
val curriedPower = curry Power;
val uncurriedpower = uncurry power;