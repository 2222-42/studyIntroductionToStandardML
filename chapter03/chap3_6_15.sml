(* SML source file. Copyright (c) by 2222-42 2020.
* Q 3.15
*)

fun fib n = if n < 2 then n else fib (n - 1) + fib (n - 2);

fun fibPair (x, y) = (fib x, fib y);

fun memo f x = let val a = f x
               in fn y => if x = y then a else f y
               end;

(* memo fibPair (2, 3); *)
(* 
unexpected retun:
    val it = fn : int * int -> int * int 

- memo;
val it = fn : (''a -> 'b) -> ''a -> ''a -> 'b
- fibPair;
val it = fn : int * int -> int * int
- memo fibPair;
val it = fn : int * int -> int * int -> int * int
*)

(* 
val fibPair = memo fibPair (34, 35);
fibPair(34,35);
fibPair(35,34);
 *)
