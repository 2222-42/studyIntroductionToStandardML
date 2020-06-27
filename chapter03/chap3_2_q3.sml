(* SML source file. Copyright (c) by 2222-42 2020.
*
*)

fun twice f x = f (f x);
(*  ('a -> 'a) -> 'a -> 'a *)

fun power m n = if m = 0 then 1
                         else n * power (m - 1) n;

val cube = power 3;
(* int -> int *)

fun id x = x;

twice cube; 
(*

expect: (int -> int) -> int -> int

- cube: int -> int
- twice: (int -> int) -> int -> int

So, the result is (`cube twice`);

    val it = fn : int -> int
*)

fn x => twice id x;
(* 

- x: 'a
- id: ('a -> 'a)
- twice ('a -> 'a) -> 'a -> 'a 

expect: 'a -> 'a
val it = fn : 'a -> 'a
*)

fun thrice f x = f (f (f x));
(*  
expect: ('a -> 'a) -> ('a -> 'a) -> 'a -> 'a

- Input1: 'a -> 'a
- Input2: 'a

val thrice = fn : ('a -> 'a) -> 'a -> 'a
*)