(* SML source file. Copyright (c) by 2222-42 2020.
*
*)

val x = 1;
val y = 2;
val x = x * 2 + y;
val y = x + y * 2;

(* 
expected: x = 4, y = 8 
*)


val x = 1;
val y = 2;
val x = x * 2 + y
and y = x + y * 2;

(*
expected: x = 4, y = 5 *)