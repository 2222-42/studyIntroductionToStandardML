(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 8.4
*)

val result11 = (fn x => x = x) 1;
val result12 = 1 = 1;

val result21 = (fn x => x = x) (ref 1);
val result22 = (ref 1) = (ref 1) ;
(* false *)
