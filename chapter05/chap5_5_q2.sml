(* SML source file. Copyright (c) by 2222-42 2020.
* 
*)

val {1=x,3={2=y,...},...} = (1,2,(3,4,5),6);
(* 
expect:
val x = (1,2)
val y = 3

result:
val x = 1 : int
val y = 4 : int

-  (1,2,(3,4,5),6);
val it = (1,2,(3,4,5),6) : int * int * (int * int * int) * int
- #1 (1,2,(3,4,5),6);
val it = 1 : int
- #3 (1,2,(3,4,5),6);
val it = (3,4,5) : int * int * int
*)

(#2 o #3) (1, 2,(3,4,5), 6);
(* 
expect:
4
*)