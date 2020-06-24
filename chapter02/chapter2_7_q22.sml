(* SML source file. Copyright (c) by 2222-42 2020.
*
*)

(* 
int -> bool型の関数は、条件を満たす整数の集合の表現とみなすことができる。
- `orelse`: 論理和を表す
- `andalso`: 論理積を表す 
*)

(* 1 *)
val emptySet = fn x => false;

(* 2 *)
fun singleton n = fn x => x = n;

(* 3 *)
fun insert S n = fn x => S x orelse x = n;

val Sample = fn x => x = 1 orelse x = 2 orelse x = 3; 
Sample 3;
Sample 4;
val Sample = insert Sample 4;
Sample 3;
Sample 4;

(* 4 *)
fun member S n = S n;

(* 5 *)
fun union S1 S2 = fn x => S1 x orelse S2 x;
fun intersection S1 S2 = fn x => S1 x andalso S2 x;
fun difference S1 S2 = fn x => S1 x andalso not (S2 x);

(* Sample1: {1, 2}, Sample2: {3} *)
val Sample1 = fn x => x = 1 orelse x = 2;
val Sample2 = fn x => x = 3;

(* unitedSample: {1, 2, 3}, intersectedSample: {1, 2}, differedSample: {3} *)
val unitedSample = union Sample1 Sample2;
val intersectedSample = intersection unitedSample Sample1;
val differedSample = difference unitedSample intersectedSample;

unitedSample 1;
unitedSample 2;
unitedSample 3;
unitedSample 4;
intersectedSample 1;
intersectedSample 2;
intersectedSample 3;
differedSample 2;
differedSample 3;