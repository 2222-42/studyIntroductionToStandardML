(* SML source file. Copyright (c) by 2222-42 2020.
* Q6.12
*)

(* 1 *)
fun member L x = foldr (fn (h, R) => ((h = x) orelse R)) false L;

fun isRelated R (a,b) = member R (a, b);

(* 2 *)
fun targetOf R a = foldr (fn ((x,y), r) => if x = a then (x,y)::r else r) [] R;

targetOf [(1,2), (2,3), (2,4), (3,4)] 1;
targetOf [(1,2), (2,3), (2,4), (3,4)] 2;

(* 3 *)
fun sourceOf R a = foldr (fn ((x,y), r) => if y = a then (x,y)::r else r) [] R;

sourceOf [(1,2), (2,3), (2,4), (3,4)] 4;
sourceOf [(1,2), (2,3), (2,4), (3,4)] 2;

(* 4 *)
fun inverseRel R = foldr (fn ((x,y), r) => (y,x)::r) [] R;
inverseRel [(1,2), (2,3), (2,4), (3,4)];
inverseRel [(1,2), (2,3), (2,4), (3,4)];
