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

(* 筆者の解答(なぜかfoldrを使っていない) *)
fun filter P nil = nil
  | filter P (h::tl) =
    if P h then h :: filter P tl else filter P tl;
fun targetOfByAuthor (R,a) = filter (fn (x,y) => x = a) R;

(* 3 *)
fun sourceOf R a = foldr (fn ((x,y), r) => if y = a then (x,y)::r else r) [] R;

sourceOf [(1,2), (2,3), (2,4), (3,4)] 4;
sourceOf [(1,2), (2,3), (2,4), (3,4)] 2;

(* 4 *)
fun inverseRel R = foldr (fn ((x,y), r) => (y,x)::r) [] R;
inverseRel [(1,2), (2,3), (2,4), (3,4)];

(* 筆者の解答 *)
fun inverseRelByAuthor R = map (fn (a,b) => (b, a)) R;
inverseRelByAuthor [(1,2), (2,3), (2,4), (3,4)];
