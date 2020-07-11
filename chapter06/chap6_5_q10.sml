(* SML source file. Copyright (c) by 2222-42 2020.
* Q6.10
*)

(* 

fun foldr f Z nil = Z
    | foldr f Z (h::t) = f(h, foldr f Z t)
foldr f Z [a1, a2, .. an] = f (a1, f (a2, f(..., f(an, Z)...)))

foldl f Z [a1, a2, .. an] = f (an, f (a(n-1), f(..., f(a1, Z)...)))
*)


(* どうやれば末尾を取るか、 *)

(* 
http://www.cs.cornell.edu/courses/cs312/2008sp/recitations/rec05.html
fun foldl (f: 'a*'b->'b) (acc: 'b) (l: 'a list): 'b =
  case l of
    [] => acc
  | x::xs => foldl f (f(x,acc)) xs
*)
fun foldl f Z nil = Z
    | foldl f Z (h::t) = foldl f (f(h, Z)) t;

(*
fun myRev xs = if null xs then []
                else 
                    let 
                        val h = hd xs
                        val tail = tl xs
                    in
                        (myRev tail) Conc [h]
                    end;
*)
fun rev L = foldl (fn (h, R) => h::R) [] L;

rev [1,2,3];
(* 
expected: [3,2,1]
*)