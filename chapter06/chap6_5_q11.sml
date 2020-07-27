(* SML source file. Copyright (c) by 2222-42 2020.
* Q6.11
*)

(* 1 *)

val R = [(1,2), (2,3), (2,4), (3,4)];
(* 
- tc [(1,2), (2,3), (2,4), (3,4)];
val it = [(1,4),(1,3),(1,4),(2,4),(1,2),(2,3),(2,4),(3,4)] : (int * int) list
*)

(* 2 *)

fun member L x = foldr (fn (h, R) => ((h = x) orelse R)) false L;
(* fun member L x = foldr (fn (h, R) => (h = x)) false []; *)

fun unique L = foldr (fn (h, R) => if member R h then R else h::R) [] L;

fun timesRel (R, S) = 
    foldr (fn ((x,a), r) =>
            foldr (fn ((b, y), rs) => 
                if a = b then (x,y)::rs else rs)
            r S
            )
        nil R;

fun powerRel r 1 = r
    | powerRel r n = timesRel (r, powerRel r (n-1));

fun accumulate h z f n = 
    if n = 0 then z
    else h (f (n), accumulate h z f (n-1));

(*
正規形だったら正規形を返すようにするために、
uniqueを使った。あまりよくないかもしれない。
*)
fun tc R = accumulate (fn (x,y) => unique (x@y)) nil (powerRel R) (length R);

tc [(1,2), (2,3), (2,4), (3,4)];

(* 
val it = [(1,3),(1,4),(1,2),(2,3),(2,4),(3,4)] : (int * int) list
val it = [(1,3),(1,2),(2,3)] : (int * int) list
1->2->3->4 : 1-> 4
1->2->4 : 1-> 4
*)

tc [(1,2), (1,2), (2,3)];

(* 
1->2->3 : 1 -> 3
1->2->3 : 1 -> 3
*)

(* 筆者の解答: *)
fun originalTc R = accumulate (op @) nil (powerRel R) (length R);
fun normalTc R = unique (originalTc R);
originalTc [(1,1),(1,2),(2,3)];
normalTc [(1,1),(1,2),(2,3)];

(* 回答者のコメント:
筆者もunique使ってるし、大丈夫でしょ
*)