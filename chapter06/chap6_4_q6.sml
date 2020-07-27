(* SML source file. Copyright (c) by 2222-42 2020.
* Q6.6
*)

(* 1 *)
fun sumList xs = case xs of nil => 0
                            | (h::t) => 
                                h + sumList t;

sumList [1,2,3,4,5];

(* 筆者の解答 (caseを使わない版)*)
fun sumListByAuthor nil = 0
  | sumListByAuthor (h::t) = h + sumListByAuthor t;

(* 2 *)
fun member list x = case list of nil => false
                            | (h::t) => if h = x then true else member t x;

member ["a", "b", "c", "d"] "c";
member ["a", "b", "c", "d"] "e";

(* 筆者の解答 (caseを使わない版)*)
fun memberByAuthor a nil = false
  | memberByAuthor a (h::tl) = a = h orelse memberByAuthor a tl;
(* orelse使っている。確かに、評価する量としても一緒になるから、等しい *)

(* 3 *)
fun unique list = case list of nil => []
                        | (h::t) => if member t h then unique t else (h :: (unique t));

unique [1,2,3,4,5];
unique [1,2,1,2];
unique [1,1,1,1,1];

(* 筆者の解答 (caseを使わない版)*)
fun uniqueByAuthor nil = nil
  | uniqueByAuthor (h::tl) =
    let
      val tl' = uniqueByAuthor tl
    in
      if memberByAuthor h tl' then tl' else h::tl'
    end;

(* 4 *)
fun filter (P: 'a -> bool) (list: 'a list) 
    = case list of nil => []
                | (h::t) => if (P h) then h::(filter P t) else filter P t;

fun MultiOf3 x = (x mod 3 = 0);
filter MultiOf3 [1,2,3,4,5,6,7];

(* 筆者の解答 (caseを使わない版)*)
fun filterByAuthor P nil = nil
  | filterByAuthor P (h::tl) =
    if P h then h :: filterByAuthor P tl else filterByAuthor P tl;

(* 5 *)
fun flatten list = case list of nil => []
                            | (h::t) => h@(flatten t);

flatten [[1], [1,2], [1,2,3]];
(* 
リストのリストのリストは考えなくてよい
flatten [[1], [1,2], [[1,2,3], [1,2,3,4]]]; *)

(* 筆者の解答 (caseを使わない版)*)
fun flattenByAuthor nil = nil
  | flattenByAuthor (h::tl) = h @ flattenByAuthor tl;

(* 6 *)

fun Mysplice (list, string) = 
    case list of nil => ""
                | (h::t) => (if null t then h else h ^ string) ^ (Mysplice (t,string));

Mysplice(["", "home", "ohori", "papers", "mltext"], "/");
(* val it = "/home/ohori/papers/mltext/" : string 
となるのを避けるために、微妙な条件分岐を設けてしまった。
*)

fun splice (nil, string) = ""
  | splice ((h::tl), string) = (if null tl then h else h ^ string) ^ (splice (tl,string));
Mysplice(["", "home", "ohori", "papers", "mltext"], "/");