(* SML source file. Copyright (c) by 2222-42 2020.
* Q6.6
*)

(* 1 *)
fun sumList xs = case xs of nil => 0
                            | (h::t) => 
                                h + sumList t;

sumList [1,2,3,4,5];

(* 2 *)
fun member list x = case list of nil => false
                            | (h::t) => if h = x then true else member t x;

member ["a", "b", "c", "d"] "c";
member ["a", "b", "c", "d"] "e";

(* 3 *)
fun unique list = case list of nil => []
                        | (h::t) => if member t h then unique t else (h :: (unique t));

unique [1,2,3,4,5];
unique [1,2,1,2];
unique [1,1,1,1,1];

(* 4 *)
fun filter (P: 'a -> bool) (list: 'a list) 
    = case list of nil => []
                | (h::t) => if (P h) then h::(filter P t) else filter P t;

fun MultiOf3 x = (x mod 3 = 0);
filter MultiOf3 [1,2,3,4,5,6,7];

(* 5 *)
fun flatten list = case list of nil => []
                            | (h::t) => h@(flatten t);

flatten [[1], [1,2], [1,2,3]];
(* 
リストのリストのリストは考えなくてよい
flatten [[1], [1,2], [[1,2,3], [1,2,3,4]]]; *)

(* 6 *)

fun splice (list, string) = 
    case list of nil => ""
                | (h::t) => (if null t then h else h ^ string) ^ (splice (t,string));

splice(["", "home", "ohori", "papers", "mltext"], "/")
(* val it = "/home/ohori/papers/mltext/" : string 
となるのを避けるために、微妙な条件分岐を設けてしまった。
*)