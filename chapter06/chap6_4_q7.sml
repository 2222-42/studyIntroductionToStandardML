(* SML source file. Copyright (c) by 2222-42 2020.
* Q6.7
*)

(* 1 *)
(* fun prefixList L = 
    let 
        fun subConcat x list = 
            case list of nil => [[x]]
                        | (h::t) => [x::h, subConcat h t]
    in case L of nil => []
            | [x] => [[x]]
            | (h::t) => 
                let val PFl = prefixList t
                in map (fn l => subConcat h l) PFl
                end
    end; *)

fun prefixList L = 
     case L of nil => [[]]
            | [x] => [[x]]
            | (h::t) => 
                map (fn x => h::x) ([] :: prefixList t);
prefixList [1,2,3];
(* 
expected:
val it = [[1], [1,2], [1,2,3]]: int list list 

result:
val it = [[1],[1,2],[1,2,3]] : int list list
*)

(* fun member list x = case list of nil => false
                            | (h::t) => if h = x then true else member t x;

fun suffixList L =
    let 
        fun subFunc h list = 
                if member list h then 
    in 
        case L of nil => [[]]
                | (h::t) => 
                    let val SLl = suffixList t
                    in 
                    end
    end; *)

fun suffixList L = 
    case L of nil  => [[]]
            | (h::t) => 
                let val Sll = suffixList t
                in (h::(hd Sll)) :: Sll
                end;

suffixList [1,2,3];
(* val it = [[1,2,3],[2,3],[3],[]] : int list list *)

(* 2 *)

fun allIntervals L =
    case L of nil => []
        | (h::t) => (prefixList (h::t)) @ (allIntervals t);

allIntervals [1,2,3];
(* 
expect:
val it [[1], [1,2], [1,2,3], [2], [2,3], [3]]

val it = [[1],[1,2],[1,2,3],[2],[2,3],[3]] : int list list

failed case:
fun allIntervals L =
    case L of nil => []
        | (h::t) => (prefixList (h::t)) @ (prefixList t);
val it = [[1],[1,2],[1,2,3],[2],[2,3]] : int list list
*)

(* 
課題点：筆者が使えといっていた関数のうち1つしか使っていないので、別解が色々あると思われる。
*)

(* 3 *)
fun powerSet L = 
    case L of nil => []
            | [x] => [[x]]
            | (h::t) =>
                let val subSet = (powerSet t)
                in 
                    (map (fn x => h::x) ([]::subSet)) @ subSet
                end;

powerSet [1,2,3];
(* 
val it = [[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]] : int list list

順序を変えても問題ないようにしたい？
*)

(* 4 *)
fun flatten list = case list of nil => []
                            | (h::t) => h@(flatten t);

fun permutations L = 
   let fun insert s nil = [[s]]
         | insert s (h::t) = 
               let val L = insert s t
               in (s::(h::t)) :: (map (fn x => h::x)L)
               end
   in case L of nil => nil
            | [x] => [[x]]
            | (h::t) => 
               let val Pt = permutations t
               in flatten (map (fn x => insert h x) Pt)
               end
   end;

fun filter (P: 'a -> bool) (list: 'a list) 
    = case list of nil => []
                | (h::t) => if (P h) then h::(filter P t) else filter P t;

fun allPermutations L n =
    let 
        val poweredSet = powerSet L
        val filterdSet = filter (fn x => length x = n) poweredSet
    in
        flatten (map (fn x => permutations x) filterdSet)
    end;

allPermutations [1,2,3] 2;
(* 
課題点：
筆者が解かせようと思っていた方法と違う感じがするので、なんともいえない。
*)