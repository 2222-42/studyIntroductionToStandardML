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

(* 筆者の解答 *)
fun prefixListByAuthor nil = nil
  | prefixListByAuthor (h::tl) =
    let
      val L = prefixListByAuthor tl
    in
       [h] :: map (fn y => h::y) L
    end;
(* 筆者の解答の場合、長さ1の場合の処理も含めてまとめている

問6.9でわかるが、この単純さ、
    特に計算の対象とするものを外側に押しやる感じは、
拡張性の観点から見て、すごく実装と理解を単純にする
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

(* 筆者の解答 *)
fun suffixListByAuthor nil = nil
  | suffixListByAuthor (h::t) =
    let
       val L = suffixListByAuthor t
    in
       (h::t)::L
    end;
(* 
筆者の解答の場合、末尾のnilが含まれないことになるので、修正する必要があるだろう
-> そもそも、空なリストを含むのがそこまで重要なのか？
*)

fun suffixListModified nil = [[]]
  | suffixListModified (h::t) =
    let
       val L = suffixListModified t
    in
       (h::t)::L
    end;

(* 筆者の方針を用いた解答の場合、私の解答のようなあっちにいったり、こっちにいったり、というのがないのがよい。 *)

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

(* 筆者の解答 *)
fun flattenByAuthor nil = nil
  | flattenByAuthor (h::tl) = h @ flattenByAuthor tl;

fun allIntervalsByAuthor L = flattenByAuthor (map prefixListByAuthor (suffixListByAuthor L));
allIntervalsByAuthor [1,2,3];
(* suffixListを並べて、それに対して、 *)

fun allIntervalsModified L = flattenByAuthor (map prefixListByAuthor (suffixListModified L));
allIntervalsModified [1,2,3];

(* 3 *)
(* 
fun powerSet L = 
    case L of nil => []
            | [x] => [[x]]
            | (h::t) =>
                let val subSet = (powerSet t)
                in 
                    (map (fn x => h::x) ([]::subSet)) @ subSet
                end;
*)
fun powerSet L = 
    case L of nil => [[]]
            (* | [x] => [[x]] *)
            | (h::t) =>
                let val subSet = (powerSet t)
                in 
                    (map (fn x => h::x) (subSet)) @ subSet
                end;

powerSet [1,2,3];
(* 
previous result:
val it = [[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]] : int list list
-> 空なリストを含んでいないからダメ。

空なリストを含んでいなかった理由:
- nil の場合に[[]]、もしくは[nil]を返していなかったこと。

あと、不要な、[[x]]を返していたのも、関数をごちゃごちゃにすることにつながっていた。

その結果、複雑な式を作ることになっていた。

- powerSet [1,2,3];
val it = [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]] : int list list
*)

(* 筆者の解答 *)
fun powerSetByAuthor nil = [nil]
  | powerSetByAuthor (h::t) =
    let
       val PT = powerSetByAuthor t
    in
       map (fn x => h::x) PT @ PT
    end;
powerSetByAuthor [1,2,3];

(* 4 *)
fun flatten list = case list of nil => []
                            | (h::t) => h@(flatten t);

fun permutations L = 
   let fun insert s nil = [[s]]
         | insert s (h::t) = 
               let val L = insert s t
               in (s::(h::t)) :: (map (fn x => h::x)L)
               end
   in case L of nil => [[]]
            | (h::t) => 
               let val Pt = permutations t
               in flatten (map (fn x => insert h x) Pt)
               end
   end;
(* 
   in case L of nil => nil
            | [x] => [[x]]
というのは冗長で、
   in case L of nil => [[]]
とすれば十分
*)
(* 筆者の解答 *)
fun permutationsByAuthor L =
    let fun insertAll s nil = [[s]]
          | insertAll s (h::t) =
              let val L = insertAll s t
              in (s::(h::t)) :: (map (fn x => h::x) L)
              end
    in foldr (fn (x,y) => foldr (fn (a,b) => insertAll x a @ b) nil y)
             [nil]
             L
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

(* 筆者の解答 *)
fun allPermutationsByAuthor L n =
    let val subs = filter (fn x => length x = n) (powerSetByAuthor L)
    in foldr (fn (x,y) => permutationsByAuthor x @ y) nil subs
    end;
allPermutationsByAuthor [1,2,3] 2;