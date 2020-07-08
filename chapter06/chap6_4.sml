(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 6.4 リスト処理の基本関数
*)

(* 
トップレベルで提供されている処理関数

- infixr 5 @
- exeption Empty
- null
- hd
- tl
- @
- rev: 逆順のリストを返す関数
- length
- map: 与えられた関数をリストの各要素に適用し、その結果のリストを返す高階関数

これらの関数はパターンマッチングと再帰的関数定義を組み合わせれば簡単に定義できる。
基本的なデータ構造であるので、組み込み関数として効率よく実装されている。
*)

map (fn x => (x,x)) [1,2,3,4];

(*
- hd [];
stdIn:1.2-1.7 Warning: type vars not generalized because of
   value restriction are instantiated to dummy types (X1,X2,...)

uncaught exception Empty 

- tl [];
stdIn:1.2-1.7 Warning: type vars not generalized because of
   value restriction are instantiated to dummy types (X1,X2,...)

uncaught exception Empty
*)

fun flatten list = case list of nil => []
                            | (h::t) => h@(flatten t);

(* 複雑な再帰処理を必要とする例 *)

(*
1. 空リストなら空リストを返す
2. 1要素のリストなら、そのリストを要素とするリストを返す
3. 2要素以上のリストなら、リストをh::tに分解し、permutations tを求める。
   その結果のリストの中の各要素のリストに対して、hを種々の位置に挿入し得られるリストを全て求め、
   それらリストを1つのリストとして返す
*)
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
(* expected *)
permutations [1,2,3];
(* 
val it = [[1,2,3], [2,1,3], [2,3,1], [1,3,2], [3,1,2], [3,2,1]]: int list list
*)
