(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 7.3 and Q7.8
*)

(* 
パターンマッチングの主な機能はcase文によるデータ型の処理
他にもいくつかの略記法や拡張機能が提供されている。
*)

(* 
処理の対象となった値も保持したい場合があるとき
変数xとその値が満たすべき条件を表すpatを以下のような形式で記述できる
    x as pat

こうすると、patに含まれる変数の束縛に加えて、xが値全体に束縛される
*)
datatype 'a tree = Empty | Node of 'a * 'a tree * 'a tree;

fun f Empty = true
  | f (Node(_, Empty, Empty)) = true
  | f (Node(_, x as Node _, y as Node _)) = f x andalso f y
  | f _ = false;

(* 
Q 7.8
上記fがtrueを返す2分木は？
高さ0の2分木もしくは、
片方の枝だけEmptyということがないような2分木
*)
(* 筆者の解答:
Node(_, Empty, Node _)の形のノードを含まない木、つまり、完全２分木である。
*)

f (Node (1,Node (2,Empty,Empty),Node (3,Node (4, Empty, Empty),Empty)));
(* expected: false *)
f (Node (1,Node (2,Empty,Empty),Node (3,Node (4, Empty, Empty), Node (5, Empty, Empty))));
(* expected: true *)

(* 
パターンマッチングのパターンには
eqtypeに属する定数も書ける
    -> 等値性のチェックを行うことができる

定数パターンは他のパターンと組み合わせて使用することもできる
*)

fun fib 0 = 1
  | fib 1 = 1
  | fib n = fib (n - 1) + fib (n - 2);

(* 
val pat = exp
という構文を用いれば、
1. expを評価し、
2. その値とパターンpatとのマッチングを試みる
3. マッチするなら、パターンの中の変数の束縛を生成し、現在の環境に追加する
4. マッチしなければBind例外を発生させる。

この機構は、組パターンやレコードパターン、定数パターンを含む全てのパターンに対して利用できる
*)

val Node(x,y,z) = Node("a", Node("b", Empty, Empty), Empty);
(* val Empty = Node("b", Empty, Empty); *)
