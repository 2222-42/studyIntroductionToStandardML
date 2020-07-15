(* SML source file. Copyright (c) by 2222-42 2020.
* Q7.7
*)

(* 
リストと同様に2分木のfold関数を定義しよう
*)

(* 
treeFoldの方針

取るべき引数
1. 変換の対象とする木 `t`
2. 木がEmptyの時の値 `z`
3. 左部分木Lと右部分木RをそれぞれtreeFoldした結果とノードのデータxから木Node(x,L,R)の結果を計算する関数 `f`

変換関数fは機構製紙Nodeの型と対応させて
'a * 'b * 'b -> 'b
という型を持つ関数と定義すると、都合がよい
*)
datatype 'a tree = Empty | Node of 'a * 'a tree * 'a tree;

fun treeFold f z Empty = z
  | treeFold f z (Node(x,L,R)) = f (x, (treeFold f z L), (treeFold f z R));

(* 1 *)

fun countNodes t = treeFold (fn (a, b1, b2) => 1 + b1 + b2) 0 t;

val testNode = Node (1,Node (2,Empty,Empty),Node (3,Node (4, Empty, Empty),Empty));

countNodes testNode;

(* 2 *)

fun sumTree t = treeFold (fn (a, b1, b2) => a + b1 + b2) 0 t;

sumTree testNode;

(* 3 *)

fun mapTree f t = treeFold (fn (a, b1, b2) => Node(f(a), b1, b2)) Empty t;

mapTree (fn x => x + 1) testNode;

(* 4 *)

fun toPostOrder t = treeFold (fn (a, b1, b2) => "(" ^ b1 ^ ")" ^ "(" ^ b2 ^ ")" ^ a) "" t;

val testStringTree = Node ("a",Node ("b",Empty,Empty),Node ("c",Node ("d", Empty, Empty),Empty));
toPostOrder testStringTree;

fun toInOrder t = treeFold (fn (a, b1, b2) => "(" ^ b1 ^ ")" ^ a ^ "(" ^ b2 ^ ")") "" t;

toInOrder testStringTree;