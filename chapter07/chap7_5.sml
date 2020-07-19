(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 7.5
*)

(* データ型の使用例として、2分木を使って値の登録と検索を行う辞書プログラム *)
datatype 'a tree = Empty | Node of 'a * 'a tree * 'a tree;

type 'a dict = (string * 'a) tree;

(* 
二分探索木として使用する：
2分木内の全てのノードNode(key, L, R)について、
L部分木に含まれるキーはkeyより小さく、
R部分木に含まれるキーはkeyより大きい
*)

(* 
val enter : string * 'a * 'a dict -> 'a dict
実装：
1. Emptyの場合
2. Node(key, L, R)の場合
  a. 与えられたkeyの方が大きいなら、Rに対して拡張処理を行い、 Node(key, L, R')を返す
  b. 与えられたkeyの方が小さいなら、Lに対して拡張処理を行い、 Node(key, L', R)を返す
  c. 与えられたkeyと等しい、そのまま返す
*)

fun enter (key, v, dict) = 
    case dict of 
        Empty => Node((key, v), Empty, Empty)
        | Node((key', v'), L, R) =>
            if key = key' then dict
            else if key > key' then 
                Node((key', v'), L, enter (key, v, R))
            else Node((key', v'), enter (key, v, L), R);


(* val lookUp : string * 'a dict -> 'a option
1. Emptyの場合、NONE
2. Node(key, L, R)の場合
  a. 与えられたkeyの方が大きいなら、再帰的にRを探索する
  b. 与えられたkeyの方が小さいなら、再帰的にLを探索する
  c. 与えられたkeyと等しい、SOME vを返す
*)

fun lookUp (key, Empty) = NONE
    | lookUp (key, Node((key', v), L, R)) =
        if key = key' then SOME v
        else if key > key' then lookUp(key, R)
        else lookUp(key, L);


