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

val lookUp : string * 'a dict -> 'a option *)