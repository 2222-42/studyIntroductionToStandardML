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

