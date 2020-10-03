(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 3.6
*)

(* 
演算子`=`について：
- 式の評価の結果が同じか否かをテストする等値演算子
- 値が格納されているメモリアドレスの同値性ではない
- 式の値の等値性をテストする
*)

(* 
等値演算子は色々な値に適用できるから一種の多相関数？
\<rightarrow>　NO. 全ての型には適用可能ではない

再帰を含んだ関数の値としての等値性は計算不可能
*)

op =;
(* 
stdIn:42.1-42.5 Warning: calling polyEqual
val it = fn : ''a * ''a -> bool

特別な多相型をMLは与えている。

`''a`は「等値性が計算可能な任意の型」(`eqtype`と呼ばれる)を表す型変数。

eqtypeの範囲:
- int, bool, char, stringなどの離散的な基本データ型
- `''a`などのeqtype型変数
- eqtypeとeqtype構成子から構成される型
  - 関数型構成子`->`を除くほとんどのものがeqtype構成子
- 参照型(t: ref)
  - t型のデータへのポインタ型に相当する型(cf: chap 8)
  - tが何でもあっても参照型はeqtypeとなる

以上のいずれかの型をもつ式同士は等値性テストが可能
(当然であるが型が異なったらテストできなかった
- "string" = 1;
stdIn:46.1-46.13 Error: operator and operand do not agree [overload conflict]
  operator domain: string * string
  operand:         string * [int ty]
  in expression:
    "string" = 1
)
*)

- fun f x = x;
(* val f = fn : 'a -> 'a *)
- fun g x = x;
(* val g = fn : 'a -> 'a *)
(* 
再帰を含まない関数で比較したらダメだった。
- f = g;
Error: operator and operand do not agree [equality type required]
  operator domain: ''Z * ''Z
  operand:         ('Y -> 'Y) * ('X -> 'X)
  in expression:
    f = g

多相型を含まない関数でやってもダメだった。
*)

(* 
eqtypeのデメリット：
- MLの型システムを複雑にする

eqtypeのメリット：
- 等値演算子を含む種々の有用な汎用の関数を定義できる
  - memo関数など

- fun memo f x = let val a = f x
=                in fn y => if x = y then a else f y
=                end;
stdIn:61.32 Warning: calling polyEqual
val memo = fn : (''a -> 'b) -> ''a -> ''a -> 'b

xの型は''aである。
*)