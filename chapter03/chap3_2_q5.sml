(* SML source file. Copyright (c) by 2222-42 2020.
* q3.6
*)

(* 1 *)
fun S x y z = (x z) (y z);
(* 
expect:
('a -> 'b -> 'c) -> ('a -> 'c) -> 'a -> 'c

result:
val S = fn : ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c

筆者コメント：コンビネータ論理においてSコンビネータとして知られる基本関数である
*)

(* 2 *)
fun K x y = x;
(* 
expect:
'a -> 'b -> 'a

result:
val K = fn : 'a -> 'b -> 'a

筆者コメント：コンビネータ論理においてKコンビネータとして知られる基本関数である
*)

(* 3 *)

fun A x y z = z y x;
(* 
expect: 'a -> 'b -> ('b -> 'a -> 'c) -> 'c

result:
val A = fn : 'a -> 'b -> ('b -> 'a -> 'c) -> 'c
*)

(* 4 *)
fun B f g = f g g;
(* 
expect:
    (('a -> 'b) -> ('a -> 'b) -> 'c) -> ('a -> 'b) -> 'c

result:
val B = fn : ('a -> 'a -> 'b) -> 'a -> 'b

最も一般的な多相型なので。

- `'a -> 'b `の部分は`'a`に
- `'c` の部分は`'b`になる。
*)

(* 5 *)
fun C x = x C;
(* 
expect: Compile error


result: Error: operator is not a function [circularity]

- Cはxを引数にする関数
- xはCを引数にする関数

よって、Cの型は、Cの型を引数とする関数を引数とする関数となるから。

無限に循環してしまうため、有限な型ではない。

筆者の回答：Cの型は，Cを引数とする関数 を引数とする関数となり，自分自身を部分に含む型となってしまい，
そのような 性質をもつ型は有限な範囲では存在しない

回答者のコメント：「自分自身を部分に含む型となってしまい」、「有限な範囲では」「そのような性質を持つ型は」「存在しない」という表現は、
私の回答であいまいにされていた箇所を明確にしている。
*)

(* 6 *)
fun D p a b = if p a then (b, a) else (a, b)
(* 
expect: ('a -> bool) -> 'a -> 'a -> ('a * 'a)

result:
val D = fn : ('a -> bool) -> 'a -> 'a -> 'a * 'a
*)