(* SML source file. Copyright (c) by 2222-42 2020.
* Chap12.1 Q12.1 Q12.2
*)

(* Position :> Integer がファイルシステム操作であるということについては、
公式のドキュメントを見よう
https://smlfamily.github.io/Basis/integer.html#section:0
*)

(* Q12.1 *)

signature Math = MATH;

fun convBase f r x = 
    (f x)/(f r);

(* 筆者の解答 : 同じだったからこれでOK*)
(* fun convBase f b x = f x / f b *)

val log2 = convBase Math.log10 2.0;

log2 1024.0;
(* 
Math ストラクチャを利用していないのだが、大丈夫だろうか？

あと、型が
val convBase = fn : ('a -> real) -> 'a -> 'a -> real
となっており、教科書に記載されているものと異なる？

教科書の記述もおかしいし、保留で。
*)

(* Q12.1 *)

signature Bool = BOOL;
signature Char = CHAR;
signature Int = INTEGER;
signature List = LIST;
signature Real = REAL;
signature String = STRING;
(* 
「モジュールのシグネチャをプリントしておくと、モジュールの参照カードの役割を果たす」
「これらストラクチャのシグネチャをプリントし、参照カードを作成せよ」
p.156とp.157の記述を読めば、これでプリントしていることになる。
疑問：これで参照カードになっているのか？
*)
