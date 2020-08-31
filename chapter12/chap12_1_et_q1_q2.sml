(* SML source file. Copyright (c) by 2222-42 2020.
* Chap12.1 Q12.1 Q12.2
*)

(* Position :> Integer がファイルシステム操作であるということについては、
公式のドキュメントを見よう
https://smlfamily.github.io/Basis/integer.html#section:0
*)

(* Q12.1 *)

signature Math = MATH;

fun convBase (f: real -> real) r x = f x / f r;

(* 筆者の解答 : 同じだったからこれでOK*)
(* fun convBase f b x = f x / f b *)
(* 関数適用は一番優先度が高いから、以下の回答者の回答より筆者の回答がより正しい。
fun convBase f r x = 
    (f x)/(f r);
*)

val log2 = convBase Math.log10 2.0;

log2 1024.0;
(* 
Math ストラクチャを利用していないのだが、大丈夫だろうか？

あと、型が
val convBase = fn : ('a -> real) -> 'a -> 'a -> real
となっており、教科書に記載されているものと異なる？
-> 教科書の記述が誤植

教科書の記述もおかしいし、保留で。
-> x についての情報がないので、型変数になってしまう。real 型を与えたいならば、それを明示する。
これにより、
val convBase = fn : (real -> real) -> real -> real -> real
と、正しい型になる。

SML#だと`/`がオーバーロードされているから、型変数が残ってしまうので、`f`に型を明示的に与える必要がある。
*)

(* Q12.2 *)

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

(* 筆者の解答:
SML#だと、
> structure Bool = Bool;
....
*)
(*
SML#で、シグネチャをプリントできるらしいが、SMLだとそれができない。
また、筆者の解答例でも特に「参照カード」なるものをコード内で明示的に作っているわけではないので、これで回答になっていると思われる。

なお、「参照カード('reference card')」でググると、以下のような結果がでてくる。
「リファレンスカード、リファレンスシート、またはベビーベッドシートは、特定のトピックに関する要約されたメモを簡潔にまとめたものです。たとえば、面積/体積を計算する数式や、特定のコンピュータープラットフォーム、アプリケーションプログラム、または正式な言語の一般的な構文規則とイディオムなどです。」
だから、回答者のように、signatureで宣言することで十分かと思われる。
*)
