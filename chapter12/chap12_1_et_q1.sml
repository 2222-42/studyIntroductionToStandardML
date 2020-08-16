(* SML source file. Copyright (c) by 2222-42 2020.
* Chap12.1 Q12.1
*)

(* Position :> Integer がファイルシステム操作であるということについては、
公式のドキュメントを見よう
https://smlfamily.github.io/Basis/integer.html#section:0
*)

(* Q12.1 *)

signature Math = MATH;

fun convBase f r x = 
    (f x)/(f r);

val log2 = convBase Math.log10 2.0;

log2 1024.0;
(* 
Math ストラクチャを利用していないのだが、大丈夫だろうか？
*)
