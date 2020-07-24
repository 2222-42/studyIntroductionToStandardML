(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 4.1 and Q 4.1
*)

(* unit 単位型　`()`を唯一のデータとして持つ型 
eqtype unit
*)

(* 使い道がわからん：
使い道は、引数を必要としない関数や結果を返す必要がない関数を定義するときに使用される。
引数が装飾用とかの場合にそれを削ることとかあるよな。
sleep関数とかは結果を返す必要がない。
*)

(* 
val before: 'a * unit -> 'a

- exp1 before exp2;
val it = (exp1の評価結果);

val ignore : 'a -> unit

- ignore exp;
val it = ();
expを評価してその結果を捨てる。
*)

fun op MyBefore(m, ()) = m;
infix 0 MyBefore;
"string"^"_" MyBefore (); 
(1+1) MyBefore ();
(1+1) MyBefore print "One\n";


fun op MyIgnore a = ();
fun fib n = if n < 2 then n else fib (n - 1) + fib (n - 2);
MyIgnore (fib 36);
(* 実行結果の表示に時間がかかるし、overflowエラーを起こそうと思えば起こせるから、多分これで評価はしているでしょ。 *)

fun authorIgnore e = (fn _ => ()) e;

fun authorBefore (e1,e2) = (fn _ => fn x => x) e1 e2;
infix 0 authorBefore;
"string"^"_" authorBefore (); 
(fib 36) authorBefore print "One\n";

fun authorAndMyBefore (e1,e2) = (fn x => fn () => x) e1 e2;
infix 0 authorAndMyBefore;
(fib 36) authorAndMyBefore print "One\n";
