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


fun op MyIgnore a = ();
MyIgnore (fib 36);
(* 実行結果の表示に時間がかかるし、overflowエラーを起こそうと思えば起こせるから、多分これで評価はしているでしょ。 *)
