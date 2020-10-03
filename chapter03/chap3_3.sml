(* SML source file. Copyright (c) by 2222-42 2020.
* 3.3
*)

(* MLはもっとも一般的な多相型が自動で推論される。可読性の向上のために、型宣言をしてその対象となる式を明示的にする選択もあり。 *)

fun intTwice f (x: int) = f (f x);
fun intTwice (f: int -> int) x = f (f x);
fun intTwice f x = f (f x): int;
fun intTwice f x : int = f (f x);
(* ((intTwice f)) x : int なので、関数適用の最終結果の型を宣言している *)

(* 注意：式自身の型より一般的な型宣言を行うとエラーが起きる *)