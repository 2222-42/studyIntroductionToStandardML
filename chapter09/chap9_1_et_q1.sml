(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 9.1 Q 9.1
*)

(* MLでは、広域的なジャンプを制御された形で実行する例外処理機構が用意されている。 *)

(* exception文で例外の名前の定義 *)
(* 
- exception exnId;
- exception exnId of t;
*)
(* 
- exnId: 例外名
- t: 例外引数の型(名前以外不要なら、これは省略して宣言すればよい)
*)

(* raise文で例外を発生させるよ *)
(* 
- raise exnId;
- raise exnId exp; 
*)
(* expはexception文で定義した型を持たなければならない。 *)

(* この構文で例外が起きると、この構文をもつプログラムの評価が中段され、システムの例外処理機構が起動される
-> この構文の表か結果は存在しない。
-> 多相型'aをもつ
*)

exception A;
(* exception A *)
fun f x = raise A;
(* val f = fn : 'a -> 'b *)
fn x => (f 1 + 1, f "a" andalso true);
(* val it = fn : 'a -> int * bool *)

(* Q9.1 *)
(* 値を返す関数は、関数適用の結果が任意の型を持つようなことはありえない *)
(* 型'a -> 'bを持つ関数を、例外を使わずに定義せよ。 *)
fun f x = f x;

(* 筆者の回答: *)
fun f x = hd nil;

(* 種々の例外；トップレベルで定義されているシステム定義例外 *)
