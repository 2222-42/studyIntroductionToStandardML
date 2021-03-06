(* SML source file. Copyright (c) by 2222-42 2020.
* Chap12.2　Q12.3
*)

signature General = GENERAL;

(* トップレベルでopenされているシグネチャ
p.129より、このシグネチャのストラクチャ内の全ての名前の束縛が、すべての環境に追加され、直接使用可能になっている、ことになる。
*)

(* 
例外の方は`exn`型。
exceptionで宣言した例外構成子で生成される例外は、exn型を持つ値
-> データ構造に格納したり、他の関数に渡したりすることができる。
*)

(* Q12.3 *)

fun catchAll f x E =
    (f x) handle exn:General.exn => (print (exnMessage(exn)^"\n");E);

(* (1 div 0) handle exn:General.exn => (print (exnMessage(exn)^"\n");999); *)

catchAll (fn (x, y) => x div y) (1,0) 999;
catchAll (fn x => (chr x)) 1000 #" ";

(* 筆者の解答例: *)
fun catchAllByAuthor f x z =
         f x handle en => (print (exnName en ^ "\n"); z);

(* 問題文では
> 以下の汎用エラー処理関数を定義せよ． 関数とその引数，さらにエラーの場合に返す値を受け取り，関数を引数に適用すると ともに，上記の基本演算のエラーをキャッチしエラーメッセージを表示した後，エ ラーの場合の値を返す，

と記載されているから、`exnName`で引数として指定された例外の外部名を返す関数ではなく
例外を表示するためのメッセージを生成する関数であるところの`exnMessage`を使った方がいいのでは？
 *)

fun catchAll f x z =
         f x handle en => (print (exnMessage en ^ "\n"); z);
