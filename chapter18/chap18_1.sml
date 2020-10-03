(* SML source file. Copyright (c) by 2222-42 2020.
* Chap18.1
*)



(* for test:
string;
(* expected: url *)
id;
(* expected: its value *)
link(e);
(* expected: eの示すURLのページに含まれるリンク構造 *)
follow(e,n);
(* expedcted: eの示すリンク構造のn番目のURL *)

val id = exp;
(* expected: idをexpの評価結果に束縛する *)

cd string;
(* expected: stringで表されるディレクトリを現ディレクトリに設定する *)

copy "http://www.jaist.ac.jp/index.html" to "file:///tmp";
copy "http://www.jaist.ac.jp/~ohori/index.html" to ".";
(* Webページ群をコピーする
- source: コピー元のルートページを指すURLを表す式
- dir: コピー先のルーティングディレクトリの指定。
- 他のページの参照を含むWebぺーじであれば、そのページから相対パスで参照されrているページをすべて、
その対応パスで参照されているページをすべて、対応する位置に再帰的にコピーする
- コピーの対象は`<a href="RelativePath">`もしくは   `<img src="RelativePath">`で、相対パス
- コピー先ディレクトリやサブディレクトリがそんざいしなければ、コピー処理で作成
- もし同一のファイル名が存在すればそのファイルのコピーは行わない
 *)

print e;
(* expected: eの結果で得られるURLをテキストとみなし、内容をプリントする *)

use e;
(* expected: eを評価し、得られるURLをスクリプトファイルとみなし、そのファイルを実行 *)

env;
(* 現在定義されている変数とその値を表示 *)

help;
(* ヘルプメッセージを表示。 *)
*)