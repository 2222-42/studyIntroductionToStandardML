(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 9.2
*)

(* 
例外ハンドラ(例外処理構文)を追加できる。

exp handle exnPat_1 => handler_1
         | exnPat_2 => handler_2
         ...
         | exnPat_n => handler_n

exnPat_i: exception文で宣言された例外名と例外引数から構成される例外パターン
hanlder_i: exnPat_iにマッチする例外が発生したときに評価される式、expと同じ型でないといけない。
*)

exception Undefined;

fun power m n = if m = 0 then 1
                else n * power (m - 1) n;

fun strictPower n m = if n = 0 andalso m = 0 then raise Undefined
                      else power n m;

(* 3 + strictPower 0 0; *)

3 + (strictPower 0 0 handle Undefined => 1);
