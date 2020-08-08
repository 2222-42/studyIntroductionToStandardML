(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 9.4 Q9.5
*)

(* 例外は多相型をパラメータとして持つことは許されない *)
(* exception Foo of 'a -> 'a; *)
(* stdIn:1.2-1.27 Error: type variable in top level exception type *)
(* raise Foo (fn x => x); *)

(* Q9.5 例外が多相型を持ちうるとすると型システムでは検出できない型エラーを起こす例 *)
exception Found of 'a
raise Found 1 handle Found C => chr((ord C) + 1) 

(* ただし、多相型を持つ例外も定義し利用することができる
多相型の値式への制限があるから。 *)
