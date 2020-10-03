(* SML source file. Copyright (c) by 2222-42 2020.
* chapter 2.6
*)

(*二つの引数を渡すのではなく、int -> (int -> int)な、関数を返す関数を作る*)
fun power m n = if m = 0 then 1
                         else n * power (m - 1) n;

val cube = power 3;

(* 関数を引き取ってそれを使って処理を行う高階の関数 *)
(* 高階の関数を使った方がプログラムの再利用による生産性の向上や、プログラム全体のモジュール性と保守性の麺からもよい *)
fun summation f n = if n = 1 then f 1
                    else f n + summation f (n - 1);

val newSumOfCube = summation cube;
val sumOfSquare = summation (power 2);
