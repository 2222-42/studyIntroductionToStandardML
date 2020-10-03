(* SML source file. Copyright (c) by 2222-42 2020.
*
*)

fun products f n = if n = 0 then 1
                   else f n * products f (n - 1);

(*
以下の関数は型エラーをコンパイル段階で自動的に検出される
 fun factorial n = products n (fn x => x) *)


