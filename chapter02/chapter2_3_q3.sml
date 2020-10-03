(* SML source file. Copyright (c) by 2222-42 2020.
* Q2.3
*)
fun sum n = if n = 0 then 0 else n + sum (n - 1);
fun big_sum n = if n = 0 then 0 else sum(n) + big_sum(n - 1);
(*
筆者の回答は以下のようになっているが、これだと巡回してしまうので、おかしい
fun g 0 = 0
     | g n = g n + f n;
*)