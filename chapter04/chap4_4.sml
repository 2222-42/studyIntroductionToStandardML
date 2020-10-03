(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 4.4
*)

(* 
type real

eqtypeではない。
    なぜなら浮動小数点表現された実数の型であり、有限の精度であり、厳密な等値性テストを行えないから
*)

(* 
実数定数:
- 小数点を含んだ数字列
- xEn("x * 10^n"を表す)

小数部か指数部かが含まれる定数はreal型、
そうでなければint型
*)

3.14;
(* val it = 3.14 : real *)
1E2;       
(* val it = 100.0 : real *)
3.14E2;  
(* val it = 314.0 : real *)

(* 
- ~
- +
- -
- *
- /
- >
- >=
- <
- <=
- abs
- real
- floor
- ceil
- round
- trunc
 *)

real 3 > 3.0; 
(* val it = false : bool *)
3.0 >= abs (real ~3); 
(* val it = true : bool *)
floor 3.4; 
(* val it = 3 : int *)
ceil 3.4;  
(* val it = 4 : int *)
round 3.4;
(* val it = 3 : int *)
round 3.5; 
(* val it = 4 : int *)
trunc 3.9;
(* val it = 3 : int *)

(* 
int型への変換で表現できない場合は、int型でのOverflow例外が起きる。

real型では例外処理がない。
0除算のexceptionなどはない

inf, ~inf, nanなどで表示される。
*)

val a = 1.1 / 0.0;
(* val a = inf : real *)
a * ~1.0;
(* val it = ~inf : real *)
a / it;
(* val it = nan : real *)
a - a;
(* val it = nan : real *)