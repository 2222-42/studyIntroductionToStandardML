(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 4.3
*)

(* 
eqtype int

システムの定める1語のビット数をLとすると
表現可能な整数の範囲は:
-2^{L-2} <= n <= 2^{L-2} -1
となる。
1ビット分少ない理由は、領域の自動割り当てを提供している関数型言語の処理系の実装上の都合による。
*)

123;

~0xF;

(* int型データ操作のための例外と基本演算 
- exception Overflow
- exception Div
- ~
- *
- div
- mod
- +
- -
- >
- >=
- <
- <=
- abs
*)
~0xF + ~0xF;
~0xF * ~0xF; 
0x10 div 3;
0x10 mod 3;
~0xF - ~0xF;
~0xF > ~0xF;
~0xF >= ~0xF;
0 < 3;
0 <= 3;
abs ~0xF; 

(* exceptionについては第9章で説明する実行時の例外処理 *)
