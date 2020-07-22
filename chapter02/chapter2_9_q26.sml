(* SML source file. Copyright (c) by 2222-42 2020.
*
*)

fun & x = x mod 16;

(* 演算した結果もmodしなければならないから、全体を&でくくる必要がある。 *)
fun op &+ (x, y) = &(& x + & y);
infix 8 &+;

fun op &- (x, y) = &(& x - & y);
infix 8 &-;

fun op &* (x, y) = &(& x * & y);
infix 9 &*;

fun op &= (x, y) = & x = & y;
infix 2 &=;

& 17 ; 
   (* val it = 1 int  *)
4 &* 5 &+ 1; 
   (* val it = 5 int   *)
9 &* 10 &+ 9; 
(* expected: 3 *)