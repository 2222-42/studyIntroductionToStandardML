(* SML source file. Copyright (c) by 2222-42 2020.
*
*)

fun & x = x mod 16;

fun op &+ (x, y) = & x + & y;
infix 8 &+;

fun op &- (x, y) = & x - & y;
infix 8 &-;

fun op &* (x, y) = & x * & y;
infix 9 &*;

fun op &= (x, y) = & x = & y;
infix 2 &=;