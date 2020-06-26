(* SML source file. Copyright (c) by 2222-42 2020.
*
*)

fun f (x, y) =
    let fun f x = x + y;
    in x + f y
    end;
f(2,3);
(* 
f x = x + 3
2 + f(3)
=> 8
*)