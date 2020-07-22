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

(* 
筆者の回答:
 各変数のスコープは以下の通り。

- 外側のf : f (2,3);
- 外側のx : in x + f y end
- 外側のy : x + y; in x + f y end;
- 内側のf : x + y; in x + f y end;
- 内側のx : x + y
*)