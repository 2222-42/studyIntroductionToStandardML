(* SML source file. Copyright (c) by 2222-42 2020.
*
*)

let fun f x =
    let fun g y = x + y
        fun h x = g (x * 3)
    in h (x + 3)
    end
in f 10
end;

(* 
{x: 10}
=> h (x + 3)
=> h 13
=> h x' = g (x' * 3)
=> h 13 = g (13 * 3)
=> h 13 = x + 13
=> 23
=> 
{x: 10, g y: 10 + y}
=> 
{x: 10, g y: 10 + y, h 10: g (30)}
*)

(*
=> f 10
{f: int -> int, a: 10} |- f a
{f: int -> int, a: 10} |- h (x + 3) a
{f: int -> int, a: 10} |- g((x + 3)*3) a
{f: int -> int, a: 10} |- (a + (x + 3)*3 ) a
10 + (10 + 3) * 3
10 + 13 * 3 
49
*)
