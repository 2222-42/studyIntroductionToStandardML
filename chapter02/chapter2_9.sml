(* SML source file. Copyright (c) by 2222-42 2020.
*
*)

fun power m n = if m = 0 then 1
                else n * power (m - 1) n;

fun Power(m, n) = power m n;

infix 8 Power;

2 Power 3 + 10;
op Power;
op Power(2,3);

infix 9 hyperexp;
fun op hyperexp (x,y) = 
    if x = 0 then y
    else 2 Power (x - 1) hyperexp y;

(* 
 2 Power ((x - 1) hyperexp y);
Reason:
- Power: 8
- hyperexp: 9 
*)

(fn f => fn x => (f(x, x))) op hyperexp 2;
(* ((fn f => fn x => (f(x, x))) op hyperexp) 2 *)

3 Power 2 Power 2;
infixr 8 Power;
3 Power 2 Power 2;
nonfix Power;
Power(2,3);