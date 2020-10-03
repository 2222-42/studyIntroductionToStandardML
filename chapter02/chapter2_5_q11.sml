(* SML source file. Copyright (c) by 2222-42 2020.
* Q 2.10
*)

fun F x = if x > 1000.0 then 0.02 
          else (0.00001 * (x - 1000.0)) + 0.01;

fun fastAI (x, n) = let 
    fun auxAI(k, x, i) = if k = 0 then (x, i) else auxAI(k-1, x*(1.0+F(x)), F(x))
in
    auxAI(n, x, F(x))
end;

fastAI(900.0,100);
fastAI(900.0,1000);