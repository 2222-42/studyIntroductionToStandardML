(* SML source file. Copyright (c) by 2222-42 2020.
* Q 2.9
*)

fun fastFib n = let 
    (* fun fib n = if n < 2 then n else fib (n - 1) + fib (n - 2) *)
    fun G(n, f, g) = if n = 0 then f else if n = 1 then g else G(n - 1, g, f + g)
in
    G(n, 0, 1)
end;