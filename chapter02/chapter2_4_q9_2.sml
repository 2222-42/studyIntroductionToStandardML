local 
    fun fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)
    fun G(n, f, g) = if n = 0 then f else if n = 1 then g else G(n - 1, g, f + g)
in
    fun fastFib n = G(n, fib 0, fib 1)
end;