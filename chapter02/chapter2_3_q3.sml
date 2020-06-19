fun sum n = if n = 1 then 1 else n + sum (n - 1);
fun big_sum n = if n = 1 then 1 else sum(n) + big_sum(n - 1)