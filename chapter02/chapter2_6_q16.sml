fun summation' f n = if n <= 0 then 0.0
                     else f n + summation' f (n - 1);