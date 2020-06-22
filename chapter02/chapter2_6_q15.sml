fun summation f n = if n = 1 then f 1
                    else f n + summation f (n - 1);

fun power m n = if m = 0 then 1
                else n * power (m - 1) n;

fun sumUpTo n = summation (power 1) n;

fun correctSquare n = summation (power 2) n;

fun sumOfSumUpTo n = summation sumUpTo n;