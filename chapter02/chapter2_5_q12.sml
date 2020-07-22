(* SML source file. Copyright (c) by 2222-42 2020.
* Q 2.12
*)

fun x(n, a, b, c, d) = if n = 0 then 1
                       else a * x(n-1, a, b, c, d) + b * z(n-1, a,b,c,d)
and y(n, a, b, c, d) = if n = 0 then 1
                       else a * y(n-1, a, b, c, d) + b * w(n-1, a,b,c,d)
and z(n, a, b, c, d) = if n = 0 then 1
                       else c * x(n-1, a, b, c, d) + d * z(n-1, a,b,c,d)
and w(n, a, b, c, d) = if n = 0 then 1
                       else c * y(n-1, a, b, c, d) + d * w(n-1, a,b,c,d);

fun matrixPower (n,a,b,c,d) =
    if n = 0 then (1,0,0,1)
    else let val (x,y,z,w) = matrixPower(n-1,a,b,c,d)
         in (a*x+b*z,a*y+b*w,c*x+d*z,c*y+d*w)
         end;

fun time f = let
    val realt = Timer.startRealTimer()
    val rv = f ()
    val elapsed = Timer.checkRealTimer realt
in
    (Time.toMilliseconds elapsed, Time.toMicroseconds elapsed,Time.toNanoseconds elapsed, rv)
end;

time (fn () => (x (20, 1, 0, 0, 1), y (20, 1, 0, 0, 1), z (20, 1, 0, 0, 1), w (20, 1, 0, 0, 1)));
time (fn () => (x (30, 1, 0, 0, 1), y (30, 1, 0, 0, 1), z (30, 1, 0, 0, 1), w (30, 1, 0, 0, 1)));
time (fn () => matrixPower (20, 1, 0, 0, 1));
time (fn () => matrixPower (30, 1, 0, 0, 1));
(* timerで0としか表示されないくらいに早い？ *)
time (fn () => matrixPower (100000, 1, 0, 0, 1));