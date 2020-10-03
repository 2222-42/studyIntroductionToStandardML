(* SML source file. Copyright (c) by 2222-42 2020.
* Q 2.14
*)

fun matrixPower (n,a,b,c,d) =
    if n = 0 then (1,0,0,1)
    else let val (x,y,z,w) = matrixPower(n-1,a,b,c,d)
         in (a*x+b*z,a*y+b*w,c*x+d*z,c*y+d*w)
         end

fun fastMatrixPower(n,a,b,c,d) = 
    if n = 0 then (1,0,0,1)
    else let 
            val m = n div 2
            val k = n mod 2
            val (x,y,z,w) = fastMatrixPower(m,a,b,c,d)
            val (p,q,r,s) = (x*x+y*z,x*y+y*w,z*x+w*z,z*y+w*w)
         in 
            if k = 1 then (a*p+b*r,a*r+b*s,c*p+d*r,c*q+d*s)
            else (p,q,r,s)
         end

fun veryFastFib n = let 
    val (x,y,z,w) = fastMatrixPower(n,0,1,1,1)
in
    y
end;

fun time f = let
    val realt = Timer.startRealTimer()
    val rv = f ()
    val elapsed = Timer.checkRealTimer realt
in
    (Time.toMilliseconds elapsed, Time.toMicroseconds elapsed,Time.toNanoseconds elapsed, rv)
end;

time (fn () => matrixPower (1000000,1, 0, 0, 1));
time (fn () => fastMatrixPower (1000000,1, 0, 0, 1));