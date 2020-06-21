fun matrixPower (n,a,b,c,d) =
    if n = 0 then (1,0,0,1)
    else let val (x,y,z,w) = matrixPower(n-1,a,b,c,d)
         in (a*x+b*z,a*y+b*w,c*x+d*z,c*y+d*w)
         end

fun fastFib' n = let 
    val (x,y,z,w) = matrixPower(n,0,1,1,1)
in
    y
end;