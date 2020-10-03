(* SML source file. Copyright (c) by 2222-42 2020.
* Q 2.20
*)

fun fastPower(n, f, v, e) = 
    if n = 0 then e
    else f (v, fastPower(n-1, f, v, e));

fun previousMatrixPower(n,a,b,c,d) = 
    if n = 0 then (1,0,0,1)
    else let 
            val m = n div 2
            val k = n mod 2
            val (x,y,z,w) = previousMatrixPower(m,a,b,c,d)
            val (p,q,r,s) = (x*x+y*z,x*y+y*w,z*x+w*z,z*y+w*w)
         in 
            if k = 1 then (a*p+b*r,a*r+b*s,c*p+d*r,c*q+d*s)
            else (p,q,r,s)
         end 


(* 単位元e: (1, 0, 0,1)
f (v1,v2) = v1 * v2
multi (a, b, c, d) (x, y, z, w) = (a*x+b*z, a*y+b*w, c*x+d*z, c*y+d*w) *)

fun fastMatrixPower(n,a,b,c,d) = 
    let
        val e = (1,0,0,1)
        fun multi((p, q, r, s),(x, y, z, w)) = (p*x+q*z, p*y+q*w, r*x+s*z, r*y+s*w)
        val m = n div 2
        val k = n mod 2
    in
        let val v1 = fastPower(n, multi, (a,b,c,d), e)
        in 
            if k = 0 then v1
            else multi ((a,b,c,d),v1)
        end
        
    end;

(* 筆者の回答 
私の回答とあまり違いがないので、まぁいいかな。
*)
   (* fun matrixMult (a,b,c,d) (x,y,z,w) = (a*x+b*z,a*y+b*w,c*x+d*z,c*y+d*w)
   fun fastPower (n,f,v,e) =
        if n = 0 then e
        else let val m = n div 2
                 val k = n mod 2
             in if m = 0 then v
                else let val v1 = fastPower(m,f,v,e)
                          val v2 = f (v1,v1)
                      in if k = 0 then v2
                          else f(v,v2)
                      end
             end;
   fun fastMatrixPower(n,a,b,c,d) =
       fastPower(n, fn (x,y) => matrixMult x y, (a,b,c,d), (1,0,0,1)); *)