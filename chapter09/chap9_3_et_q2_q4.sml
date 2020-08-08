(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 9.2
*)

datatype 'a tree = Empty | Node of 'a * 'a tree * 'a tree;

type 'a dict = (string * 'a) tree;

(* fun enter (key:string, v, dict) = 
    case dict of 
        Empty => Node((key, v), Empty, Empty)
        | Node((key', v'), L, R) =>
            if key = key' then dict
            else if key > key' then 
                Node((key', v'), L, enter (key, v, R))
            else Node((key', v'), enter (key, v, L), R); *)

(* 例外処理を導入すれば検索結果が辞書に存在したか否かのテストが不要になる *)
exception NotFound;

fun lookUp (key:string, Empty) = raise NotFound
    | lookUp (key, Node((key', v), L, R)) =
        if key = key' then v
        else if key > key' then lookUp(key, R)
        else lookUp(key, L);

fun assoc (nil, dict) = nil
  | assoc ((h::t), dict) =
        (h, lookUp(h, dict))::assoc(t, dict)
        handle NotFound => (print "Undefined key. \n"; nil);

(* Q9.2 *)
exception DuplicateEntry;

fun enter (key, v, dict) = 
    case dict of 
        Empty => Node((key, v), Empty, Empty)
        | Node((key', v'), L, R) =>
            if key = key' then raise DuplicateEntry
            else if key > key' then 
                Node((key', v'), L, enter (key, v, R))
            else Node((key', v'), enter (key, v, L), R);

(* 筆者の解答:
   fun enter (key,v,dict) =
       case dict of
            Empty => Node((key,v),Empty,Empty)
          | Node((key',v'),L,R) =>
              if key = key' then raise DuplicateEntry
              else if key > key' then
                   Node((key',v'),L, enter (key,v,R))
              else Node((key',v'),enter (key,v,L),R)
完全一致
*)

(* キーが見つかったら、その値とともに例外を発生させる。
可読性も処理効率も向上する。
*)
fun lookAll key dictList = 
    let exception Found of 'a
        fun lookUp key Empty = ()
          | lookUp key (Node((key', v),L,R)) = 
                if key = key' then raise Found v
                else if key > key' then lookUp key R
                else lookUp key L
    in (map(lookUp key) dictList; raise NotFound)
        handle Found v => v
    end;

fun makeMemoFun f =
    let exception NotThere
        val memo = ref (fn x => (raise NotThere))
    in fn x => !memo x
        handle NotThere =>
            let val v = f x
                val oldMemo = !memo
            in (memo := (fn y => if x = y then v
                                 else oldMemo y);
                v)
            end
    end;

local
    exception NotThere
    fun f memo 0 = 0
      | f memo 1 = 1
      | f memo n = !memo n
            handle NotThere =>
                let val v = f memo (n - 1) + f memo (n - 2)
                    val oldMemo = !memo
                in (memo := (fn y => if n = y then v
                                     else oldMemo y);
                    v)
                end
in val fastFib = f (ref (fn x => raise NotThere))
end;

(* Q9.4 *)
fun fib n = if n < 2 then n else fib (n - 1) + fib (n - 2);
fun slowFib n = makeMemoFun fib n;

fun G(n,fk,fk1) = if n = 0 then fk
                  else if n = 1 then fk1
                  else G(n-1,fk1,fk+fk1);
fun soFastFib n = G(n,0,1);

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

time (fn () => fib 43);
(* val it = (11741,11741993,11741993000,433494437)
  : IntInf.int * IntInf.int * IntInf.int * int *)
time (fn () => fastFib 43);
(* val it = (0,0,0,433494437) : IntInf.int * IntInf.int * IntInf.int * int *)
time (fn () => soFastFib 43);
(* val it = (0,0,0,433494437) : IntInf.int * IntInf.int * IntInf.int * int *)
time (fn () => veryFastFib 43);
(* val it = (0,0,0,433494437) : IntInf.int * IntInf.int * IntInf.int * int *)
