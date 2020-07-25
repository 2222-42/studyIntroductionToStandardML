(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 4.6 and Q 4.6, 4.7
*)

(* 
- exception Substring
- size
- substring
- explode
- implode
- concat
- >
- >=
- <
- <=
- ^
- print
*)

"Standard ML";
substring(it,9,2);
explode it;
map (fn x => ord x + 1) it;
map chr it;
implode it;
it < "ML";

(* 回答者のnextStartPosはすごく混乱して書きあげられたものと考えられる。 *)
fun match s1 s2 = 
    let
        val maxIndex1 = size s1 - 1
        val maxIndex2 = size s2 - 1
        fun nextStartPos (i,j) = if j = maxIndex2 then (i+1,0) else (i,j+1)
        (* fun nextStartPos (i, j) =
            if j + 1 <= maxIndex2 then (i, j + 1)
            else if i + 1 <= maxIndex1 then (i + 1, 0)
            else (size s1, size s2) *)
        
        fun findMatch (from1, from2) (start1, start2, max) = 
            if from1 > maxIndex1 orelse from2 > maxIndex2 then
                (start1, start2, max) 
            else 
                let 
                    fun advance n =
                        if from1 + n > maxIndex1 orelse from2 + n > maxIndex2 then n
                        else if substring(s1,from1 + n,1) = substring(s2,from2 + n,1) then advance (n + 1)
                        else n
                    val newSize = advance 0
                in 
                    if max < newSize then
                        findMatch (nextStartPos (from1, from2)) (from1, from2, newSize)
                    else
                        findMatch (nextStartPos (from1, from2)) (start1, start2, max)
                end
    in findMatch (0,0) (0,0,0)
    end;

match "sssssss" "sssss";
match "sssseee" "ssee";
match "sssse" "ssee";
match "sssseessse" "ssee";
match "ssssesssee" "ssee";
match "ssee" "ssee";
match "ssee" "aasseepp";
(* 
val it = (0,0,5) : int * int * int
val it = (2,0,4) : int * int * int
val it = (2,0,3) : int * int * int
val it = (2,0,4) : int * int * int
val it = (6,0,4) : int * int * int
val it = (0,0,4) : int * int * int
val it = (0,2,4) : int * int * int
*)