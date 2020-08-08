(* SML source file. Copyright (c) by 2222-42 2020.
* Q9.3
*)

fun multipleMulti list = 
    let exception Zero
        fun multi L = foldr (fn (h, R) => if h = 0 then raise Zero
                                        else h * R
                          ) 1 L;
    in
        multi list handle Zero => 0
    end;

val testList1 = [2,3,4,5];
val testList2 = [2,3,0,5];

multipleMulti testList1;
multipleMulti testList2;

(* 筆者の解答 *)
   fun prodList L =
     let
       exception Zero
       fun f nil r = r
         | f (0::t) r = raise Zero
         | f (h::t) r = f t (h * r)
     in
       f L 1
       handle Zero => 0
     end;

(* 回答者のコメント:
これの方が、よりMLらしさのある感じがした。
*)
prodList testList1;
prodList testList2;
