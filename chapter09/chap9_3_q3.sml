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

