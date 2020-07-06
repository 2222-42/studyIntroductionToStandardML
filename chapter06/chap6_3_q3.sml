(* SML source file. Copyright (c) by 2222-42 2020.
* Q 6.3
*)


fun zip (h1::t1, h2::t2) = (h1, h2)::zip(t1, t2)
    | zip _ = nil;

fun unzip ((h1, h2) :: t) =
        let val (L1, L2) = unzip t
        in (h1::L1, h2::L2)
        end
    | unzip _ = (nil,nil);

fun last [x] = x
    | last (h::t) = last t;
