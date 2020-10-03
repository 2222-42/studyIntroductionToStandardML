(* SML source file. Copyright (c) by 2222-42 2020.
* Q 6.3
*)


fun zip (h1::t1, h2::t2) = (h1, h2)::zip(t1, t2)
    | zip _ = nil;

(* 筆者の解答(順番が違うだけで、本質にかかわる、もしくは結果に関わるような差異はないかと思う) *)
fun zipByAuthor (nil,_) = nil
  | zipByAuthor (_,nil) = nil
  | zipByAuthor (h1::t1, h2::t2) = (h1,h2) :: zipByAuthor (t1, t2)

fun unzip ((h1, h2) :: t) =
        let val (L1, L2) = unzip t
        in (h1::L1, h2::L2)
        end
    | unzip _ = (nil,nil);

(* 筆者の解答 *)
fun unzip nil = (nil,nil)
  | unzip ((a,b)::t) = let val (A,B) = unzip t in (a::A, b::B) end

fun last [x] = x
    | last (h::t) = last t;

(* 筆者の解答(不要なものは_として無視している) *)
fun last [a] = a
  | last (_::t) = last t
