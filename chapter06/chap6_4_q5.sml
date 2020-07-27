(* SML source file. Copyright (c) by 2222-42 2020.
* Q6.5
*)

(* パターンマッチングではないが何を意味するのか全くわからなくなってしまった *)

(* 末尾再帰な定義が思いつかんかった *)
infixr 5 Conc;

fun op Conc (xs, ys) = if null xs then ys 
                    else 
                        let 
                            val h = hd xs;
                            val tl = tl xs;
                        in
                            h :: (tl Conc ys)
                        end;
[1,2] Conc [3,4];

fun myRev xs = if null xs then []
                else 
                    let 
                        val h = hd xs
                        val tail = tl xs
                    in
                        (myRev tail) Conc [h]
                    end;

myRev [1,2,3,4];

fun myMap f xs = 
            if null xs then [] else (f (hd xs)) :: (myMap f (tl xs));

myMap (fn x => x + 1)[1,2];

(* 筆者の解答を読んでからの感想:
let ... in ... endを使う必要はなかったな
*)
