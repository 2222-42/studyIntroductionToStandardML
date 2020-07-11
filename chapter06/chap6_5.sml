(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 6.5
*)

(* 
一般的な再帰処理：
1. リストがnilなら特定の値Zを返す
2. リストがh::tの形なら、部分リストtに対する値Rを計算する
3. hとRに特定の演算fを適用し、得られた結果を返す
*)

(* 
リスト処理構造を実現する汎用の高階関数を一度定義しておけば、
種々のリスト処理関数を、その高階関数の簡単な適用によって実現できる
*)

fun foldr f Z nil = Z
    | foldr f Z (h::t) = f(h, foldr f Z t);

val sumList = foldr (fn (h, R) => h + R) 0;

fun length L = foldr (fn (_, R) => 1 + R) 0 L;

fun L1 @ L2 = foldr (op ::) L2 L1;

(* 関係Rの推移的閉方R+を 作る*)

fun timesRel (R, S) = 
    foldr (fn ((x,a), r) =>
            foldr (fn ((b, y), rs) => 
                if a = b then (x,y)::rs else rs)
            r S
            )
        nil R;

fun powerRel r 1 = r
    | powerRel r n = timesRel (r, powerRel r (n-1));

fun accumulate h z f n = 
    if n = 0 then z
    else h (f (n), accumulate h z f (n-1));

fun tc R = accumulate (op @) nil (powerRel R) (length R);

tc [(1,2), (2,3), (3,4)];
