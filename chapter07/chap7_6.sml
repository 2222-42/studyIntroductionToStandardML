(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 7.6
*)

(* 
無限リストを扱いたい

無限リストを作りたい
-> 無限にリストを生成して、終了しないプログラムになってしまう
-> リストの2番目以降の部分の評価を、必要となるまで遅延する
    fn () => exp : unit -> t
評価が遅延されたt型データとみなせる
*)

datatype 'a inflist = NIL | CONS of 'a * (unit -> 'a inflist);

fun FROMN n = CONS(n, fn () => FROMN (n+1))

fun HD (CONS(a,b)) = a;
fun TL (CONS(a,b)) = b();
fun NULL NIL = true | NULL _ = false;

val naturalNumbers = FROMN 0;
HD naturalNumbers;
TL naturalNumbers;
HD (TL(TL(TL it)));

fun NTH 0 L = HD L
    | NTH n L = NTH (n -1) (TL L);

(* 通常の定義:
fun filter f l = if null l then nil 
                 else if f (hd l) then
                    hd l :: (filter f (tl l))
                 else filter f (tl l); *)

fun FILTER f l = if NULL l then NIL 
                 else if f (HD l) then
                    CONS (HD l, fn () => (FILTER f (TL l)))
                 else FILTER f (TL l);

(* 問7.13 *)

val evenNumbers = FILTER (fn x => x mod 2 = 0) naturalNumbers;

(* 問7.14 *)

fun DROP 0 l = l 
    | DROP n l = DROP (n - 1) (TL l);

fun TAKE 0 l = []
    | TAKE n l = [HD l] @ TAKE (n-1) (TL l);

fun VIEW (n, m) l = TAKE m (DROP n l);

(* 素数の無限リストを作ろう *)

fun SIFT NIL = NIL
    | SIFT L = 
        let val a = HD L
        in CONS(a, fn () =>
            SIFT (FILTER (fn x => x mod a <> 0) (TL L)))
        end;

val PRIMES = SIFT (FROMN 2);
VIEW (100,10) PRIMES;