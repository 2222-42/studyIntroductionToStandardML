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

(* 問7.15 *)

fun Cord n = CONS(FROMN 0, fn () => Cord (n+1))
val TestCord = Cord 0;

(* 1 *)
fun pointByReader (n, m) listOfList = 
    let
        val slice1 = DROP n listOfList
        val head = HD slice1
        val tail = DROP m head
    in
        HD tail
    end;

pointByReader (100,50) TestCord;

(* 筆者の解答 *)

fun point(x,y) L = NTH y (NTH x L);
(* わざわざドロップさせなくてもN番目の要素取るだけだから *)

(* 2 *)
fun graph (f:int * int -> int) =
    let
        fun UpSecond (n, m) = CONS(f(n, m), fn () => UpSecond (n, m + 1))
        fun SubGraph n = CONS(UpSecond(n, 0),fn () => SubGraph (n + 1))
    in
        SubGraph 0
    end;


fun testFunc (x, y) = x + y;
point (10, 15) (graph testFunc);
(* expected: 25 *)

(* 筆者の解答 *)
fun graphByAuthor f  =
    let fun fromx x =
            let fun fromy y  = CONS(f(x,y),fn () => fromy (y + 1))
            in CONS(fromy 0,fn () => fromx (x + 1))
            end
    in fromx 0
    end;

(* 3 *)
(* 
'a inflist inflist -> 'a inflist
*)

fun enumerateByReader infListOfList = 
    let
        fun getNext (n,m) = 
            if n = 0 then (m + 1, 0)
            else (n - 1, m + 1)
        fun SubGraph (n, m) = CONS(point (n,m) infListOfList, fn () => SubGraph (getNext(n,m)))
    in
        SubGraph (0,0)
    end;

val sortedTestCord = enumerateByReader TestCord;
VIEW (6,10) sortedTestCord;

(* 筆者の解答 *)
fun enumerateByAuthor f =
    let fun next (0,a) = (a+1,0)
          | next (a,b) = (a - 1, b + 1)
        fun from a = CONS(f a, fn () => from (next a))
    in from (0,0)
    end;

(* val enumerateByAuthor = fn : (int * int -> 'a) -> 'a inflist *)
(* 望んでいるのは、fn : 'a inflist inflist -> 'a inflistなので違う *)

fun enumerate IL =
    let fun next (0,a) = (a+1,0)
          | next (a,b) = (a - 1, b + 1)
        fun from a = CONS(point a IL, fn () => from (next a))
    in from (0,0)
    end;

val sortedTestCord = enumerate TestCord;
VIEW (6,10) sortedTestCord;