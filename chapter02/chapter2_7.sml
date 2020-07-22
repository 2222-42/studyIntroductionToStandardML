(* SML source file. Copyright (c) by 2222-42 2020.
*
*)

(* 宣言文の書けない場所(例えば関数内とか)に関数の値そのものを表す(無名関数のような)式、関数式、fn式 
    fn p => exp
*)

(fn x => x + 1) 3 * 10; 

fun fib n = if n < 2 then n else fib (n - 1) + fib (n - 2);

fun naiveFibMod n m = (fib n) mod m = 0;

fun fastFibMod n = let val a = (fib n)
                   in fn m => a mod m = 0
                   end

(* for Q21 *)
val g = naiveFibMod 35;
(* (g 1, g 2, g 3, g 4, g 5); *)

val h = fastFibMod 35;
(* (h 1, h 2, h 3, h 4, h 5); *)

fun time f = let
    val realt = Timer.startRealTimer()
    val rv = f ()
    val elapsed = Timer.checkRealTimer realt
in
    (Time.toMilliseconds elapsed, Time.toMicroseconds elapsed,Time.toNanoseconds elapsed, rv)
end;

time (fn () => g 5);
time (fn () => h 5);

(* 計算の一部が引数の一部のみに依存するような関数の場合、関数式を用いて関数定義を分解し、計算を段階的に行うようにすると、効率がよくなる。 *)
(* 関数は値を記憶しておくデータ構造としても使用できる *)

fun memo f x = let val a = f x
               in fn y => if x = y then a else f y
               end
(* Warning: calling polyEqual *)

(* val fib = memo fib 31;
val fib = memo fib 32;
val fib = memo fib 33;
val fib = memo fib 34; *)
