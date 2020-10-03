(* SML source file. Copyright (c) by 2222-42 2020.
* https://gist.githubusercontent.com/hebiyan/7bb55c0902e799d11462/raw/44bc3c25e7c785f079714aa4f5fc03eabf8f2f4b/elapsed_time.md
* 
*)

fun time f = let
    val realt = Timer.startRealTimer()
    val rv = f ()
    val elapsed = Timer.checkRealTimer realt
in
    (Time.toMilliseconds elapsed, Time.toMicroseconds elapsed,Time.toNanoseconds elapsed, rv)
end;

fun timeRun f x =
    let 
        val timer = Timer.startCPUTimer()
        val _ = f x
        val tm = Timer.checkCPUTimer timer
        val ut = Time.toMicroseconds (#usr tm)
    in LargeInt.toInt ut
    end

fun convBase f r (x: real) = f x / f r;

fun nlogn n = 
  (Real.fromInt n) * (convBase Math.log10 2.0 (Real.fromInt n));

fun padString str = 
  if String.size(str) < 20 then 
    let
      val length = 20 - String.size(str)
      fun addEmp (length, string) = 
        if length <= 0 then string
        else addEmp(length - 1, " "^string)
    in
      addEmp(length, str)
    end
  else str;

fun printLine (i1, i2, r) =
  print(padString(Int.toString(i1))^padString(Int.toString(i2))^padString(Real.toString(r))^"\n");


fun eval {prog, input, size, base} =
  let
    val tm = timeRun prog input
    val (n: int) = size input
    val ratio = Real.fromInt tm / base n
  in
    (n, tm div 1000, ratio)
  end;