(* SML source file. Copyright (c) by 2222-42 2020.
* Q16.11
*)



fun timeRun f x =
    let 
        val timer = Timer.startCPUTimer()
        val _ = f x
        val tm = Timer.checkCPUTimer timer
        val ut = Time.toMicroseconds (#usr tm)
    in LargeInt.toInt ut
    end;

local val seed = (Random.rand(0,1))
in fun randomInt n = Random.randInt seed
end;

fun genArray n = Array.tabulate(n, randomInt);

use "./chapter13/chap13_2_q4.sml";

fun convBase f r (x: real) = f x / f r;

(* Q14.2 *)
fun nlogn n = 
  (Real.fromInt n) * (convBase Math.log10 2.0 (Real.fromInt n));

fun checkTime n = 
  let 
    val array = genArray n
    val tm = timeRun ArrayQuickSort.sort (array, Int.compare)
    val nlognRatio = Real.fromInt(tm) / (nlogn n)
  in
    (n, tm div 1000, nlognRatio)
  end;

use "./chapter16/chap16_3_q9.sml";

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

fun formatReal r = (Real.fmt (StringCvt.FIX (SOME 8)) r)
fun printLine (i1, i2, r) =
  Format.printf "%20d%20d%20f\n" [Format.I i1, Format.I i2, Format.R r];

fun evalSort list = 
  let 
    val results = map checkTime list
    val average = (foldr(fn ((a,b,c),R) => c+R ) 0.0 results)/Real.fromInt(length list)
  in
    Format.printf "%20s%20s%20s\n" [Format.S "array size", Format.S "milli-sec.", Format.S "micro s./(n log(n))"];
    map printLine results;
    print("---------------------------------------------------------------\n");
    Format.printf "%20s%20s%20f\n" [Format.S " ", Format.S "average", Format.R average]
  end;

val test_list = [500000,1000000,5000000];
evalSort test_list;