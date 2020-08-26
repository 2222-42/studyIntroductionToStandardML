(* SML source file. Copyright (c) by 2222-42 2020.
* Chap14.1 Q14.1 Q14.2
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

fun eval {prog, input, size, base} =
  let
    val tm = timeRun prog input
    val (n: int) = size input
    val ratio = Real.fromInt tm / base n
  in
    (n, tm div 1000, ratio)
  end;

(* Case1 *)
fun checkTimePerCompare1 n = 
  let 
    (* n入れて、arrayにして、またnにしているから無駄がある。 *)
    fun compSub n = 
      let 
        val array = genArray n
        val p = Array.sub(array, 0)
        val m = n div 2
        fun compare x = if x <= m then ()
                        else (Int.compare(Array.sub(array, n - x), p);
                              Int.compare(Array.sub(array, x - 1), p);
                              compare(x-1)
                              )
      in
        compare n
      end
  in
    eval {prog=compSub, input=n, size= fn x => x, base=real}
  end

(* Case2 *)
fun compareElementsOfArray array = 
    let val n = Array.length(array)
        val p = Array.sub(array, 0)
        val m = n div 2
        fun compare x = if x <= m then ()
                        else (Int.compare(Array.sub(array, n - x), p);
                              Int.compare(Array.sub(array, x - 1), p);
                              compare(x-1)
                              )
    in
    compare n
    end

fun checkTimePerCompare2 n = 
  let
    val array = genArray n
  in
    eval {prog=compareElementsOfArray, input=array, size=Array.length, base=real}
  end;

val test_list = [500000,1000000,5000000];

(* Case3 *)
fun compareElementsFrom i = 
    let val array = genArray i
        val n = Array.length(array)
        val p = Array.sub(array, 0)
        val m = n div 2
        fun compare x = if x <= m then ()
                        else (Int.compare(Array.sub(array, n - x), p);
                              Int.compare(Array.sub(array, x - 1), p);
                              compare(x-1)
                              )
    in
        compare n
    end

fun checkTimePerCompare3 n = 
  eval {prog=compareElementsFrom, input=n, size=fn x => x, base=real}

(* Case4 *)
fun compareElementsFromOther i = 
    let val array = genArray i
        val n = i
        val p = Array.sub(array, 0)
        val m = n div 2
        fun compare x = if x <= m then ()
                        else (Int.compare(Array.sub(array, n - x), p);
                              Int.compare(Array.sub(array, x - 1), p);
                              compare(x-1)
                              )
    in
        compare n
    end;

(* Case4 *)
fun compareElementsFromOther2 i = 
    let val array = genArray i
        val p = Array.sub(array, 0)
        val m = i div 2
        fun compare x = if x <= m then ()
                        else (Int.compare(Array.sub(array, i - x), p);
                              Int.compare(Array.sub(array, x - 1), p);
                              compare(x-1)
                              )
    in
        compare i
    end;

val test_list = [500000,1000000,5000000];
map checkTimePerCompare1 test_list;
map checkTimePerCompare2 test_list;
map checkTimePerCompare3 test_list;

    (* val array1 = genArray 5000000
    val tm1 = timeRun compareElementsFrom array1
    val (n1: int) = size 5000000
    val ratio1 = Real.fromInt tm1 / real n1 *)

    val array2 = genArray 5000000
    val tm2 = timeRun compareElementsOfArray array2
    (* val (n2: int) = Array.length(array2) 
    val ratio2 = Real.fromInt tm2 / real n2; *)

    val tm3 = timeRun compareElementsFrom 5000000
    (* val (n3: int) = 5000000
    val ratio3 = Real.fromInt tm3 / real n3; *)

    val tm4 = timeRun compareElementsFromOther 5000000

    val tm5 = timeRun compareElementsFromOther2 5000000