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
