(* SML source file. Copyright (c) by 2222-42 2020.
* Chap14.1 Q14.1 Q14.2
*)

 Time.toString(Time.now());

(* 
signature DATE =
  sig
    datatype weekday = Fri | Mon | Sat | Sun | Thu | Tue | Wed
    datatype month
      = Apr | Aug | Dec | Feb | Jan | Jul | Jun | Mar | May | Nov | Oct | Sep
    type date
    exception Date
    val year : date -> int
    val month : date -> month
    val day : date -> int
    val hour : date -> int
    val minute : date -> int
    val second : date -> int
    val weekDay : date -> weekday
    val yearDay : date -> int
    val isDst : date -> bool option
    val offset : date -> Time.time option
    val localOffset : unit -> Time.time
    val
        date : {day:int, hour:int, minute:int, month:month,
                 offset:Time.time option, second:int, year:int}
                -> date
    val fromTimeLocal : Time.time -> date
    val fromTimeUniv : Time.time -> date
    val toTime : date -> Time.time
    val toString : date -> string
    val fmt : string -> date -> string
    val fromString : string -> date option
    val scan : (char,'a) StringCvt.reader -> (date,'a) StringCvt.reader
    val compare : date * date -> order
  end
*)

fun currentTime() = 
    Date.fmt "%Y-%m-%d %H:%M:%S" (Date.fromTimeLocal(Time.now()));
(*   DATE
    {day=21,hour=8,isDst=SOME false,minute=29,month=Aug,
     offset=SOME (TIME {usec=578254988000000}),second=42,wday=Fri,yday=234,
     year=2020} : Date.date *)
currentTime(); 
(* end Q14.1 *)

(* signature TIMER =
  sig
    type cpu_timer
    type real_timer
    val startCPUTimer : unit -> cpu_timer
    val totalCPUTimer : unit -> cpu_timer
    val
        checkCPUTimes : cpu_timer
                         -> {gc:{sys:Time.time, usr:Time.time},
                          nongc:{sys:Time.time, usr:Time.time}}
    val checkCPUTimer : cpu_timer -> {sys:Time.time, usr:Time.time}
    val checkGCTime : cpu_timer -> Time.time
    val startRealTimer : unit -> real_timer
    val totalRealTimer : unit -> real_timer
    val checkRealTimer : real_timer -> Time.time
  end
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
  end
(* chapter14\chap14_et_q1.sml:94.9-94.36 Error: operator and operand do not agree [tycon mismatch]
  operator domain: real * real
  operand:         int * real
  in expression:
    tm / nlogn n *)
(* val checkTime = fn : int -> int * int * real *)
