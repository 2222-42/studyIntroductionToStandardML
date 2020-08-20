(* SML source file. Copyright (c) by 2222-42 2020.
* Chap14.1 Q14.1
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
