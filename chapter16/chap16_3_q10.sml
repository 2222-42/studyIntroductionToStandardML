(* SML source file. Copyright (c) by 2222-42 2020.
* Q16.10
*)

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

signature TIME_FORMAT =
sig   datatype kind = 
              HOUR24
            | HOUR12
            | HOUR
            | MINUTE
            | SEC
            | DAY
            | MONTH
            | YEAR
    datatype align = LEFT | RIGHT
    datatype format = 
                LITERAL of string
              | SPEC of {kind: kind}

    exception formatError
    val format : string -> string
    val showTime : string -> unit
end

structure ShowTime : TIME_FORMAT =
struct
  datatype align = LEFT | RIGHT
  datatype kind = 
              HOUR24
            | HOUR12
            | HOUR
            | MINUTE
            | SEC
            | DAY
            | MONTH
            | YEAR
  datatype format = 
          LITERAL of string
        | SPEC of {kind: kind}



  exception formatError
  structure SS = Substring


  fun formatData {kind} data =
  let
    fun string24Hour time = 
      StringCvt.padLeft #"0" 2 (Int.toString (Date.hour (Date.fromTimeLocal time)))
    fun string12Hour time = 
      StringCvt.padLeft #"0" 2 (Int.toString (
        let val h = (Date.hour (Date.fromTimeLocal time)) mod 12
        in
          if h = 0 then 12
          else h
        end))
    fun stringHour time = 
      Int.toString (Date.hour (Date.fromTimeLocal time))
    fun stringMinutes time = 
      (Int.toString (Date.minute (Date.fromTimeLocal time)))
    fun stringSeconds time = 
      (Int.toString (Date.second (Date.fromTimeLocal time)))
    fun stringDay time = 
      StringCvt.padLeft #"0" 2 (Int.toString (Date.day (Date.fromTimeLocal time)))
    fun stringMonth time = 
      case Date.month (Date.fromTimeLocal time) of
          Date.Apr => "04"
        | Date.Aug => "08"
        | Date.Dec => "10"
        | Date.Feb => "02"
        | Date.Jan => "01"
        | Date.Jul => "07"
        | Date.Jun => "06"
        | Date.Mar => "03"
        | Date.May => "05"
        | Date.Nov => "11"
        | Date.Oct => "10"
        | Date.Sep => "09"
    fun stringYear time = 
      StringCvt.padLeft #"0" 4 (Int.toString (Date.year (Date.fromTimeLocal time)))
  in
    case (kind, data) of
          (HOUR24, data) => string24Hour data
        | (HOUR12, data) => string12Hour data
        | (HOUR, data) => stringHour data
        | (MINUTE, data) => stringMinutes data
        | (SEC, data) => stringSeconds data
        | (DAY, data) => stringDay data
        | (MONTH, data) => stringMonth data
        | (YEAR, data) => stringYear data
  end

  fun oneFormat s =
    let
      val s = SS.triml 1 s
    in
      if SS.isPrefix "%" s then (LITERAL "%", SS.triml 1 s)
      else
        let
          val (c, s) = case SS.getc s of NONE => raise formatError
                                      | SOME s => s
        in
          (SPEC{kind=case c of
              #"H" => HOUR24
            | #"I" => HOUR12
            | #"k" => HOUR
            | #"M" => MINUTE
            | #"S" => SEC
            | #"d" => DAY
            | #"m" => MONTH
            | #"Y" => YEAR
            | _ => raise formatError
          }, s)
        end
    end

  fun parse s =
    let 
      val (s1, s) = StringCvt.splitl (fn c => c <> #"%") SS.getc s
      val prefix = if s1 = "" then nil else [LITERAL s1]
    in
      if SS.isEmpty s then prefix
      else let
            val (f, s) = oneFormat s
            val L = parse s
          in
            prefix@(f::L)
          end
      
    end
  fun format s= 
    let
        val time = Time.now()
        val FL = parse (SS.full s)
        
        fun traverse (h::t) = 
        (case h of
            LITERAL s => s ^ traverse t
          | SPEC fmt => (formatData fmt time
                          ^ (traverse t )))
          | traverse nil = ""
    in
        (traverse FL)
    end

  fun showTime s = print (format s)
end;


(* Q16.10 *)

(* ShowTime.showTime "The time is %H hour %M minutes on %m/%d/%Y.\n"; *)
