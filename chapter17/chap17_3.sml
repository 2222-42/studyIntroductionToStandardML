(* SML source file. Copyright (c) by 2222-42 2020.
* Chap17.3
*)

(* 
signature SML_OF_NJ =
  sig
  structure Cont :
    sig
      type 'a cont
      val callcc : ('a cont -> 'a) -> 'a
      val throw : 'a cont -> 'a -> 'b
      val isolate : ('a -> unit) -> 'a cont
      type 'a control_cont
      val capture : ('a control_cont -> 'a) -> 'a
      val escape : 'a control_cont -> 'a -> 'b
    end
  structure IntervalTimer :
    sig
      val minInterval : unit -> Time.time
      val setIntTimer : Time.time option -> unit
    end
  structure Internals :
    sig
      structure CleanUp : <sig>
      structure ProfControl : <sig>
      structure GC : <sig>
      val prHook : (string -> unit) ref
      val initSigTbl : unit -> unit
      val clearSigTbl : unit -> unit
      val resetSigTbl : unit -> unit
      val resetTimers : unit -> unit
      structure TDP : <sig>
    end
  structure SysInfo :
    sig
      exception UNKNOWN
      datatype os_kind = UNIX | WIN32
      val getOSKind : unit -> os_kind
      val getOSName : unit -> string
      val getOSVersion : unit -> string
      val getArchName : unit -> string
      val getArchSize : unit -> int
      val getHostSize : unit -> int
      val getHostArch : unit -> string
      val getTargetArch : unit -> string
      val hasSoftwarePolling : unit -> bool
      val hasMultiprocessing : unit -> bool
      val getHeapSuffix : unit -> string
    end
  structure Weak :
    sig
      type 'a weak
      val weak : 'a -> 'a weak
      val strong : 'a weak -> 'a option
      type weak'
      val weak' : 'a -> weak'
      val strong' : weak' -> bool
    end
  structure Susp :
    sig
      type 'a susp
      val delay : (unit -> 'a) -> 'a susp
      val force : 'a susp -> 'a
    end
  val exportML : string -> bool
  val exportFn : string * (string * string list -> OS.Process.status) -> unit
  val getCmdName : unit -> string
  val getArgs : unit -> string list
  val getAllArgs : unit -> string list
  val shiftArgs : unit -> unit
  datatype 'a frag = ANTIQUOTE of 'a | QUOTE of string
  val exnHistory : exn -> string list
end *)

fun check (x, argList) = 
  (print (x^"\n");
   map (fn x => print(x^"\n")) argList;
   OS.Process.success);

(* SMLofNJ.exportFn ("check", check); *)
