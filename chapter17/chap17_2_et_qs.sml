(* SML source file. Copyright (c) by 2222-42 2020.
* Chap17.2 Q17.3 Q17.4
* This file is able to be runned on UNIX machine.
*)

(* signature UNIX =
  sig
  type ('a,'b) proc
  type signal
  datatype exit_status
    = W_EXITED
  | W_EXITSTATUS of Word8.word
  | W_SIGNALED of signal
  | W_STOPPED of signal
  val fromStatus : OS.Process.status -> exit_status
  val executeInEnv : string * string list * string list -> ('a,'b) proc
  val execute : string * string list -> ('a,'b) proc
  val textInstreamOf : (TextIO.instream,'a) proc -> TextIO.instream
  val binInstreamOf : (BinIO.instream,'a) proc -> BinIO.instream
  val textOutstreamOf : ('a,TextIO.outstream) proc -> TextIO.outstream
  val binOutstreamOf : ('a,BinIO.outstream) proc -> BinIO.outstream
  val streamsOf :
      (TextIO.instream,TextIO.outstream) proc
       -> TextIO.instream * TextIO.outstream
  val reap : ('a,'b) proc -> OS.Process.status
  val kill : ('a,'b) proc * signal -> unit
  val exit : Word8.word -> 'a
end *)

(* signature TEXT_IO =
  sig
  type vector = string
  type elem = char
  type instream
  type outstream
  val input : instream -> vector
  val input1 : instream -> elem option
  val inputN : instream * int -> vector
  val inputAll : instream -> vector
  val canInput : instream * int -> int option
  val lookahead : instream -> elem option
  val closeIn : instream -> unit
  val endOfStream : instream -> bool
  val output : outstream * vector -> unit
  val output1 : outstream * elem -> unit
  val flushOut : outstream -> unit
  val closeOut : outstream -> unit
  structure StreamIO :
    sig
      type vector = string
      type elem = char
      type reader
      type writer
      type instream
      type outstream
      type pos
      type out_pos
      val input : instream -> vector * instream
      val input1 : instream -> (elem * instream) option
      val inputN : instream * int -> vector * instream
      val inputAll : instream -> vector * instream
      val canInput : instream * int -> int option
      val closeIn : instream -> unit
      val endOfStream : instream -> bool
      val mkInstream : reader * vector -> instream
      val getReader : instream -> reader * vector
      val filePosIn : instream -> pos
      val output : outstream * vector -> unit
      val output1 : outstream * elem -> unit
      val flushOut : outstream -> unit
      val closeOut : outstream -> unit
      val setBufferMode : outstream * IO.buffer_mode -> unit
      val getBufferMode : outstream -> IO.buffer_mode
      val mkOutstream : writer * IO.buffer_mode -> outstream
      val getWriter : outstream -> writer * IO.buffer_mode
      val getPosOut : outstream -> out_pos
      val setPosOut : out_pos -> unit
      val filePosOut : out_pos -> pos
      val inputLine : instream -> (string * instream) option
      val outputSubstr : outstream * substring -> unit
    end
  val mkInstream : StreamIO.instream -> instream
  val getInstream : instream -> StreamIO.instream
  val setInstream : instream * StreamIO.instream -> unit
  val getPosOut : outstream -> StreamIO.out_pos
  val setPosOut : outstream * StreamIO.out_pos -> unit
  val mkOutstream : StreamIO.outstream -> outstream
  val getOutstream : outstream -> StreamIO.outstream
  val setOutstream : outstream * StreamIO.outstream -> unit
  val inputLine : instream -> string option
  val outputSubstr : outstream * substring -> unit
  val openIn : string -> instream
  val openString : string -> instream
  val openOut : string -> outstream
  val openAppend : string -> outstream
  val stdIn : instream
  val stdOut : outstream
  val stdErr : outstream
  val print : string -> unit
  val scanStream :
      ((elem,StreamIO.instream) StringCvt.reader
        -> ('a,StreamIO.instream) StringCvt.reader)
       -> instream -> 'a option
end *)

fun splice (nil,_) = ""
  | splice ([x],_) = x
  | splice ((h::t),s) = h ^ s ^ splice (t,s);

fun execCommand c args = 
  Unix.streamsOf (Unix.execute("/bin/bash", "-c"::[splice(c::args, "/")]))

fun useCommand c args ins outs = 
  let
    val p = (Unix.execute("/bin/bash", "-c"::[splice(c::args, "/")]) : (TextIO.instream, TextIO.outstream) Unix.proc)
        (* - Unix.execute;
        val it = fn : string * string list -> ('a,'b) Unix.proc *)
        (* - Unix.streamsOf;
        val it = fn
          : (TextIO.instream,TextIO.outstream) Unix.proc
            -> TextIO.instream * TextIO.outstream *)
    val (ins',outs') = Unix.streamsOf p
    val endOfIns = ref false
    fun send () = 
      if TextIO.endOfStream ins
        then (endOfIns := true; TextIO.closeOut outs')
      else (TextIO.output(outs', TextIO.inputN(ins, 1));
            TextIO.flushOut outs')
    (* このreceiveが機能していない説がある。 *)
    fun receive() = 
      case TextIO.canInput(ins', 1) of
         SOME 1 => (TextIO.output(outs, TextIO.inputN(ins', 1));
                    TextIO.flushOut outs)
       | _ => ()
    fun receiveRest () = 
      if TextIO.endOfStream ins' then ()
      else (TextIO.output(outs, TextIO.inputN(ins', 1));
            receiveRest())
    fun loop () = (if !endOfIns then receiveRest() else (send(); receive(); loop()))
  in
    (loop(); Unix.reap p;())
  end
  (* val c = "./test.sh";
  val args = [];
  val ins = TextIO.openIn "testInts.txt"; 
  val outs = TextIO.openOut "outs.txt";
   *)

(* Q17.3 *)
fun processFile inf com outf = 
  let
    val ins = TextIO.openIn inf
    val outs = TextIO.openOut outf
  in
    (useCommand com [] ins outs; TextIO.closeIn ins; TextIO.closeOut outs)
  end
  (* processFile "testInts.txt" "./test.sh" "out.txt"; *)

(* Q17.4 *)
(* fun pipe cmd1 cmd2 ins outs = 
  (useCommand cmd1 [] ins outs; useCommand cmd2 [] ins outs) *)

fun pipeModified cmd1 cmd2 ins outs = 
  let
    val p1 = (Unix.execute("/bin/bash", "-c"::[splice(cmd1::[], "/")]) : (TextIO.instream, TextIO.outstream) Unix.proc)
    val p2 = (Unix.execute("/bin/bash", "-c"::[splice(cmd2::[], "/")]) : (TextIO.instream, TextIO.outstream) Unix.proc)
    val (ins1,outs1) = Unix.streamsOf p1
    val (ins2,outs2) = Unix.streamsOf p2
    val endOfIns = ref false

    fun send1 () = 
      if TextIO.endOfStream ins
        then (endOfIns := true; TextIO.closeOut outs1)
      else (TextIO.output(outs1, TextIO.inputN(ins, 1));
            TextIO.flushOut outs1)
    fun send2 () = 
      if TextIO.endOfStream ins1
        then (endOfIns := true; TextIO.closeOut outs2)
      else (TextIO.output(outs2, TextIO.inputN(ins1, 1));
            TextIO.flushOut outs2)

    fun receive1() = 
      case TextIO.canInput(ins1, 1) of
         SOME 1 => (TextIO.output(outs2, TextIO.inputN(ins1, 1));
                    print "present\n";
                    TextIO.flushOut outs2)
       | _ => (print "not present\n";())     
    fun receive2() = 
      case TextIO.canInput(ins2, 1) of
         SOME 1 => (TextIO.output(outs, TextIO.inputN(ins2, 1));
                    TextIO.flushOut outs)
       | _ => ()


    fun receiveSub () = 
      if TextIO.endOfStream ins1 then ()
      else (TextIO.output(outs2, TextIO.inputN(ins1, 1));
            receiveSub())
    fun receiveRest () = 
      if TextIO.endOfStream ins2 then ()
      else (TextIO.output(outs, TextIO.inputN(ins2, 1));
            receiveRest())
    fun loop () = (if !endOfIns then (receiveSub();receiveRest()) else (send1(); receive1(); send2(); receive2(); loop()))
  in
    (loop(); Unix.reap p1; Unix.reap p2;())
  end
fun pipeFileModified cmd1 cmd2 inf outf = 
  let
    val ins = TextIO.openIn inf
    val outs = TextIO.openOut outf
  in
    pipeModified cmd1 cmd2 ins outs
  end
(* pipeFileModified "./test.sh" "./test2.sh" "testInts.txt" "out2.txt";
val cmd1 = "./test.sh" 
val cmd2 = "./test2.sh"
val inf = "testInts.txt" 
val outf = "out2.txt";
splice([splice(cmd1::[], "/"),splice(cmd2::[], "/")], " | ")
 *)

fun pipe cmd1 cmd2 ins outs = 
  let
    val p = (Unix.execute("/bin/bash", "-c"::[splice([splice(cmd1::[], "/"),splice(cmd2::[], "/")], " | ")]) : (TextIO.instream, TextIO.outstream) Unix.proc)
    val (ins',outs') = Unix.streamsOf p
    val endOfIns = ref false
    fun send () = 
      if TextIO.endOfStream ins
        then (endOfIns := true; TextIO.closeOut outs')
      else (TextIO.output(outs', TextIO.inputN(ins, 1));
            TextIO.flushOut outs')
    fun receive() = 
      case TextIO.canInput(ins', 1) of
         SOME 1 => (TextIO.output(outs, TextIO.inputN(ins', 1));
                    TextIO.flushOut outs)
       | _ => ()
    fun receiveRest () = 
      if TextIO.endOfStream ins' then ()
      else (TextIO.output(outs, TextIO.inputN(ins', 1));
            receiveRest())
    fun loop () = (if !endOfIns then receiveRest() else (send(); receive(); loop()))
  in
    (loop(); Unix.reap p;())
  end

fun pipeFile cmd1 cmd2 inf outf = 
  let
    val ins = TextIO.openIn inf
    val outs = TextIO.openOut outf
  in
    pipe cmd1 cmd2 ins outs
  end
(* pipeFileModified "./test.sh" "./test2.sh" "testInts.txt" "out2.txt";
val cmd1 = "./test.sh" 
val cmd2 = "./test2.sh"
val inf = "testInts.txt" 
val outf = "out2.txt";
splice([splice(cmd1::[], "/"),splice(cmd2::[], "/")], " | ")
 *)
