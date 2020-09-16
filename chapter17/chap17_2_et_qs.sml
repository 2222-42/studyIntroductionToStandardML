(* SML source file. Copyright (c) by 2222-42 2020.
* Chap17.2 Q17.3
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

fun splice (nil,_) = ""
  | splice ([x],_) = x
  | splice ((h::t),s) = h ^ s ^ splice (t,s);

fun execCommand c args = 
  Unix.streamsOf (Unix.execute("/bin/bash", "-c"::[splice(c::args, "/")]))

fun useCommand c args ins outs = 
  let
    val p = Unix.execute("/bin/bash", "-c"::[splice(c::args, "/")])
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
    fun loop () = (if !endOfIns then receiveRest else (send(); receive(); loop()))
  in
    (loop(); Unix.reap p;())
  end

(* Q17.3 *)
fun processFile inf com outf = 
  let
    val ins = TextIO.openIn inf
    val outs = TextIO.openOut outf
  in
    (useCommand com [] ins outs; TextIO.closeIn ins; TextIO.closeOut outs)
  end
